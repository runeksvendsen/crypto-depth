{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}
module CryptoDepth.RateMap

where

import CryptoDepth.Internal.DPrelude hiding (head)
import CryptoDepth.Internal.Util
import CryptoDepth.Types
import CryptoDepth.BuildGraph

import qualified OrderBook.Types as OB

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Control.Category as Cat

import qualified Data.Graph.Inductive.Query.BFS as G
import Data.List (init)
import qualified Data.HashMap.Strict as Map
import qualified Money


type RateGraph = G.Gr Sym Money.SomeExchangeRate
type RateMap numeraire = Map.HashMap Sym (RateFrom numeraire)

-- | An exchange rate from 'src' (universally quantified) to 'dst' (existentially quantified)
data RateFrom src = forall dst. KnownSymbol dst => RateFrom (Money.ExchangeRate src dst)

-- | The destination currency of a 'RateFrom'
rateFromDst :: KnownSymbol src => RateFrom src -> Text
rateFromDst (RateFrom er) =
    Money.someExchangeRateDstCurrency $ Money.toSomeExchangeRate er

instance KnownSymbol dst => Show (RateFrom dst) where
    show (RateFrom rt) = show rt

-- |
toRate
    :: forall numeraire.
       KnownSymbol numeraire
       -- | E.g.: [ ExchangeRate "USD" "BTC", ExchangeRate "BTC" "DOGE" ]
    => [Money.SomeExchangeRate]
       -- | E.g.: (RateFrom (ExchangeRate "USD" "DOGE") :: RateFrom "USD")
    -> Either String (RateFrom numeraire)
toRate =
    foldl' foldRate (Right (RateFrom Cat.id :: RateFrom numeraire))
  where
    foldRate :: Either String (RateFrom numeraire)
             -> Money.SomeExchangeRate
             -> Either String (RateFrom numeraire)
    foldRate (Right (RateFrom erFrom)) someEr =
        Money.withSomeExchangeRate someEr (tryCompose erFrom)
    foldRate left _ = left
    tryCompose :: forall src1 src2 dst1 dst2.
                  (KnownSymbol src1, KnownSymbol dst1, KnownSymbol src2, KnownSymbol dst2)
               => Money.ExchangeRate src1 dst1
               -> Money.ExchangeRate src2 dst2
               -> Either String (RateFrom src1)
    tryCompose er1 er2 =
        case sameSymbol (Proxy :: Proxy dst1) (Proxy :: Proxy src2) of
            Just Refl -> Right . RateFrom $ er2 Cat.. er1
            Nothing   -> Left $ "Incompatible from/to rates: " ++ show (er1,er2)

buildRateMap :: KnownSymbol numeraire => [ABook] -> RateMap numeraire
buildRateMap books =
    toRateMap (graph :: RateGraph) nodeMap
  where
    (graph, nodeMap) = buildGraph toRateEdges books

toRateMap
    :: forall gr numeraire.
       (G.Graph gr, Show (gr Sym (RateFrom numeraire)), KnownSymbol numeraire)
    => gr Sym Money.SomeExchangeRate
    -> NodeMap
    -> RateMap numeraire
toRateMap g nodeMap =
      foldr (insertRate . toKeyVal) Map.empty
    $ map (either error id)             -- A "Left" value indicates a bug, so we throw an error
    $ map (toRate . map snd)
    $ allPaths (symNode numeraire) g    -- Find all paths with startNode=numeraire
  where
    toKeyVal rateFrom = (rateFromDst rateFrom, rateFrom)
    numeraire = toS $ symbolVal (Proxy :: Proxy numeraire)
    insertRate (sym, rate) rateMap = Map.insert sym rate rateMap
    symNode sym = fromMaybe (error $ show sym ++  " not found in map") $
        Map.lookup sym nodeMap

toRateEdges
    :: NodeMap
    -> ABook
    -> [G.LEdge Money.SomeExchangeRate]
toRateEdges symbolMap (ABook ob) =
   catMaybes
       [ mkSellEdge <$> bestBidM    -- Consume bid: base->quote (sell)
       , mkBuyEdge  <$> bestAskM    -- Consume ask: quote->base (buy)
       ]
 where
   bestBidM = OB.oPrice <$> OB.bestBid ob
   bestAskM = OB.oPrice <$> OB.bestAsk ob
   -- Ex.: BTC/USD @ 6500 -> (BTC,USD,6500) (USD,BTC,1/6500)
   mkSellEdge bb = (baseNode, quoteNode, Money.toSomeExchangeRate bb)
   mkBuyEdge  ba = (quoteNode, baseNode, Money.toSomeExchangeRate $ Money.exchangeRateRecip ba)
   -- Util
   baseNode = lookupSymFail (abBase ob) symbolMap
   quoteNode = lookupSymFail (abQuote ob) symbolMap


-- | All paths from a given start node using BFS.
--   Returns a list of edges from the given start node to all other nodes.
allPaths
    :: (G.Graph gr)
    => G.Node       -- ^ Start node
    -> gr a b
    -> [[G.LNode b]]
allPaths start g =
      map reverse
    $ map init
    $ filter (not . null)
    $ map (G.unLPath)
    $ G.lbft start g
    -- G.lbft: From source of Data.Graph.Inductive.Query.BFS:
    -- -- Note that the label of the first node in a returned path is meaningless;
    -- -- all other nodes are paired with the label of their incoming edge.

