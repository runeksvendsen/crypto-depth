{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}

module CryptoDepth.Paths
( module CryptoDepth.Types
, module CryptoDepth.Paths
)
where

import CryptoDepth.Internal.DPrelude hiding (head)
import CryptoDepth.Internal.Util
import CryptoDepth.Types
import OrderBook.Types
import qualified OrderBook.Matching as Match

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Graph.Inductive.Query.SP as G
import qualified Data.Graph.Inductive.Query.BFS as G
import qualified Data.Graph.Inductive.Internal.RootPath as G
import Data.List (init, last, tail, head)
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.HashMap.Strict as Map
import qualified Money
import qualified Data.Vector  as Vec


--- #### Graph building #### ---

type Sym = Text                         -- ^ A currency symbol, e.g. "USD", "EUR", "BTC", "ETH" etc.
type NodeMap = Map.HashMap Sym Int
type GraphM m a = S.StateT NodeMap m a

buildGraph
    :: forall gr edgeLabel. (G.DynGraph gr, Show edgeLabel)
    => (NodeMap -> ABook -> [G.LEdge edgeLabel])
    -> [ABook]
    -> (gr Sym edgeLabel, NodeMap)
buildGraph toEdges books =
    let (edges, nodeMap) = S.runState (buildGraphM toEdges books) Map.empty
        nodes = sortOn fst $ map swap (Map.toList nodeMap)
    in (G.mkGraph nodes edges, nodeMap)

buildGraphM
    :: forall m edgeLabel. (Monad m, Show edgeLabel)
    => (NodeMap -> ABook -> [G.LEdge edgeLabel])
    -> [ABook]
    -> GraphM m [G.LEdge edgeLabel]
buildGraphM toEdges =
    foldrM insertBook []
  where
    symbolNode :: Sym -> NodeMap -> (G.LNode Sym, NodeMap)
    symbolNode symStr bimap =
            let nextNode = Map.size bimap in
            case Map.lookup symStr bimap of
                Nothing   -> ((nextNode, symStr), Map.insert symStr nextNode bimap)
                Just node -> ((node,     symStr), bimap)
    insertBook
        :: ABook
        -> [G.LEdge edgeLabel]
        -> GraphM m [G.LEdge edgeLabel]
    insertBook ab@(ABook ob) edges = do
        bimap <- S.get
        let (baseSym, quoteSym) = (abBase ob, abQuote ob)
            (baseNode,  baseBimap)  = symbolNode baseSym bimap
            (quoteNode, quoteBimap) = symbolNode quoteSym baseBimap
        S.put quoteBimap
        return $ edges ++ toEdges quoteBimap ab


--- #### Graph queries #### ---

allPaths
    :: (G.Graph gr, Real b)
    => G.Node       -- ^ Start node
    -> gr a b
    -> [[G.LNode b]]
allPaths start g =
      map init
    $ filter (not . null)
    $ map (G.unLPath)
    $ G.lbft start g
    -- G.lbft: From source of Data.Graph.Inductive.Query.BFS:
    --
    -- -- Note that the label of the first node in a returned path is meaningless;
    -- -- all other nodes are paired with the label of their incoming edge.


--- #### Rate graph #### ---

type RateGraph = G.Gr Sym Rational
type USDRateMap = Map.HashMap Sym Rational

toRate
    :: G.Graph gr
    => gr Sym Rational
    -> [G.LNode Rational]
    -> Maybe (Sym, Rational)
toRate _ [] = Nothing
toRate g nodes@(firstNode:_) = Just
    (firstNodeLabel, rate)
  where
    rate = 1 / (foldr (*) (1%1) $ map snd nodes)
    firstNodeLabel = fromMaybe (error $ "toRate: no such node in graph: " ++ show firstNode) $
        G.lab g (fst firstNode)

buildRateMap :: [ABook] -> USDRateMap
buildRateMap books =
    toRateMap (graph :: RateGraph) nodeMap
  where
    (graph, nodeMap) = buildGraph toRateEdges books

toRateMap
    :: (G.Graph gr, Show (gr Sym Rational))
    => gr Sym Rational
    -> NodeMap
    -> USDRateMap
toRateMap g nodeMap =
      insertRate ("USD", 1 % 1)     -- USD/USD exchange rate is 1
    $ foldr insertRate Map.empty
    $ catMaybes
    $ map (toRate g)
    $ allPaths (symNode "USD") g
  where
    insertRate (sym, rate) rateMap = Map.insert sym rate rateMap
    symNode sym = fromMaybe (error $ show sym ++  " not found in map") $
        Map.lookup sym nodeMap

toRateEdges
    :: NodeMap
    -> ABook
    -> [G.LEdge Rational]
toRateEdges symbolMap ab@(ABook anyBook@ob) =
   catMaybes
       [ mkSellEdge <$> bestBidM    -- Consume bid: quote->base
       , mkBuyEdge  <$> bestAskM    -- Consume ask: base->quote
       ]
 where
   bestBidM = rationalPrice <$> bestBid ob
   bestAskM = rationalPrice <$> bestAsk ob
   -- Ex.: BTC/USD @ 6500 -> (BTC,USD,6500) (USD,BTC,1/6500)
   mkSellEdge bb = (baseNode, quoteNode, bb)
   mkBuyEdge  ba = (quoteNode, baseNode, 1 / ba)
   -- Util
   rationalPrice = Money.exchangeRateToRational . oPrice
   baseNode = lookupSymFail (abBase ob) symbolMap
   quoteNode = lookupSymFail (abQuote ob) symbolMap


--- #### Depth graph #### ---

-- TODO: move out of here
slippagePercent :: Rational
slippagePercent = 5 % 1

type DepthEdge = Pair (Maybe SomeSide) Rational
type DepthGraph = G.Gr Sym DepthEdge

buildDepthGraph :: USDRateMap -> [ABook] -> (DepthGraph, NodeMap)
buildDepthGraph rateMap books =
    buildGraph (toDepthEdges rateMap) books

toDepthEdges
    :: USDRateMap
    -> NodeMap
    -> ABook
    -> [G.LEdge DepthEdge]
toDepthEdges rateMap symbolMap ab@(ABook anyBook@ob) =
   [ sellEdge rateMap symbolMap (obBids ob)
   , buyEdge  rateMap symbolMap (obAsks ob)
   ]

sellEdge
    :: forall venue base quote.
       (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
    => USDRateMap
    -> NodeMap
    -> BuySide venue base quote
    -> G.LEdge DepthEdge
sellEdge rateMap symbolMap bs =
    (baseNode, quoteNode, pairSell)
  where
    -- Node info
    quoteSym = abQuote bs
    baseNode = lookupSymFail (abBase bs) symbolMap
    quoteNode = lookupSymFail quoteSym symbolMap
    -- Edge info
    pairSell = Pair (Just . SomeSide . Left $ bs) (mkEdgeWeight sellQty)
    sellQty = usdQuoteQty quoteSym rateMap $
        Match.slippageSell bs slippagePercent

buyEdge
    :: forall venue base quote.
       (KnownSymbol venue, KnownSymbol base, KnownSymbol quote)
    => USDRateMap
    -> NodeMap
    -> SellSide venue base quote
    -> G.LEdge DepthEdge
buyEdge rateMap symbolMap ss =
    (quoteNode, baseNode, pairBuy)
  where
    -- Nodes info
    quoteSym = abQuote ss
    baseNode = lookupSymFail (abBase ss) symbolMap
    quoteNode = lookupSymFail quoteSym symbolMap
    -- Edge info
    pairBuy = Pair (Just . SomeSide . Right $ ss) (mkEdgeWeight buyQty)
    buyQty  = usdQuoteQty quoteSym rateMap $
        Match.slippageBuy ss slippagePercent

mkEdgeWeight :: Rational -> Rational
mkEdgeWeight qty =
    if qty == 0 then 1000000%1 else 1 / qty
    -- A zero-volume edge will get a weight as if it had 1e-6 volume

toEdge
    :: USDRateMap
    -> NodeMap
    -> SomeSide
    -> G.LEdge DepthEdge
toEdge rateMap symbolMap (SomeSide (Left bs)) = sellEdge rateMap symbolMap bs
toEdge rateMap symbolMap (SomeSide (Right ss)) = buyEdge rateMap symbolMap ss

-- | Paths from given symbol to another given symbol in descending order of liquidity
liquidPaths
    :: USDRateMap
    -> NodeMap
    -> DepthGraph
    -> G.Node          -- ^ From
    -> G.Node          -- ^ To
    -> [[SomeSide]]    -- ^ List of From->To paths in descending order of liquidity
liquidPaths rm nm dg f =
    map catMaybes . map (map (pFst . snd)) . filter (not . null)
        . reverse . liquidPathsR [[]] rm nm dg f

liquidPathsR
    :: [[G.LNode DepthEdge]]    -- ^ Accumulator
    -> USDRateMap
    -> NodeMap
    -> DepthGraph
    -> G.Node                   -- ^ From
    -> G.Node                   -- ^ To
    -> [[G.LNode DepthEdge]]    -- ^ List of From->To paths in ascending order of liquidity
liquidPathsR currPaths rateMap nodeMap g from to =
    if null mostLiquidPath
        then currPaths
        else liquidPathsR (mostLiquidPath : currPaths) rateMap nodeMap newGraph from to
  where
    mostLiquidPath :: [G.LNode DepthEdge]
    mostLiquidPath = G.unLPath . G.getLPath to . G.spTree from $ g
    newGraph = delAllLEdges (pathEdges rateMap nodeMap mostLiquidPath) g

pathEdges
    :: USDRateMap
    -> NodeMap
    -> [G.LNode DepthEdge]
    -> [G.LEdge DepthEdge]
pathEdges rateMap nodeMap =
    map (toEdge rateMap nodeMap) . depthEdgeBooks

depthEdgeBooks
    :: [G.LNode DepthEdge]
    -> [SomeSide]
depthEdgeBooks =
     catMaybes . map pFst . map snd
