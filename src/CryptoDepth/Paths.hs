{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE GADTs #-}

module CryptoDepth.Paths
( module CryptoDepth.Internal.Types
, module CryptoDepth.Paths
, module CryptoDepth.Internal.Types.EdgePath
)
where

import CryptoDepth.Internal.DPrelude hiding (head)
import CryptoDepth.Internal.Util
import CryptoDepth.Internal.Types
import CryptoDepth.Internal.Types.EdgePath
import CryptoDepth.BuildGraph
import CryptoDepth.RateMap

import OrderBook.Types
import qualified OrderBook.Types as OB
import qualified OrderBook.Matching as Match

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Graph.Inductive.Query.SP as G
import qualified Data.Graph.Inductive.Internal.RootPath as G
import qualified Money


type DepthEdge numeraire slippage = Pair (Maybe SomeEdgeVenue) (Weight numeraire slippage)
type DepthGraph numeraire slippage = G.Gr Sym (DepthEdge numeraire slippage)

buildDepthGraph
    :: (KnownSymbol numeraire, KnownFraction slippage)
    => RateMap numeraire
    -> [ABook]
    -> (DepthGraph numeraire slippage, NodeMap)
buildDepthGraph rateMap =
    buildGraph (toDepthEdges rateMap)

toDepthEdges
    :: (KnownSymbol numeraire, KnownFraction slippage)
    => RateMap numeraire
    -> NodeMap
    -> ABook
    -> [G.LEdge (DepthEdge numeraire slippage)]
toDepthEdges rateMap symbolMap (ABook ob) =
    catMaybes
    [ toEdgeM rateMap symbolMap (obBuy ob)
    , toEdgeM  rateMap symbolMap (OB.invert <$> obSell ob)
    ]

toEdgeM
    :: forall base quote numeraire slippage.
       (KnownSymbol base, KnownSymbol quote, KnownSymbol numeraire, KnownFraction slippage)
    => RateMap numeraire
    -> NodeMap
    -> (Venue, BuySide base quote)
    -> Maybe (G.LEdge (DepthEdge numeraire slippage))
toEdgeM rateMap symbolMap (venue, buySide) =
    maybe logNotFound mkEdge pairSellM
  where
    logNotFound = trace logString Nothing
    logString = "DEBUG: Skipping edge " ++ showEdge buySide
    mkEdge pairSell = Just (baseNode, quoteNode, pairSell)
    -- Node info
    quoteSym = sideQuote buySide
    baseSym  = sideBase buySide
    baseNode = lookupSymFail baseSym symbolMap
    quoteNode = lookupSymFail quoteSym symbolMap
    -- Edge info
    pairSellM = Pair (Just $ SomeEdgeVenue (fromBuySide buySide, venue))
                 <$> (edgeWeightM quoteSym rateMap buySide :: Maybe (Weight numeraire slippage))

showEdge :: (KnownSymbol base, KnownSymbol quote) => BuySide base quote -> String
showEdge buySide =
    toS (sideBase buySide) ++ "->" ++ toS (sideQuote buySide)

edgeWeightM
    :: forall numeraire base quote slippage.
       (KnownSymbol numeraire, KnownSymbol base, KnownSymbol quote, KnownFraction slippage)
    => Sym
    -> RateMap numeraire
    -> BuySide base quote
    -> Maybe (Weight numeraire slippage)
edgeWeightM quoteSym rateMap buySide =
    fromRational <$> (denseQtyM >>= mkEdgeWeight)
  where
    slipPct = fracValPercent (Proxy :: Proxy slippage)
    denseQtyM = numeraireQuoteQty quoteSym rateMap $
        Match.slippageSell buySide slipPct

mkEdgeWeight :: Money.Dense numeraire -> Maybe Rational
mkEdgeWeight dense = let qty = toRational dense in
    if qty == 0 then Nothing else Just (1 / qty)

-- | Convert an amount denomiated in "quoteSym" to a
--    quantity denominated in "numeraire" (approximation).
--   E.g. convert ETH to EUR.
numeraireQuoteQty
    :: forall numeraire base quote.
       (KnownSymbol numeraire, KnownSymbol quote)
    => Sym
    -> RateMap numeraire
    -> Match.MatchResult base quote
    -> Maybe (Money.Dense numeraire)
numeraireQuoteQty quoteSym rateMap matchRes =
    mkResultInvert <$> lookupRateM quoteSym rateMap
  where
    -- Invert 'RateFrom numeraire' in order to convert *to* 'numeraire'
    mkResultInvert (RateFrom erInv) = mkResult (Money.exchangeRateRecip erInv)
    mkResult :: forall src.
                KnownSymbol src
             => Money.ExchangeRate src numeraire
             -> Money.Dense numeraire
    mkResult er =
        case sameSymbol (Proxy :: Proxy quote) (Proxy :: Proxy src) of
            Just Refl -> Money.exchange er (Match.resQuoteQty matchRes)
            Nothing   -> error $ printf "RateMap: wrong 'src' for sym '%s': %s" quoteSym (show er)

-- | Paths from given symbol to another given symbol in descending order of liquidity
liquidPaths
    :: (KnownSymbol numeraire, KnownFraction slippage)
    => RateMap numeraire
    -> NodeMap
    -> DepthGraph numeraire slippage
    -> G.Node          -- ^ From
    -> G.Node          -- ^ To
    -> [EdgePath numeraire] -- ^ List of From->To paths in descending order of liquidity
liquidPaths rm nm dg f =
    map EdgePath .
    toNonEmpty . map catMaybes . map (map (pFst . snd))
        . reverse . liquidPathsR [[]] rm nm dg f
  where
    toNonEmpty = map (nonEmptyErr . nonEmpty) . filter (not . null)
    nonEmptyErr = fromMaybe (error "non-empty list is empty")

liquidPathsR
    :: forall numeraire slippage.
       (KnownSymbol numeraire, KnownFraction slippage)
    => [[G.LNode (DepthEdge numeraire slippage)]]    -- ^ Accumulator
    -> RateMap numeraire
    -> NodeMap
    -> DepthGraph numeraire slippage
    -> G.Node                   -- ^ From
    -> G.Node                   -- ^ To
    -> [[G.LNode (DepthEdge numeraire slippage)]]    -- ^ List of From->To paths in ascending order of liquidity
liquidPathsR currPaths rateMap nodeMap g from to =
    if null mostLiquidPath
        then currPaths
        else liquidPathsR (mostLiquidPath : currPaths) rateMap nodeMap newGraph from to
  where
    mostLiquidPath :: [G.LNode (DepthEdge numeraire slippage)]
    mostLiquidPath = G.unLPath . G.getLPath to . G.spTree from $ g
    newGraph = delAllLEdges (pathEdges rateMap nodeMap mostLiquidPath) g

pathEdges
    :: forall numeraire slippage.
       (KnownSymbol numeraire, KnownFraction slippage)
    => RateMap numeraire
    -> NodeMap
    -> [G.LNode (DepthEdge numeraire slippage)]
    -> [G.LEdge (DepthEdge numeraire slippage)]
pathEdges rateMap nodeMap =
    map fromSomeEdgeVenue . depthEdgeBooks
  where
    fromSomeEdgeVenue :: SomeEdgeVenue -> G.LEdge (DepthEdge numeraire slippage)
    fromSomeEdgeVenue (SomeEdgeVenue (SomeEdge (Edge bs), v)) =
        -- 'toEdgeM' returns edges that end up in 'DepthGraph'.
        --  'pathEdges' takes in edges from 'DepthGraph', which means that 'toEdgeM'
        --  must return a "Just" for this edge (because it did so in order for the edge
        --  to end up the the 'DepthGraph' in the first place)
        fromMaybe (error $ "BUG: pathEdges: " ++ showEdge bs ++ " not found") $
        toEdgeM rateMap nodeMap (v,bs)

depthEdgeBooks
    :: [G.LNode (DepthEdge numeraire slippage)]
    -> [SomeEdgeVenue]
depthEdgeBooks =
     catMaybes . map pFst . map snd
