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
    [ toEdge rateMap symbolMap (obBuy ob)
    , toEdge  rateMap symbolMap (OB.invert <$> obSell ob)
    ]

toEdge
    :: forall base quote numeraire slippage.
       (KnownSymbol base, KnownSymbol quote, KnownSymbol numeraire, KnownFraction slippage)
    => RateMap numeraire
    -> NodeMap
    -> (Venue, BuySide base quote)
    -> G.LEdge (DepthEdge numeraire slippage)
toEdge rateMap symbolMap (venue, buySide) =
    (baseNode, quoteNode, pairSell)
  where
    -- Node info
    quoteSym = sideQuote buySide
    baseNode = lookupSymFail (sideBase buySide) symbolMap
    quoteNode = lookupSymFail quoteSym symbolMap
    -- Edge info
    pairSell = Pair (Just $ SomeEdgeVenue (fromBuySide buySide, venue))
                    (edgeWeight quoteSym rateMap buySide)

edgeWeight
    :: forall numeraire base quote slippage.
       (KnownSymbol numeraire, KnownSymbol base, KnownSymbol quote, KnownFraction slippage)
    => Sym
    -> RateMap numeraire
    -> BuySide base quote
    -> Weight numeraire slippage
edgeWeight quoteSym rateMap buySide =
    fromRational $ mkEdgeWeight denseQty
  where
    slipPct = fracValPercent (Proxy :: Proxy slippage)
    denseQty = usdQuoteQty quoteSym rateMap $
        Match.slippageSell buySide slipPct

mkEdgeWeight :: Money.Dense numeraire -> Rational
mkEdgeWeight dense = let qty = toRational dense in
    if qty == 0 then 1000000%1 else 1 / qty
    -- A zero-volume edge will get a weight as if it had 1e-6 volume

usdQuoteQty
    :: forall numeraire base quote.
       (KnownSymbol numeraire, KnownSymbol quote)
    => Sym
    -> RateMap numeraire
    -> Match.MatchResult base quote
    -> Money.Dense numeraire
usdQuoteQty quoteSym rateMap matchRes =
    case lookupRateFail quoteSym rateMap of
        -- Invert 'RateFrom numeraire' in order to convert *to* 'numeraire'
        RateFrom erInv -> mkResult (Money.exchangeRateRecip erInv)
  where
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
        toEdge rateMap nodeMap (v,bs)

depthEdgeBooks
    :: [G.LNode (DepthEdge numeraire slippage)]
    -> [SomeEdgeVenue]
depthEdgeBooks =
     catMaybes . map pFst . map snd
