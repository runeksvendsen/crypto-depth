{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE GADTs #-}

module CryptoDepth.Paths
( module CryptoDepth.Internal.Types
, module CryptoDepth.Paths
)
where

import CryptoDepth.Internal.DPrelude hiding (head)
import CryptoDepth.Internal.Util
import CryptoDepth.Internal.Types
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


type DepthEdge = Pair (Maybe SomeEdgeVenue) Rational
type DepthGraph = G.Gr Sym DepthEdge

buildDepthGraph
    :: KnownSymbol numeraire
    => Rational
    -> RateMap numeraire
    -> [ABook]
    -> (DepthGraph, NodeMap)
buildDepthGraph slipPct rateMap books =
    buildGraph (toDepthEdges slipPct rateMap) books

toDepthEdges
    :: KnownSymbol numeraire
    => Rational
    -> RateMap numeraire
    -> NodeMap
    -> ABook
    -> [G.LEdge DepthEdge]
toDepthEdges slipPct rateMap symbolMap (ABook ob) =
    [ toEdge rateMap symbolMap slipPct (obBuy ob)
    , toEdge  rateMap symbolMap slipPct (OB.invert <$> obSell ob)
    ]

toEdge
    :: forall base quote numeraire.
       (KnownSymbol base, KnownSymbol quote, KnownSymbol numeraire)
    => RateMap numeraire
    -> NodeMap
    -> Rational
    -> (Venue, BuySide base quote)
    -> G.LEdge DepthEdge
toEdge rateMap symbolMap slipPct (venue, buySide) =
    (baseNode, quoteNode, pairSell)
  where
    -- Node info
    quoteSym = sideQuote buySide
    baseNode = lookupSymFail (sideBase buySide) symbolMap
    quoteNode = lookupSymFail quoteSym symbolMap
    -- Edge info
    pairSell = Pair (Just $ SomeEdgeVenue (fromBuySide buySide, venue))
                    (mkEdgeWeight sellQty)
    sellQty = usdQuoteQty quoteSym rateMap $
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
    :: KnownSymbol numeraire
    => Rational
    -> RateMap numeraire
    -> NodeMap
    -> DepthGraph
    -> G.Node          -- ^ From
    -> G.Node          -- ^ To
    -> [[SomeEdgeVenue]]    -- ^ List of From->To paths in descending order of liquidity
liquidPaths slip rm nm dg f =
    map catMaybes . map (map (pFst . snd)) . filter (not . null)
        . reverse . liquidPathsR [[]] slip rm nm dg f

liquidPathsR
    :: KnownSymbol numeraire
    => [[G.LNode DepthEdge]]    -- ^ Accumulator
    -> Rational
    -> RateMap numeraire
    -> NodeMap
    -> DepthGraph
    -> G.Node                   -- ^ From
    -> G.Node                   -- ^ To
    -> [[G.LNode DepthEdge]]    -- ^ List of From->To paths in ascending order of liquidity
liquidPathsR currPaths slipPct rateMap nodeMap g from to =
    if null mostLiquidPath
        then currPaths
        else liquidPathsR (mostLiquidPath : currPaths) slipPct rateMap nodeMap newGraph from to
  where
    mostLiquidPath :: [G.LNode DepthEdge]
    mostLiquidPath = G.unLPath . G.getLPath to . G.spTree from $ g
    newGraph = delAllLEdges (pathEdges slipPct rateMap nodeMap mostLiquidPath) g

pathEdges
    :: KnownSymbol numeraire
    => Rational
    -> RateMap numeraire
    -> NodeMap
    -> [G.LNode DepthEdge]
    -> [G.LEdge DepthEdge]
pathEdges slipPct rateMap nodeMap =
    map fromSomeEdgeVenue . depthEdgeBooks
  where
    fromSomeEdgeVenue (SomeEdgeVenue (SomeEdge (Edge bs), v)) =
        toEdge rateMap nodeMap slipPct (v,bs)

depthEdgeBooks
    :: [G.LNode DepthEdge]
    -> [SomeEdgeVenue]
depthEdgeBooks =
     catMaybes . map pFst . map snd
