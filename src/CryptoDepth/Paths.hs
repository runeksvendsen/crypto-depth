{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE GADTs #-}

module CryptoDepth.Paths
( module CryptoDepth.Types
, module CryptoDepth.Paths
)
where

import CryptoDepth.Internal.DPrelude hiding (head)
import CryptoDepth.Internal.Util
import CryptoDepth.Types
import CryptoDepth.BuildGraph
import CryptoDepth.RateMap

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






--- #### Depth graph #### ---

-- TODO: move out of here
slippagePercent :: Rational
slippagePercent = 5 % 1

type DepthEdge = Pair (Maybe SomeSide) Rational
type DepthGraph = G.Gr Sym DepthEdge

buildDepthGraph :: KnownSymbol numeraire => RateMap numeraire -> [ABook] -> (DepthGraph, NodeMap)
buildDepthGraph rateMap books =
    buildGraph (toDepthEdges rateMap) books

toDepthEdges
    :: KnownSymbol numeraire
    => RateMap numeraire
    -> NodeMap
    -> ABook
    -> [G.LEdge DepthEdge]
toDepthEdges rateMap symbolMap ab@(ABook anyBook@ob) =
   [ sellEdge rateMap symbolMap (obBids ob)
   , buyEdge  rateMap symbolMap (obAsks ob)
   ]

sellEdge
    :: forall venue base quote numeraire.
       (KnownSymbol venue, KnownSymbol base, KnownSymbol quote, KnownSymbol numeraire)
    => RateMap numeraire
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
    :: forall venue base quote numeraire.
       (KnownSymbol venue, KnownSymbol base, KnownSymbol quote, KnownSymbol numeraire)
    => RateMap numeraire
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

toEdge
    :: KnownSymbol numeraire
    => RateMap numeraire
    -> NodeMap
    -> SomeSide
    -> G.LEdge DepthEdge
toEdge rateMap symbolMap (SomeSide (Left bs)) = sellEdge rateMap symbolMap bs
toEdge rateMap symbolMap (SomeSide (Right ss)) = buyEdge rateMap symbolMap ss

-- | Paths from given symbol to another given symbol in descending order of liquidity
liquidPaths
    :: KnownSymbol numeraire
    => RateMap numeraire
    -> NodeMap
    -> DepthGraph
    -> G.Node          -- ^ From
    -> G.Node          -- ^ To
    -> [[SomeSide]]    -- ^ List of From->To paths in descending order of liquidity
liquidPaths rm nm dg f =
    map catMaybes . map (map (pFst . snd)) . filter (not . null)
        . reverse . liquidPathsR [[]] rm nm dg f

liquidPathsR
    :: KnownSymbol numeraire
    => [[G.LNode DepthEdge]]    -- ^ Accumulator
    -> RateMap numeraire
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
    :: KnownSymbol numeraire
    => RateMap numeraire
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
