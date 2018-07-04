module CryptoDepth.Internal.Util where

import CryptoDepth.Internal.DPrelude hiding (head)
import qualified OrderBook.Matching as Match
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.HashMap.Strict as Map


traceIt :: Show a => a -> a
traceIt a = show a `trace` a

pathLabels
    :: G.LPath b
    -> [b]
pathLabels = map snd . G.unLPath

delAllLEdges
    :: (G.DynGraph gr, Eq b)
    => [G.LEdge b]
    -> gr a b
    -> gr a b
delAllLEdges edges =
    flip (foldr G.delAllLEdge) edges

lookupRateFail
    :: (Hashable k, Eq k, Show k)
    => k
    -> Map.HashMap k a
    -> a
lookupRateFail sym rateMap = fromMaybe (error $ "rate not found: " ++ show sym) $
    Map.lookup sym rateMap

lookupSymFail
    :: (Hashable k, Eq k, Show k)
    => k
    -> Map.HashMap k a
    -> a
lookupSymFail sym symbolMap = fromMaybe (error $ "node not found: " ++ show sym) $
    Map.lookup sym symbolMap

usdQuoteQty
    :: (Show k, Eq k, Hashable k)
    => k
    -> Map.HashMap k Rational
    -> Match.MatchResult base quote
    -> Rational
usdQuoteQty quoteSym rateMap matchRes =
    lookupRateFail quoteSym rateMap * toRational (Match.resQuoteQty matchRes)
