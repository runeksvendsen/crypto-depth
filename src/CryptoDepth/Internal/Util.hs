module CryptoDepth.Internal.Util where

import CryptoDepth.Internal.DPrelude hiding (head)
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

lookupRateFail sym rateMap = fromMaybe (error $ "rate not found: " ++ show sym) $
    Map.lookup sym rateMap
lookupOrFail sym symbolMap = fromMaybe (error $ "node not found: " ++ show sym) $
    Map.lookup sym symbolMap
usdQuoteQty quoteSym rateMap matchRes = 
    lookupRateFail quoteSym rateMap * toRational (Match.resQuoteQty matchRes)
