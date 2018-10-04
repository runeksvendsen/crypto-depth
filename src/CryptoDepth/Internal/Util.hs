module CryptoDepth.Internal.Util where

import CryptoDepth.Internal.DPrelude hiding (head)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.HashMap.Strict as Map
import OrderBook.Types                          (AnyBook(..))
import CryptoDepth.Internal.Types               (ABook(ABook))
import CryptoVenues.Fetch                       (MarketBook)


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

lookupSymFail
    :: (Hashable k, Eq k, Show k)
    => k
    -> Map.HashMap k a
    -> a
lookupSymFail sym symbolMap = fromMaybe (error $ "node not found: " ++ show sym) $
    Map.lookup sym symbolMap

toABook :: MarketBook venue => AnyBook venue -> ABook
toABook (AnyBook ob) = ABook ob
