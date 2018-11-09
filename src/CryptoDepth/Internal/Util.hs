module CryptoDepth.Internal.Util where

import CryptoDepth.Internal.DPrelude hiding (head)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet                   as Set
import OrderBook.Types                          (AnyBook(..))
import CryptoDepth.Internal.Types               (ABook(ABook))
import CryptoVenues.Fetch                       (MarketBook)
import Data.List                                (sortBy)


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

-- | Trim a graph so that it contains, at most, a specified number of nodes.
--   The edges with the "largest" edge labels (as defined by the edge label
--   comparison function) that are connected to the remaining nodes are
--   retained.
trim
    :: (Eq edgeLabel, G.DynGraph gr)
    => (edgeLabel -> edgeLabel -> Ordering) -- ^ Edge comparison function.
                                            --   The smallest edges are removed.
    -> Int                                  -- ^ Retain this number of nodes
    -> gr nodeLabel edgeLabel               -- ^ Input graph
    -> gr nodeLabel edgeLabel               -- ^ Trimmed graph
trim compare maxNodes graph =
    let (nodeSet, edges) = foldr f (Set.fromList [], []) sortedEdges
        newGraphAllNodes = G.mkGraph (G.labNodes graph) edges
    in G.labnfilter (\lNode -> fst lNode `Set.member` nodeSet) newGraphAllNodes
  where
    f edge@(fromNode, toNode, _) state@(nodeSet, edgeList)
        | fromNode `Set.member` nodeSet && toNode `Set.member` nodeSet =
            (nodeSet, edge : edgeList)
        | Set.size nodeSet >= maxNodes =
            state
        | otherwise =
            (addNodes nodeSet [fromNode, toNode] , edge : edgeList)
    addNodes set = foldr Set.insert set
    sortedEdges = sortBy (compare `on` G.edgeLabel) $ G.labEdges graph
