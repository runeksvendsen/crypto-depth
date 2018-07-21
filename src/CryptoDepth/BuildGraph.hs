{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}

module CryptoDepth.BuildGraph

where

import CryptoDepth.Internal.DPrelude hiding (head)
import CryptoDepth.Internal.Types

import qualified Data.Graph.Inductive.Graph as G
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.HashMap.Strict as Map


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
    symbolNode symStr nmap =
            let nextNode = Map.size nmap in
            case Map.lookup symStr nmap of
                Nothing   -> ((nextNode, symStr), Map.insert symStr nextNode nmap)
                Just node -> ((node,     symStr), nmap)
    insertBook
        :: ABook
        -> [G.LEdge edgeLabel]
        -> GraphM m [G.LEdge edgeLabel]
    insertBook ab@(ABook ob) edges = do
        nmap <- S.get
        let (baseSym, quoteSym) = (abBase ob, abQuote ob)
            (baseNode,  baseBimap)  = symbolNode baseSym nmap
            (quoteNode, quoteBimap) = symbolNode quoteSym baseBimap
        S.put quoteBimap
        return $ edges ++ toEdges quoteBimap ab
