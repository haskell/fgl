module Data.Graph.Inductive.Query.TransClos(
    trc
) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS (reachable)


getNewEdges :: (DynGraph gr) => [LNode a] -> gr a b -> [LEdge ()]
getNewEdges vs g = map (`toLEdge` ())
                   . concatMap (\u -> map ((,) u) (reachable u g))
                   $ map fst vs

{-|
Finds the transitive closure of a directed graph.
Given a graph G=(V,E), its transitive closure is the graph:
G* = (V,E*) where E*={(i,j): i,j in V and there is a path from i to j in G}
-}
trc :: (DynGraph gr) => gr a b -> gr a ()
trc g = insEdges (getNewEdges ln g) (insNodes ln empty)
        where ln = labNodes g
