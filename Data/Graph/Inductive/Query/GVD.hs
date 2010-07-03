-- (c) 2000-2005 by Martin Erwig [see file COPYRIGHT]
-- | Graph Voronoi Diagram 

module Data.Graph.Inductive.Query.GVD (
    Voronoi,
    gvdIn,gvdOut,
    voronoiSet,nearestNode,nearestDist,nearestPath,
--    vd,nn,ns,
--    vdO,nnO,nsO
) where

import Data.Maybe (listToMaybe)
import Data.List (nub)

import qualified Data.Graph.Inductive.Internal.Heap as H

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.SP (dijkstra)
import Data.Graph.Inductive.Internal.RootPath
import Data.Graph.Inductive.Basic

type Voronoi a = LRTree a

gvdIn :: (DynGraph gr, Real b) => [Node] -> gr a b -> Voronoi b
gvdIn vs g = gvdOut vs (grev g)

gvdOut :: (Graph gr, Real b) => [Node] -> gr a b -> Voronoi b
gvdOut vs = dijkstra (H.build (zip (repeat 0) (map (\v->LP [(v,0)]) vs)))

voronoiSet :: Real b => Node -> Voronoi b -> [Node]
voronoiSet v = nub . concat . filter (\p->last p==v) . map (\(LP p)->map fst p)

maybePath :: Real b => Node -> Voronoi b -> Maybe (LPath b)
maybePath v = listToMaybe . filter (\(LP ((w,_):_))->w==v)

nearestNode :: Real b => Node -> Voronoi b -> Maybe Node
nearestNode v = fmap (\(LP ((w,_):_))->w) . maybePath v

nearestDist :: Real b => Node -> Voronoi b -> Maybe b
nearestDist v = fmap (\(LP ((_,l):_))->l) . maybePath v

nearestPath :: Real b => Node -> Voronoi b -> Maybe Path
nearestPath v = fmap (\(LP p)->map fst p) . maybePath v


-- vd = gvdIn [4,5] vor
-- vdO = gvdOut [4,5] vor
-- nn = map (flip nearestNode vd) [1..8]
-- nnO = map (flip nearestNode vdO) [1..8]
-- ns = map (flip voronoiSet vd) [1..8]
-- nsO = map (flip voronoiSet vdO) [1..8]
