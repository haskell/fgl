-- (c) 2000-2005 by Martin Erwig [see file COPYRIGHT]

module Data.Graph.Inductive.Query.SP(
    spTree,spLength,sp,
    dijkstra
) where

import qualified Data.Graph.Inductive.Internal.Heap as H

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Internal.RootPath

expand :: Real b => b -> LPath b -> Context a b -> [H.Heap b (LPath b)]
expand d (LP p) (_,_,_,s) = map (\(l,v)->H.unit (l+d) (LP ((v,l+d):p))) s

-- | Implementation of Dijkstra's shortest path algorithm  
dijkstra :: (Graph gr, Real b) => H.Heap b (LPath b) -> gr a b -> LRTree b
dijkstra h g | H.isEmpty h || isEmpty g = []
dijkstra h g =
    case match v g of
         (Just c,g')  -> p:dijkstra (H.mergeAll (h':expand d p c)) g'
         (Nothing,g') -> dijkstra h' g'  
    where (_,p@(LP ((v,d):_)),h') = H.splitMin h
        
spTree :: (Graph gr, Real b) => Node -> gr a b -> LRTree b
spTree v = dijkstra (H.unit 0 (LP [(v,0)]))

spLength :: (Graph gr, Real b) => Node -> Node -> gr a b -> b
spLength s t = getDistance t . spTree s

sp :: (Graph gr, Real b) => Node -> Node -> gr a b -> Path
sp s t = getLPathNodes t . spTree s
