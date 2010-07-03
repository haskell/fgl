-- (c) 1999 - 2002 by Martin Erwig [see file COPYRIGHT]
-- | Tree-based implementation of 'Graph' and 'DynGraph'

module Data.Graph.Inductive.Tree (Gr,UGr) where

import Data.List        (foldl')

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Internal.FiniteMap

import Data.Maybe (fromJust)


----------------------------------------------------------------------
-- GRAPH REPRESENTATION
----------------------------------------------------------------------

data Gr a b = Gr (GraphRep a b)

type GraphRep a b = FiniteMap Node (Context' a b)
type Context' a b = (Adj b,a,Adj b)

type UGr = Gr () ()


----------------------------------------------------------------------
-- CLASS INSTANCES
----------------------------------------------------------------------


-- Show
--
showsGraph :: (Show a,Show b) => GraphRep a b -> ShowS
showsGraph Empty = id
showsGraph (Node _ l (v,(_,l',s)) r) = showsGraph l . ('\n':) . 
     shows v . (':':) . shows l' . ("->"++) . shows s . showsGraph r
                
instance (Show a,Show b) => Show (Gr a b) where
  showsPrec _ (Gr g) = showsGraph g


-- Graph
-- 
instance Graph Gr where
  empty           = Gr emptyFM
  isEmpty (Gr g)  = case g of {Empty -> True; _ -> False}
  match           = matchGr
  mkGraph vs es   = (insEdges' . insNodes vs) empty
        where
          insEdges' g = foldl' (flip insEdge) g es

  labNodes (Gr g) = map (\(v,(_,l,_))->(v,l)) (fmToList g)
  -- more efficient versions of derived class members
  --
  matchAny (Gr Empty)                = error "Match Exception, Empty Graph"
  matchAny g@(Gr (Node _ _ (v,_) _)) = (c,g') where (Just c,g') = matchGr v g
  noNodes   (Gr g) = sizeFM g
  nodeRange (Gr Empty) = (0,0)
  nodeRange (Gr g)     = (ix (minFM g),ix (maxFM g)) where ix = fst.fromJust
  labEdges  (Gr g) = concatMap (\(v,(_,_,s))->map (\(l,w)->(v,w,l)) s) (fmToList g)


matchGr v (Gr g) = 
      case splitFM g v of 
           Nothing -> (Nothing,Gr g)
           Just (g',(_,(p,l,s))) -> (Just (p',v,l,s),Gr g2)
                where s'   = filter ((/=v).snd) s
                      p'   = filter ((/=v).snd) p
                      g1   = updAdj g' s' (clearPred v)
                      g2   = updAdj g1 p' (clearSucc v)


-- DynGraph
-- 
instance DynGraph Gr where
  (p,v,l,s) & (Gr g) | elemFM g v = error ("Node Exception, Node: "++show v)
                     | otherwise  = Gr g3
      where g1 = addToFM g v (p,l,s)
            g2 = updAdj g1 p (addSucc v)
            g3 = updAdj g2 s (addPred v)


----------------------------------------------------------------------
-- UTILITIES
----------------------------------------------------------------------

addSucc v l (p,l',s) = (p,l',(l,v):s)
addPred v l (p,l',s) = ((l,v):p,l',s)

clearSucc v _ (p,l,s) = (p,l,filter ((/=v).snd) s)
clearPred v _ (p,l,s) = (filter ((/=v).snd) p,l,s)

updAdj :: GraphRep a b -> Adj b -> (b -> Context' a b -> Context' a b) -> GraphRep a b
updAdj g []         _              = g
updAdj g ((l,v):vs) f | elemFM g v = updAdj (updFM g v (f l)) vs f
                      | otherwise  = error ("Edge Exception, Node: "++show v)



