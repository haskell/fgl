{-# LANGUAGE BangPatterns, CPP, ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#endif

-- |An efficient implementation of 'Data.Graph.Inductive.Graph.Graph'
-- using big-endian patricia tree (i.e. "Data.IntMap").
--
-- This module provides the following specialised functions to gain
-- more performance, using GHC's RULES pragma:
--
-- * 'Data.Graph.Inductive.Graph.insNode'
--
-- * 'Data.Graph.Inductive.Graph.insEdge'
--
-- * 'Data.Graph.Inductive.Graph.gmap'
--
-- * 'Data.Graph.Inductive.Graph.nmap'
--
-- * 'Data.Graph.Inductive.Graph.emap'

module Data.Graph.Inductive.PatriciaTree
    ( Gr
    , UGr
    )
    where

import Data.Graph.Inductive.Graph

import           Control.Applicative (liftA2)
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IM
import           Data.List           (sort)
import           Data.Maybe          (fromMaybe)

#if MIN_VERSION_containers (0,4,2)
import Control.DeepSeq (NFData (..))
#endif

#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif

#if MIN_VERSION_base (4,8,0)
import Data.Bifunctor
#else
import Control.Arrow (second)
#endif

----------------------------------------------------------------------
-- GRAPH REPRESENTATION
----------------------------------------------------------------------

newtype Gr a b = Gr (GraphRep a b)
#if __GLASGOW_HASKELL__ >= 702
  deriving (Generic)
#endif

type GraphRep a b = IntMap (Context' a b)
type Context' a b = (IntMap [b], a, IntMap [b])

type UGr = Gr () ()

----------------------------------------------------------------------
-- CLASS INSTANCES
----------------------------------------------------------------------

instance (Eq a, Ord b) => Eq (Gr a b) where
  (Gr g1) == (Gr g2) = fmap sortAdj g1 == fmap sortAdj g2
    where
      sortAdj (p,n,s) = (fmap sort p,n,fmap sort s)

instance (Show a, Show b) => Show (Gr a b) where
  showsPrec d g = showParen (d > 10) $
                    showString "mkGraph "
                    . shows (labNodes g)
                    . showString " "
                    . shows (labEdges g)

instance (Read a, Read b) => Read (Gr a b) where
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("mkGraph", s) <- lex r
    (ns,t) <- reads s
    (es,u) <- reads t
    return (mkGraph ns es, u)

instance Graph Gr where
    empty           = Gr IM.empty

    isEmpty (Gr g)  = IM.null g

    match           = matchGr

    mkGraph vs es   = insEdges es
                      . Gr
                      . IM.fromList
                      . map (second (\l -> (IM.empty,l,IM.empty)))
                      $ vs

    labNodes (Gr g) = [ (node, label)
                            | (node, (_, label, _)) <- IM.toList g ]

    noNodes   (Gr g) = IM.size g

    nodeRange (Gr g) = fromMaybe (error "nodeRange of empty graph")
                       $ liftA2 (,) (ix (IM.minViewWithKey g))
                                    (ix (IM.maxViewWithKey g))
      where
        ix = fmap (fst . fst)

    labEdges (Gr g) = do (node, (_, _, s)) <- IM.toList g
                         (next, labels)    <- IM.toList s
                         label             <- labels
                         return (node, next, label)

instance DynGraph Gr where
    (p, v, l, s) & (Gr g)
        = let !g1 = IM.insert v (fromAdj p, l, fromAdj s) g
              !g2 = addSucc g1 v p
              !g3 = addPred g2 v s
          in Gr g3

#if MIN_VERSION_containers (0,4,2)
instance (NFData a, NFData b) => NFData (Gr a b) where
  rnf (Gr g) = rnf g
#endif

#if MIN_VERSION_base (4,8,0)
instance Bifunctor Gr where
  bimap = fastNEMap

  first = fastNMap

  second = fastEMap
#endif

matchGr :: Node -> Gr a b -> Decomp Gr a b
matchGr node (Gr g)
    = case IM.lookup node g of
        Nothing
            -> (Nothing, Gr g)

        Just (p, label, s)
            -> let !g1 = IM.delete node g
                   !p' = IM.delete node p
                   !s' = IM.delete node s
                   !g2 = clearPred g1 node (IM.keys s')
                   !g3 = clearSucc g2 node (IM.keys p')
               in (Just (toAdj p', node, label, toAdj s), Gr g3)

----------------------------------------------------------------------
-- OVERRIDING FUNCTIONS
----------------------------------------------------------------------

{-# RULES
      "insNode/Data.Graph.Inductive.PatriciaTree"  insNode = fastInsNode
  #-}
fastInsNode :: LNode a -> Gr a b -> Gr a b
fastInsNode (v, l) (Gr g) = g' `seq` Gr g'
  where
    g' = IM.insert v (IM.empty, l, IM.empty) g

{-# RULES
      "insEdge/Data.Graph.Inductive.PatriciaTree"  insEdge = fastInsEdge
  #-}
fastInsEdge :: LEdge b -> Gr a b -> Gr a b
fastInsEdge (v, w, l) (Gr g) = g2 `seq` Gr g2
  where
    g1 = IM.adjust addSucc' v g
    g2 = IM.adjust addPred' w g1

    addSucc' (ps, l', ss) = (ps, l', IM.insertWith addLists w [l] ss)
    addPred' (ps, l', ss) = (IM.insertWith addLists v [l] ps, l', ss)

{-# RULES
      "gmap/Data.Graph.Inductive.PatriciaTree"  gmap = fastGMap
  #-}
fastGMap :: forall a b c d. (Context a b -> Context c d) -> Gr a b -> Gr c d
fastGMap f (Gr g) = Gr (IM.mapWithKey f' g)
  where
    f' :: Node -> Context' a b -> Context' c d
    f' = ((fromContext . f) .) . toContext

{-# RULES
      "nmap/Data.Graph.Inductive.PatriciaTree"  nmap = fastNMap
  #-}
fastNMap :: forall a b c. (a -> c) -> Gr a b -> Gr c b
fastNMap f (Gr g) = Gr (IM.map f' g)
  where
    f' :: Context' a b -> Context' c b
    f' (ps, a, ss) = (ps, f a, ss)

{-# RULES
      "emap/Data.Graph.Inductive.PatriciaTree"  emap = fastEMap
  #-}
fastEMap :: forall a b c. (b -> c) -> Gr a b -> Gr a c
fastEMap f (Gr g) = Gr (IM.map f' g)
  where
    f' :: Context' a b -> Context' a c
    f' (ps, a, ss) = (IM.map (map f) ps, a, IM.map (map f) ss)

{-# RULES
      "nemap/Data.Graph.Inductive.PatriciaTree"  nemap = fastNEMap
  #-}
fastNEMap :: forall a b c d. (a -> c) -> (b -> d) -> Gr a b -> Gr c d
fastNEMap fn fe (Gr g) = Gr (IM.map f g)
  where
    f :: Context' a b -> Context' c d
    f (ps, a, ss) = (IM.map (map fe) ps, fn a, IM.map (map fe) ss)

----------------------------------------------------------------------
-- UTILITIES
----------------------------------------------------------------------

toAdj :: IntMap [b] -> Adj b
toAdj = concatMap expand . IM.toList
  where
    expand (n,ls) = map (flip (,) n) ls

fromAdj :: Adj b -> IntMap [b]
fromAdj = IM.fromListWith addLists . map (second (:[]) . swap)

toContext :: Node -> Context' a b -> Context a b
toContext v (ps, a, ss) = (toAdj ps, v, a, toAdj ss)

fromContext :: Context a b -> Context' a b
fromContext (ps, _, a, ss) = (fromAdj ps, a, fromAdj ss)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

-- A version of @++@ where order isn't important, so @xs ++ [x]@
-- becomes @x:xs@.  Used when we have to have a function of type @[a]
-- -> [a] -> [a]@ but one of the lists is just going to be a single
-- element (and it isn't possible to tell which).
addLists :: [a] -> [a] -> [a]
addLists [a] as  = a : as
addLists as  [a] = a : as
addLists xs  ys  = xs ++ ys

addSucc :: GraphRep a b -> Node -> [(b, Node)] -> GraphRep a b
addSucc g _ []              = g
addSucc g v ((l, p) : rest) = addSucc g' v rest
    where
      g' = IM.adjust f p g
      f (ps, l', ss) = (ps, l', IM.insertWith addLists v [l] ss)


addPred :: GraphRep a b -> Node -> [(b, Node)] -> GraphRep a b
addPred g _ []              = g
addPred g v ((l, s) : rest) = addPred g' v rest
  where
    g' = IM.adjust f s g
    f (ps, l', ss) = (IM.insertWith addLists v [l] ps, l', ss)


clearSucc :: GraphRep a b -> Node -> [Node] -> GraphRep a b
clearSucc g _ []       = g
clearSucc g v (p:rest) = clearSucc g' v rest
  where
    g' = IM.adjust f p g
    f (ps, l, ss) = (ps, l, IM.delete v ss)


clearPred :: GraphRep a b -> Node -> [Node] -> GraphRep a b
clearPred g _ []       = g
clearPred g v (s:rest) = clearPred g' v rest
  where
    g' = IM.adjust f s g
    f (ps, l, ss) = (IM.delete v ps, l, ss)
