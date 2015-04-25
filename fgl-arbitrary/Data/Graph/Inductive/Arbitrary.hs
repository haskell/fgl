{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Data.Graph.Inductive.Arbitrary
   Description : Arbitrary definition for fgl graphs
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com

This module provides default definitions for use with QuickCheck's
'Arbitrary' class.

 -}
module Data.Graph.Inductive.Arbitrary
       ( arbitraryGraph
       , arbitraryGraphWith
       , shrinkGraph
         -- * Specific graph structures
       , NoMultipleEdges(..)
       , SimpleGraph(..)
         -- * Node and edge lists
       , arbitraryNodes
       , arbitraryEdges
       , GraphNodesEdges(..)
       ) where

import           Data.Graph.Inductive.Graph        (Graph, LEdge, LNode, Node,
                                                    delNode, mkGraph, nodes,
                                                    toEdge)
import qualified Data.Graph.Inductive.PatriciaTree as P
import qualified Data.Graph.Inductive.Tree         as T

import Test.QuickCheck (Arbitrary (..), Gen, elements, listOf)

import Control.Applicative (liftA3)
import Data.Function       (on)
import Data.Functor        ((<$>))
import Data.List           (deleteBy, groupBy, sortBy)

-- -----------------------------------------------------------------------------

-- | Generally a list of labelled nodes.
arbitraryNodes :: (Arbitrary a) => Gen [LNode a]
arbitraryNodes = arbitrary >>= mapM ((<$> arbitrary) . (,)) . uniq

-- | Given a specified list of nodes, generate a list of edges.
arbitraryEdges :: (Arbitrary b) => [LNode a] -> Gen [LEdge b]
arbitraryEdges lns
  | null lns  = return []
  | otherwise = listOf (liftA3 (,,) nGen nGen arbitrary)
  where
    nGen = elements (map fst lns)

-- | Defined so as to be able to generate valid 'arbitrary' node and
--   edge lists.
--
--   If any specific structure (no multiple edges, no loops, etc.) is
--   required then you will need to post-process this after generating
--   it.
data GraphNodesEdges a b = GNEs { graphNodes :: [LNode a]
                                , graphEdges :: [LEdge b]
                                }
  deriving (Eq, Ord, Show, Read)

instance (Arbitrary a, Arbitrary b) => Arbitrary (GraphNodesEdges a b) where
  arbitrary = do ns <- arbitraryNodes
                 GNEs ns <$> arbitraryEdges ns

  shrink (GNEs ns es) = case ns of
                          _:_:_ -> map delN ns
                          _     -> []
    where
      delN ln@(n,_) = GNEs ns' es'
        where
          ns' = deleteBy ((==)`on`fst) ln ns
          es' = filter (not . hasN) es

          hasN (v,w,_) = v == n || w == n

-- -----------------------------------------------------------------------------

-- | Generate an arbitrary graph.  Multiple edges are allowed.
arbitraryGraph :: (Graph gr, Arbitrary a, Arbitrary b) => Gen (gr a b)
arbitraryGraph = arbitraryGraphWith id

-- | Generate an arbitrary graph, using the specified function to
--   manipulate the generated list of edges (e.g. remove multiple
--   edges).
arbitraryGraphWith :: (Graph gr, Arbitrary a, Arbitrary b)
                      => ([LEdge b] -> [LEdge b]) -> Gen (gr a b)
arbitraryGraphWith f = do GNEs ns es <- arbitrary
                          let es' = f es
                          return (mkGraph ns es')

-- Ensure we have a list of unique Node values; this will also sort
-- the list, but that shouldn't matter.
uniq :: [Node] -> [Node]
uniq = uniqBy id

uniqBy :: (Ord b) => (a -> b) -> [a] -> [a]
uniqBy f = map head . groupBy ((==) `on` f) . sortBy (compare `on` f)

-- | For a graph with at least two nodes, return every possible way of
--   deleting a single node (i.e. will never shrink to an empty
--   graph).
shrinkGraph :: (Graph gr) => gr a b -> [gr a b]
shrinkGraph gr = case nodes gr of
                   -- Need to have at least 2 nodes before we delete one!
                   ns@(_:_:_) -> map (`delNode` gr) ns
                   _          -> []

instance (Arbitrary a, Arbitrary b) => Arbitrary (T.Gr a b) where
  arbitrary = arbitraryGraph

  shrink = shrinkGraph

instance (Arbitrary a, Arbitrary b) => Arbitrary (P.Gr a b) where
  arbitrary = arbitraryGraph

  shrink = shrinkGraph

-- | A newtype wrapper to generate a graph without multiple edges
--   (loops allowed).
newtype NoMultipleEdges gr a b = NME { nmeGraph :: gr a b }
                                 deriving (Eq, Show, Read)

instance (Graph gr, Arbitrary a, Arbitrary b) => Arbitrary (NoMultipleEdges gr a b) where
  arbitrary = NME <$> arbitraryGraphWith (uniqBy toEdge)

  shrink = map NME . shrinkGraph . nmeGraph

-- | A newtype wrapper to generate a graph without multiple edges and
--   no loops.
newtype SimpleGraph gr a b = SG { simpleGraph :: gr a b }
                             deriving (Eq, Show, Read)

instance (Graph gr, Arbitrary a, Arbitrary b) => Arbitrary (SimpleGraph gr a b) where
  arbitrary = SG <$> arbitraryGraphWith (filter notLoop . uniqBy toEdge)

  shrink = map SG . shrinkGraph . simpleGraph

notLoop :: LEdge b -> Bool
notLoop (v,w,_) = v /= w
