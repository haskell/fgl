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
       , shrinkGraph
       ) where

import           Data.Graph.Inductive.Graph        (Graph, Node, delNode,
                                                    mkGraph, nodes)
import qualified Data.Graph.Inductive.PatriciaTree as P
import qualified Data.Graph.Inductive.Tree         as T

import Test.QuickCheck (Arbitrary (..), Gen, elements, listOf, suchThat)

import Control.Applicative (liftA3)
import Data.List           (group, sort)

-- -----------------------------------------------------------------------------

-- | Generate an arbitrary graph.  Multiple edges are allowed.
arbitraryGraph :: (Graph gr, Arbitrary a, Arbitrary b) => Gen (gr a b)
arbitraryGraph = do ns <- suchThat genNs (not . null)
                    let nGen = elements ns
                    lns <- mapM makeLNode ns
                    les <- listOf $ makeLEdge nGen
                    return $ mkGraph lns les
  where
    genNs = fmap uniq arbitrary
    makeLNode n = fmap ((,) n) arbitrary
    makeLEdge nGen = liftA3 (,,) nGen nGen arbitrary

-- Ensure we have a list of unique Node values; this will also sort
-- the list, but that shouldn't matter.
uniq :: [Node] -> [Node]
uniq = map head . group . sort

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
