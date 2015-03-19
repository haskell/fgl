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
       ) where

import           Data.Graph.Inductive.Graph        (Edge, Graph, LEdge, Node,
                                                    delNode, empty, mkGraph,
                                                    nodes)
import qualified Data.Graph.Inductive.PatriciaTree as P
import qualified Data.Graph.Inductive.Tree         as T

import Test.QuickCheck (Arbitrary (..), Gen, elements, listOf)

import Control.Applicative (liftA3)
import Data.Function       (on)
import Data.Functor        ((<$>))
import Data.List           (groupBy, sortBy)

-- -----------------------------------------------------------------------------

-- | Generate an arbitrary graph.  Multiple edges are allowed.
arbitraryGraph :: (Graph gr, Arbitrary a, Arbitrary b) => Gen (gr a b)
arbitraryGraph = arbitraryGraphWith id

-- | Generate an arbitrary graph, using the specified function to
--   manipulate the generated list of edges (e.g. remove multiple
--   edges).
arbitraryGraphWith :: (Graph gr, Arbitrary a, Arbitrary b)
                      => ([LEdge b] -> [LEdge b]) -> Gen (gr a b)
arbitraryGraphWith f = do ns <- genNs
                          if null ns
                             then return empty
                             else do let nGen = elements ns
                                     lns <- mapM makeLNode ns
                                     les <- fmap f . listOf $ makeLEdge nGen
                                     return $ mkGraph lns les
  where
    genNs = fmap uniq arbitrary
    makeLNode n = fmap ((,) n) arbitrary
    makeLEdge nGen = liftA3 (,,) nGen nGen arbitrary

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

toEdge :: LEdge b -> Edge
toEdge (v,w,_) = (v,w)

notLoop :: LEdge b -> Bool
notLoop (v,w,_) = v /= w
