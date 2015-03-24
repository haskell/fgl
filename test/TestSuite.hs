{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

{- |
   Module      : TestSuite
   Description : fgl test suite
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Main where

import Data.Graph.Inductive.Arbitrary  ()
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Properties
import Data.Graph.Inductive.Proxy

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck       (Arbitrary, Testable)

-- -----------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  graphTests "Tree Graphs"         (Proxy :: TreeP)
  graphTests "PatriciaTree Graphs" (Proxy :: PatriciaTreeP)
  describe "Miscellaneous" $ do
    prop "edge projections" (edge_projections :: LEdge Char -> Bool)

-- -----------------------------------------------------------------------------

-- | Run all available tests on the specified graph type.  Requires
--   multiple edges and loops to be permissible.
graphTests :: forall gr. (DynGraph gr, Eq (GraphType gr), Arbitrary (GraphType gr), Show (GraphType gr))
               => String -> GraphProxy gr -> Spec
graphTests nm p = describe nm $ do
  describe "Static tests" $ do
    propType  "Eq instance"     valid_Eq
    propType  "node count"      valid_node_count
    propType  "nodeRange"       valid_nodeRange
    proxyProp "mkGraph (nodes)" valid_mkGraph_nodes
    proxyProp "mkGraph (edges)" valid_mkGraph_edges
    propType  "match"           valid_match
    propType  "matchAny"        valid_matchAny
    propType  "newNodes"        newNodes_really_new
    propType  "ufold (nodes)"   ufold_all_nodes
    propType  "gelem"           all_nodes_gelem
    propType  "gelem vs nodes"  gelem_in_nodes

  describe "Dynamic tests" $ do
    propType  "merging (&)"     valid_merge
    propType  "gmap (id)"       gmap_id
    propType  "insNode"         valid_insNode
    propType  "insNodes"        valid_insNodes
    propType  "insEdge"         valid_insEdge
    propType  "insEdges"        valid_insEdges
    propType  "delNode"         valid_delNode
    propType  "delNodes"        valid_delNodes
    propType  "delEdge"         valid_delEdge
    propType  "delEdges"        valid_delEdges
    propType  "delLEdge"        valid_delLEdge
    propType  "delAllLEdge"     valid_delAllLEdge
    proxyProp "valid_mkGraph"   valid_mkGraph
    propType  "valid_buildGr"   valid_buildGr

  where
    proxyProp str = prop str . ($p)

    propType :: (Testable pr) => String -> (GraphType gr -> pr) -> Spec
    propType = prop

-- -----------------------------------------------------------------------------
