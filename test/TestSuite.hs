{- |
   Module      : TestSuite
   Description : fgl test suite
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Main where

import Data.Graph.Inductive.Arbitrary ()
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Proxy

import Test.Hspec
import Test.Hspec.QuickCheck

-- -----------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  graphTests "Tree Graphs"         (Proxy :: TreeP)
  graphTests "PatriciaTree Graphs" (Proxy :: PatriciaTreeP)

-- -----------------------------------------------------------------------------

graphTests :: (Graph gr) => String -> GraphProxy gr -> Spec
graphTests nm p = describe nm $ do
  return ()

-- -----------------------------------------------------------------------------
