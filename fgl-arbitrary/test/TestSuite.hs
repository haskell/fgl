{- |
   Module      : TestSuite
   Description : fgl-arbitrary test suite
   Copyright   : Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Main where

import Data.Graph.Inductive.Arbitrary
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.Set  as S
import           Data.Word (Word8)

-- -----------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  propShrink "nodes and edges" prop_gnes
  propShrink "connectivity"    is_connected

-- Also ensure the shrink implementations are valid.
propShrink :: (Arbitrary a, Show a) => String -> (a -> Bool) -> Spec
propShrink nm f = prop nm f'
  where
    f' a = all f (a : shrink a)

-- -----------------------------------------------------------------------------

prop_gnes :: GraphNodesEdges NLabel ELabel -> Bool
prop_gnes (GNEs lns les) = uniqueNs && validEs
  where
    uniqueNs = length ns == S.size nsS
    validEs = all (`S.member` nsS) ens

    ns = map fst lns
    nsS = S.fromList ns

    ens = concatMap (\(v,w,_) -> [v,w]) les

is_connected :: Connected Gr NLabel ELabel -> Bool
is_connected cg
  | isEmpty g = True
  | otherwise = go S.empty (S.singleton . node' . fst . matchAny $ g)
  where
    g = connGraph cg
    ns = S.fromList (nodes g)

    go vis cnd
      | S.null cnd = vis == ns
      | otherwise  = go vis'
                     . S.unions
                     . map ((`S.difference`vis') . S.fromList . neighbors g)
                     . S.toList
                     $ cnd
      where
        vis' = vis `S.union` cnd

-- -----------------------------------------------------------------------------
-- Rather than defining proxies for this test-suite, just pre-define
-- the node and edge label types.

type NLabel = Char
type ELabel = Word8
