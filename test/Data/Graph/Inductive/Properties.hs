{- |
   Module      : Data.Graph.Inductive.Properties
   Description : Expected properties of inductive graphs
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Data.Graph.Inductive.Properties where

import Data.Graph.Inductive
import Data.Graph.Inductive.Arbitrary
import Data.Graph.Inductive.Proxy

import Test.QuickCheck

import Control.Arrow ((***))
import Data.Function (on)
import Data.Functor  ((<$>))
import Data.List     (groupBy, sort, sortBy)

-- -----------------------------------------------------------------------------
-- Non-dynamic graphs

-- | Ensure that a custom 'Eq' instance matches the behaviour of the
--   'equal' function.
valid_Eq :: (Graph gr, Eq a, Eq b, Eq (gr a b)) => gr a b -> Bool
valid_Eq g = equal g g && g == g

-- | Ensure that the definition of 'noNodes' matches the default
--   implementation.
valid_node_count :: (Graph gr) => gr a b -> Bool
valid_node_count g = noNodes g == length (nodes g)

-- | Ensure that the definition of 'nodeRange' matches the default
--   implementation.
valid_nodeRange :: (Graph gr) => gr a b -> Property
valid_nodeRange g = not (isEmpty g) ==>
                        nodeRange g == (minimum vs, maximum vs)
  where
    vs = nodes g

-- | Make sure that a graph created with specified nodes contains
--   those nodes (and only those nodes) and no edges are created.
valid_mkGraph_nodes :: (Graph gr, Arbitrary a, Eq a) => Proxy (gr a b) -> Gen Bool
valid_mkGraph_nodes p = do ns <- arbitraryNodes
                           let g = mkGraph ns [] `asProxyTypeOf` p
                           return ( sortOn fst (labNodes g) == ns
                                    && null (labEdges g))

-- | Make sure that a graph created with specified edges contains
--   those edges (and only those edges), and that no additional nodes
--   are created.
valid_mkGraph_edges :: (Graph gr, Eq a, Eq b) => Proxy (gr a b)
                       -> GraphNodesEdges a b  -> Bool
valid_mkGraph_edges p (GNEs ns es) = sortOn toEdge (labEdges g) == es'
                                     && sortOn fst (labNodes g) == ns
  where
    es' = uniqBy toEdge es

    g = mkGraph ns es' `asProxyTypeOf` p

-- | Ensure that when a node is matched, it is indeed removed from the
--   resulting graph.
valid_match :: (Graph gr) => gr a b -> Gen Bool
valid_match g = check_match <$> elements (nodes g)
  where
    order = noNodes g

    check_match n = maybe False check_context mc
      where
        (mc, g') = match n g

        check_context c = (node' c `notElem` nodes g')
                          && (noNodes g' == order - 1)
                          -- Edges were previously in the graph
                          && all (elem (node' c) . pre g) (sucC c)
                          && all (elem (node' c) . suc g) (preC c)
                          -- Edges not in new graph
                          && all (notElem (node' c) . pre g') (sucC c)
                          && all (notElem (node' c) . suc g') (preC c)

-- | Ensure that 'matchAny' is valid by verifying that it achieves the
--   same result as matching for that node specifically.
valid_matchAny :: (Graph gr, Eq a, Ord b) => gr a b -> Property
valid_matchAny g = not (isEmpty g) ==>
                     (uncurry (&&)
                      . (maybe False ((c'==) . sortContext) *** (equal g'))
                       $ match (node' c) g)
  where
    (c,g') = matchAny g

    c' = sortContext c

-- -----------------------------------------------------------------------------
-- Dynamic graphs

-- | Ensure that matching and then merging using '&' produces the
--   original graph again.
--
--   We do it this way because it isn't possible to generate an
--   arbitrary 'Context' to test against; 'valid_match' \"proves\"
--   that matching is valid, so if merging produces the original graph
--   again then it must be valid as well.
valid_merge :: (DynGraph gr, Eq a, Eq b) => gr a b -> Bool
valid_merge g = all check_merge (nodes g)
  where
    -- Using equal here rather than requiring an Eq instance.
    check_merge n = maybe False (equal g . (&g')) mc
      where
        (mc, g') = match n g

-- -----------------------------------------------------------------------------

esToNs :: [LEdge b] -> [Node]
esToNs = uniqBy id . concatMap (\(v,w,_) -> [v,w])

uniqBy :: (Ord b) => (a -> b) -> [a] -> [a]
uniqBy f = map head . groupBy ((==) `on` f) . sortOn f

sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)

-- | As with suc', but also remove any loops
sucC :: Context a b -> [Node]
sucC c = filter (/= (node' c)) (suc' c)

-- | As with pre', but also remove any loops
preC :: Context a b -> [Node]
preC c = filter (/= (node' c)) (pre' c)

-- In case a Context is produced with the Adj lists in different
-- orders, sort them so that they can then be equality tested.
sortContext :: (Ord b) => Context a b -> Context a b
sortContext (p,v,l,s) = (sort p, v, l, sort s)
