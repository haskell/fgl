{- |
   Module      : Data.Graph.Inductive.Query.SCC
   Description : Finds all strongly connected components.
   Copyright   : (c) Gabriel Hjort Blindell <gabriel.hjort.blindell@gmail.com>
   License     : BSD3
-}

module Data.Graph.Inductive.Query.SCC
  ( strongComponentsOf )
where

import Data.Graph.Inductive.Graph

import qualified Data.IntMap as M



-- | Node information (whether the node is on the stack, its index, and its low
-- link), which is used as part of 'SCCState'.
data SCCNodeInfo
  = SCCNodeInfo
      { sccIsNodeOnStack :: Bool
      , sccNodeIndex :: Int
      , sccNodeLowLink :: Int
      }
  deriving (Show, Read, Eq)

-- | Contains the necessary data structures used by 'strongComponentsOf'.
data SCCState g a b
  = SCCState
      { sccComponents :: [g a b]
        -- ^ The components found so far.
      , sccCurrentIndex :: Int
        -- ^ The current index.
      , sccStack :: [Node]
        -- ^ The node stack.
      , sccNodeInfo :: M.IntMap SCCNodeInfo
        -- ^ Node information.
      , sccGraph :: g a b
        -- ^ The input graph.
      }
  deriving (Show, Read, Eq)

-- | Find all strongly connected components of a graph. Returned list is sorted
-- in topological order.
--
-- Implements Tarjan's algorithm:
-- https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
strongComponentsOf :: (Graph g) => g a b -> [g a b]
strongComponentsOf g =
  sccComponents $
  foldr ( \n st ->
          let i = sccNodeIndex $ sccNodeInfo st M.! n
          in if i < 0 then findSCCFor n st else st
        )
        (mkInitSCCState g)
        (nodes g)

findSCCFor :: (Graph g) => Node -> SCCState g a b -> SCCState g a b
findSCCFor n st0 =
  let i = sccCurrentIndex st0
      st1 = st0 { sccCurrentIndex = i + 1
                , sccStack = (n:sccStack st0)
                , sccNodeInfo = M.insert n
                                         (SCCNodeInfo True i i)
                                         (sccNodeInfo st0)
                }
      g = sccGraph st1
      st2 = foldr computeLowLinks st1 (suc g n)
      ni = sccNodeInfo st2 M.! n
      index = sccNodeIndex ni
      lowlink = sccNodeLowLink ni
      st3 = if index == lowlink then produceSCC st2 else st2
  in st3
  where
    computeLowLinks m st
      | isIndexUndefined =
          let st' = findSCCFor m st
              ni = sccNodeInfo st' M.! n
              n_lowlink = sccNodeLowLink ni
              m_lowlink = sccNodeLowLink $ sccNodeInfo st' M.! m
              new_ni = ni { sccNodeLowLink = min n_lowlink m_lowlink }
          in st' { sccNodeInfo = M.insert n new_ni (sccNodeInfo st') }
      | isOnStack =
          let ni = sccNodeInfo st M.! n
              n_lowlink = sccNodeLowLink ni
              m_index = sccNodeIndex $ sccNodeInfo st M.! m
              new_ni = ni { sccNodeLowLink = min n_lowlink m_index }
          in st { sccNodeInfo = M.insert n new_ni (sccNodeInfo st) }
      | otherwise = st
      where isIndexUndefined = let i = sccNodeIndex $ (sccNodeInfo st) M.! m
                               in i < 0
            isOnStack = sccIsNodeOnStack $ (sccNodeInfo st) M.! m
    produceSCC st =
      let stack = sccStack st
          (p0, p1) = span (/= n) stack
          ns = (head p1:p0)
          lab_ns = filter (\(n', _) -> n' `elem` ns) $
                   labNodes $
                   sccGraph st
          new_stack = tail p1
          new_map = foldr ( \n' ni_map ->
                            let ni = ni_map M.! n'
                                new_ni = ni { sccIsNodeOnStack = False }
                            in M.insert n' new_ni ni_map
                          )
                          (sccNodeInfo st)
                          ns
          lab_es = filter (\(n', m', _) -> n' `elem` ns || m' `elem` ns) $
                   labEdges $
                   sccGraph st
          comp = mkGraph lab_ns lab_es
          new_cs = (comp:sccComponents st)
      in st { sccComponents = new_cs
            , sccStack = new_stack
            , sccNodeInfo = new_map
            }

mkInitSCCState :: (Graph g) => g a b -> SCCState g a b
mkInitSCCState g =
  let ns = nodes g
      init_ni = SCCNodeInfo False (-1) (-1)
  in SCCState { sccComponents = []
              , sccCurrentIndex = 0
              , sccStack = []
              , sccNodeInfo = M.fromList $ zip ns (repeat init_ni)
              , sccGraph = g
              }
