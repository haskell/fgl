-- Implemented by Gabriel Hjort Blindell <gabriel.hjort.blindell@gmail.com>

module Data.Graph.Inductive.Query.Cycles
  ( cycles
  , cycles'
  , uniqueCycles
  , uniqueCycles'
  )
where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.SCC

import Data.List ((\\), delete, tails)
import Data.Maybe (fromJust)
import Control.Monad (ap)
import qualified Data.IntMap as M



-- The following functions were copied from the Graphalyze package.

-- | Obtain the labels for a list of 'Node's. It is assumed that each 'Node' is
-- indeed present in the given graph.
addLabels :: (Graph g) => g a b -> [Node] -> [LNode a]
addLabels gr = map (ap (,) (fromJust . lab gr))

twoCycle      :: (Graph g) => g a b -> Node -> [Node]
twoCycle gr n = filter (elem n . suc gr) (delete n $ suc gr n)

-- | Determines if the list of nodes represents a regular subgraph.
isRegular :: (Graph g) => g a b -> [Node] -> Bool
isRegular g ns = all allTwoCycle split
    where
      -- Node + Rest of list
      split = zip ns tns'
      tns' = tail $ tails ns
      allTwoCycle (n,rs) = null $ rs \\ twoCycle g n

-- End of copied functions.



-- | Contains the necessary data structures used by 'cycles'.
data CyclesInState g a b
  = CyclesInState
      { cisCycles :: [[Node]]
        -- ^ The cycles found so far, in topological order.
      , cisBlocked :: M.IntMap Bool
        -- ^ The nodes which are currently blocked.
      , cisBlockMap :: M.IntMap [Node]
        -- ^ The B set.
      , cisStack :: [Node]
        -- ^ The node stack.
      , cisS :: Maybe Node
        -- ^ The current S value.
      , cisCurrentComp :: Maybe (g a b)
        -- ^ The component currently being processed.
      , cisComponents :: [g a b]
        -- ^ The components of the input graph.
      , cisGraph :: g a b
        -- ^ The input graph.
      }
  deriving (Show, Read, Eq)

-- | Finds all cycles in a given graph using Johnson's algorithm.
--
-- See Donald B. Johnson: Finding All the Elementary Circuits of a Directed
-- Graph. SIAM Journal on Computing. Volumne 4, Nr. 1 (1975), pp. 77-84.
cycles :: (DynGraph g) => g a b -> [[LNode a]]
cycles g = map (addLabels g) (cycles' g)

-- | Finds all cycles in a given graph using Johnson's algorithm.
--
-- See Donald B. Johnson: Finding All the Elementary Circuits of a Directed
-- Graph. SIAM Journal on Computing. Volumne 4, Nr. 1 (1975), pp. 77-84.
cycles' :: (DynGraph g) => g a b -> [[Node]]
cycles' g =
  cisCycles $
  foldr cyclesFor (mkInitCyclesInState g) (nodes g)

-- | Find all cycles in the given graph, excluding those that are also cliques.
uniqueCycles   :: (DynGraph g) => g a b -> [[LNode a]]
uniqueCycles g = map (addLabels g) (uniqueCycles' g)

-- | Find all cycles in the given graph, excluding those that are also cliques.
uniqueCycles'   :: (DynGraph g) => g a b -> [[Node]]
uniqueCycles' g = filter (not . isRegular g) (cycles' g)

cyclesFor :: (DynGraph g) => Node -> CyclesInState g a b -> CyclesInState g a b
cyclesFor n st0 =
  let n_comp = head $
               filter (\c -> n `gelem` c) $
               cisComponents st0
  in if noNodes n_comp > 1
     then let st1 = st0 { cisS = Just n
                        , cisCurrentComp = Just n_comp
                        }
              st2 = fst $ cCircuits n st1
              g = cisGraph st2
              new_g = delNode n g
              new_comps = strongComponentsOf new_g
              st3 = st2 { cisGraph = new_g
                        , cisComponents = new_comps
                        }
          in st3
     else st0 -- Skip to next node

cCircuits :: (DynGraph g) => Node -> CyclesInState g a b ->
             (CyclesInState g a b, Bool)
cCircuits n st0 =
  let st1 = st0 { cisBlocked = M.insert n True (cisBlocked st0)
                , cisStack = (n:cisStack st0)
                }
      c = fromJust $ cisCurrentComp st1
      n_suc = suc c n
      (st2, f) =
        foldr ( \m (st, f') ->
                if m == fromJust (cisS st)
                then let new_cycle = reverse (m:cisStack st)
                         st' = st { cisCycles = (new_cycle:cisCycles st) }
                     in (st', True)
                else if not (cisBlocked st M.! m)
                     then let (st', f'') = cCircuits m st
                          in (st', f' || f'')
                     else (st, f')
              )
              (st1, False)
              n_suc
      st3 = if f
            then cUnblock n st2
            else foldr ( \m st ->
                         let bm = cisBlockMap st
                             m_blocked = bm M.! m
                             new_m_blocked = (n:m_blocked)
                         in if n `notElem` m_blocked
                            then st { cisBlockMap =
                                        M.insert m new_m_blocked bm
                                    }
                            else st
                       )
                       st2
                       n_suc
      st4 = st3 { cisStack = tail $ cisStack st3 }
  in (st4, f)

cUnblock :: (DynGraph g) => Node -> CyclesInState g a b -> CyclesInState g a b
cUnblock n st0 =
  let n_blocked = cisBlockMap st0 M.! n
      st1 = st0 { cisBlocked = M.insert n False (cisBlocked st0)
                , cisBlockMap = M.insert n [] (cisBlockMap st0)
                }
      st2 = foldr ( \m st ->
                    if cisBlocked st M.! m then cUnblock m st else st
                  )
                  st1
                  n_blocked
  in st2

mkInitCyclesInState :: (DynGraph g) => g a b -> CyclesInState g a b
mkInitCyclesInState g =
  let ns = nodes g
  in CyclesInState { cisCycles = []
                   , cisBlocked = M.fromList $ zip ns (repeat False)
                   , cisBlockMap = M.fromList $ zip ns (repeat [])
                   , cisStack = []
                   , cisS = Nothing
                   , cisCurrentComp = Nothing
                   , cisComponents = strongComponentsOf g
                   , cisGraph = g
                   }
