-- Implemented by Gabriel Hjort Blindell <gabriel.hjort.blindell@gmail.com>

module Data.Graph.Inductive.Query.Cycles
  ( cyclesIn
  , cyclesIn'
  , strongComponentsOf
  , uniqueCycles
  , uniqueCycles'
  )
where

import Data.Graph.Inductive.Graph

import Data.List
  ( (\\)
  , delete
  , tails
  )
import Data.Maybe
  ( fromJust )
import Control.Monad
  ( ap )
import qualified Data.Map as M



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



-- | Contains the necessary data structures used by 'strongComponentsOf'.
data SCCState g a b
  = SCCState
      { sccComponents :: [g a b]
        -- ^ The components found so far.
      , sccCurrentIndex :: Int
        -- ^ The current index.
      , sccStack :: [Node]
        -- ^ The node stack.
      , sccNodeInfo :: M.Map Node (Bool, Int, Int)
        -- ^ Node information as a tuple (whether the node is on the stack, its
        -- index, and its low link).
      , sccGraph :: g a b
        -- ^ The input graph.
      }

-- | Find all strongly connected components of a graph. Implements Tarjan's
-- algorithm. Returned list is sorted in topological order.
strongComponentsOf :: (DynGraph g) => g a b -> [g a b]
strongComponentsOf g =
  sccComponents $
  foldr ( \n st ->
          let (_, i, _) = sccNodeInfo st M.! n
          in if i < 0 then findSCCFor n st else st
        )
        (mkInitSCCState g)
        (nodes g)

findSCCFor :: (DynGraph g) => Node -> SCCState g a b -> SCCState g a b
findSCCFor n st0 =
  let i = sccCurrentIndex st0
      st1 = st0 { sccCurrentIndex = i + 1
                , sccStack = (n:sccStack st0)
                , sccNodeInfo = M.insert n (True, i, i) (sccNodeInfo st0)
                }
      g = sccGraph st1
      st2 = foldr ( \m st ->
                    let st_ni = sccNodeInfo st
                        (m_on_stack, m_index, _) = st_ni M.! m
                    in if m_index < 0
                       then let st' = findSCCFor m st
                                st_ni' = sccNodeInfo st'
                                (n_on_stack', n_index', n_lowlink') =
                                  st_ni' M.! n
                                (_, _, m_lowlink) = st_ni' M.! m
                                new_n_ni = ( n_on_stack'
                                           , n_index'
                                           , min n_lowlink' m_lowlink
                                           )
                            in st' { sccNodeInfo =
                                       M.insert n new_n_ni st_ni'
                                   }
                       else if m_on_stack
                            then let (n_on_stack', n_index', n_lowlink') =
                                       st_ni M.! n
                                     new_n_ni = ( n_on_stack'
                                                , n_index'
                                                , min n_lowlink' m_index
                                                )
                                 in st { sccNodeInfo =
                                           M.insert n new_n_ni st_ni
                                       }
                            else st
                  )
                  st1
                  (suc g n)
      (_, n_index, n_lowlink) = sccNodeInfo st2 M.! n
      st3 = if n_index == n_lowlink
            then let stack = sccStack st2
                     (p0, p1) = span (/= n) stack
                     comp_ns = (head p1:p0)
                     new_stack = tail p1
                     new_ni = foldr ( \n' ni ->
                                      let (_, n_index', n_lowlink') = ni M.! n'
                                          new_n_ni = ( False
                                                     , n_index'
                                                     , n_lowlink'
                                                     )
                                      in M.insert n' new_n_ni ni
                                    )
                                    (sccNodeInfo st2)
                                    comp_ns
                     comp = nfilter (`elem` comp_ns) (sccGraph st2)
                     new_cs = (comp:sccComponents st2)
                 in st2 { sccComponents = new_cs
                        , sccStack = new_stack
                        , sccNodeInfo = new_ni
                        }
            else st2
  in st3

mkInitSCCState :: (DynGraph g) => g a b -> SCCState g a b
mkInitSCCState g =
  let ns = nodes g
  in SCCState { sccComponents = []
              , sccCurrentIndex = 0
              , sccStack = []
              , sccNodeInfo = M.fromList $ zip ns (repeat (False, -1, -1))
              , sccGraph = g
              }

-- | Contains the necessary data structures used by 'cyclesIn'.
data CyclesInState g a b
  = CyclesInState
      { cisCycles :: [[Node]]
        -- ^ The cycles found so far, in topological order.
      , cisBlocked :: M.Map Node Bool
        -- ^ The nodes which are currently blocked.
      , cisBlockMap :: M.Map Node [Node]
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

-- | Finds all cycles in a given graph using Johnson's algorithm.
--
-- See Donald B. Johnson: Finding All the Elementary Circuits of a Directed
-- Graph. SIAM Journal on Computing. Volumne 4, Nr. 1 (1975), pp. 77-84.
cyclesIn :: (DynGraph g) => g a b -> [[LNode a]]
cyclesIn g = map (addLabels g) (cyclesIn' g)

-- | Finds all cycles in a given graph using Johnson's algorithm.
--
-- See Donald B. Johnson: Finding All the Elementary Circuits of a Directed
-- Graph. SIAM Journal on Computing. Volumne 4, Nr. 1 (1975), pp. 77-84.
cyclesIn' :: (DynGraph g) => g a b -> [[Node]]
cyclesIn' g =
  cisCycles $
  foldr cyclesFor (mkInitCyclesInState g) (nodes g)

-- | Find all cycles in the given graph, excluding those that are also cliques.
uniqueCycles   :: (DynGraph g) => g a b -> [[LNode a]]
uniqueCycles g = map (addLabels g) (uniqueCycles' g)

-- | Find all cycles in the given graph, excluding those that are also cliques.
uniqueCycles'   :: (DynGraph g) => g a b -> [[Node]]
uniqueCycles' g = filter (not . isRegular g) (cyclesIn' g)

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
                    if cisBlocked st M.! m
                    then cUnblock m st
                    else st
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
