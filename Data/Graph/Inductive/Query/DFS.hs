-- (c) 2000 - 2005 by Martin Erwig [see file COPYRIGHT]
-- | Depth-First Search

module Data.Graph.Inductive.Query.DFS(
    CFun,
    dfs,dfs',dff,dff',
    dfsWith, dfsWith',dffWith,dffWith',
    xdfsWith,xdfWith,xdffWith,
    -- * Undirected DFS
    udfs,udfs',udff,udff',
    udffWith,udffWith',
    -- * Reverse DFS
    rdff,rdff',rdfs,rdfs',
    rdffWith,rdffWith',
    -- * Applications of DFS\/DFF
    topsort,topsort',scc,reachable,
    -- * Applications of UDFS\/UDFF
    components,noComponents,isConnected
) where

import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Graph
import Data.Tree

----------------------------------------------------------------------
-- DFS AND FRIENDS
----------------------------------------------------------------------

{-

  Classification of all 32 dfs functions:

    dfs-function ::= [direction]"df"structure["With"]["'"]
    direction  -->  "x" | "u" | "r"
    structure  -->  "s" | "f"

              |   structure
   direction  |   "s"   "f"
   ------------------------   + optional With + optional '
      "x"     | xdfs  xdff
      " "     |  dfs   dff
      "u"     | udfs  udff
      "r"     | rdfs  rdff
   ------------------------

  Direction Parameter
  -------------------
   x : parameterized by a function that specifies which nodes
       to be visited next

  " ": the "normal case: just follow successors

   u : undirected, ie, follow predecesors and successors

   r : reverse, ie, follow predecesors


  Structure Parameter
  -------------------
   s : result is a list of
        (a) objects computed from visited contexts  ("With"-version)
        (b) nodes                                   (normal version)

   f : result is a tree/forest of
        (a) objects computed from visited contexts  ("With"-version)
        (b) nodes                                   (normal version)

  Optional Suffixes
  -----------------
   With : objects to be put into list/tree are given by a function
          on contexts, default for non-"With" versions: nodes

   '    : parameter node list is given implicitly by the nodes of the
          graph to be traversed, default for non-"'" versions: nodes
          must be provided explicitly


  Defined are only the following 22 most frabjuous function versions:

    xdfsWith
     dfsWith,dfsWith',dfs,dfs'
     udfs,udfs'
     rdfs,rdfs'
    xdffWith
     dffWith,dffWith',dff,dff'
     udffWith,udffWith',udff,udff'
     rdffWith,rdffWith',rdff,rdff'

  Others can be added quite easily if needed.

-}

-- fixNodes fixes the nodes of the graph as a parameter
--
fixNodes :: (Graph gr) => ([Node] -> gr a b -> c) -> gr a b -> c
fixNodes f g = f (nodes g) g


-- generalized depth-first search
--  (could also be simply defined as applying preorderF to the
--   result of xdffWith)
--
type CFun a b c = Context a b -> c

xdfsWith :: (Graph gr) => CFun a b [Node] -> CFun a b c -> [Node] -> gr a b -> [c]
xdfsWith _ _ []     _             = []
xdfsWith _ _ _      g | isEmpty g = []
xdfsWith d f (v:vs) g = case match v g of
                         (Just c,g')  -> f c:xdfsWith d f (d c++vs) g'
                         (Nothing,g') -> xdfsWith d f vs g'


-- dfs
--
dfsWith :: (Graph gr) => CFun a b c -> [Node] -> gr a b -> [c]
dfsWith = xdfsWith suc'

dfsWith' :: (Graph gr) => CFun a b c -> gr a b -> [c]
dfsWith' f = fixNodes (dfsWith f)

dfs :: (Graph gr) => [Node] -> gr a b -> [Node]
dfs = dfsWith node'

dfs' :: (Graph gr) => gr a b -> [Node]
dfs' = dfsWith' node'


-- undirected dfs, ie, ignore edge directions
--
udfs :: (Graph gr) => [Node] -> gr a b -> [Node]
udfs = xdfsWith neighbors' node'

udfs' :: (Graph gr) => gr a b -> [Node]
udfs' = fixNodes udfs


-- reverse dfs, ie, follow predecessors
--
rdfs :: (Graph gr) => [Node] -> gr a b -> [Node]
rdfs = xdfsWith pre' node'

rdfs' :: (Graph gr) => gr a b -> [Node]
rdfs' = fixNodes rdfs


-- generalized depth-first forest
--
xdfWith :: (Graph gr) => CFun a b [Node] -> CFun a b c -> [Node] -> gr a b -> ([Tree c],gr a b)
xdfWith _ _ []     g             = ([],g)
xdfWith _ _ _      g | isEmpty g = ([],g)
xdfWith d f (v:vs) g = case match v g of
                        (Nothing,g1) -> xdfWith d f vs g1
                        (Just c,g1)  -> (Node (f c) ts:ts',g3)
                                 where (ts,g2)  = xdfWith d f (d c) g1
                                       (ts',g3) = xdfWith d f vs g2

xdffWith :: (Graph gr) => CFun a b [Node] -> CFun a b c -> [Node] -> gr a b -> [Tree c]
xdffWith d f vs g = fst (xdfWith d f vs g)


-- dff
--
dffWith :: (Graph gr) => CFun a b c -> [Node] -> gr a b -> [Tree c]
dffWith = xdffWith suc'

dffWith' :: (Graph gr) => CFun a b c -> gr a b -> [Tree c]
dffWith' f = fixNodes (dffWith f)

dff :: (Graph gr) => [Node] -> gr a b -> [Tree Node]
dff = dffWith node'

dff' :: (Graph gr) => gr a b -> [Tree Node]
dff' = dffWith' node'


-- undirected dff
--
udffWith :: (Graph gr) => CFun a b c -> [Node] -> gr a b -> [Tree c]
udffWith = xdffWith neighbors'

udffWith' :: (Graph gr) => CFun a b c -> gr a b -> [Tree c]
udffWith' f = fixNodes (udffWith f)

udff :: (Graph gr) => [Node] -> gr a b -> [Tree Node]
udff = udffWith node'

udff' :: (Graph gr) => gr a b -> [Tree Node]
udff' = udffWith' node'


-- reverse dff, ie, following predecessors
--
rdffWith :: (Graph gr) => CFun a b c -> [Node] -> gr a b -> [Tree c]
rdffWith = xdffWith pre'

rdffWith' :: (Graph gr) => CFun a b c -> gr a b -> [Tree c]
rdffWith' f = fixNodes (rdffWith f)

rdff :: (Graph gr) => [Node] -> gr a b -> [Tree Node]
rdff = rdffWith node'

rdff' :: (Graph gr) => gr a b -> [Tree Node]
rdff' = rdffWith' node'


----------------------------------------------------------------------
-- ALGORITHMS BASED ON DFS
----------------------------------------------------------------------

components :: (Graph gr) => gr a b -> [[Node]]
components = (map preorder) . udff'

noComponents :: (Graph gr) => gr a b -> Int
noComponents = length . components

isConnected :: (Graph gr) => gr a b -> Bool
isConnected = (==1) . noComponents

postflatten :: Tree a -> [a]
postflatten (Node v ts) = postflattenF ts ++ [v]

postflattenF :: [Tree a] -> [a]
postflattenF = concatMap postflatten

topsort :: (Graph gr) => gr a b -> [Node]
topsort = reverse . postflattenF . dff'

topsort' :: (Graph gr) => gr a b -> [a]
topsort' = reverse . postorderF . (dffWith' lab')

scc :: (Graph gr) => gr a b -> [[Node]]
scc g = map preorder (rdff (topsort g) g)            -- optimized, using rdff
-- sccOrig g = map preorder (dff (topsort g) (grev g))  -- original by Sharir

reachable :: (Graph gr) => Node -> gr a b -> [Node]
reachable v g = preorderF (dff [v] g)
