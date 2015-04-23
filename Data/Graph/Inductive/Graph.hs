-- (c) 1999-2005 by Martin Erwig [see file COPYRIGHT]
-- | Static and Dynamic Inductive Graphs
module Data.Graph.Inductive.Graph (
    -- * General Type Defintions
    -- ** Node and Edge Types
    Node,LNode,UNode,
    Edge,LEdge,UEdge,
    -- ** Types Supporting Inductive Graph View
    Adj,Context,MContext,Decomp,GDecomp,UContext,UDecomp,
    Path,LPath(..),UPath,
    -- * Graph Type Classes
    -- | We define two graph classes:
    --
    --   Graph: static, decomposable graphs.
    --    Static means that a graph itself cannot be changed
    --
    --   DynGraph: dynamic, extensible graphs.
    --             Dynamic graphs inherit all operations from static graphs
    --             but also offer operations to extend and change graphs.
    --
    -- Each class contains in addition to its essential operations those
    -- derived operations that might be overwritten by a more efficient
    -- implementation in an instance definition.
    --
    -- Note that labNodes is essentially needed because the default definition
    -- for matchAny is based on it: we need some node from the graph to define
    -- matchAny in terms of match. Alternatively, we could have made matchAny
    -- essential and have labNodes defined in terms of ufold and matchAny.
    -- However, in general, labNodes seems to be (at least) as easy to define
    -- as matchAny. We have chosen labNodes instead of the function nodes since
    -- nodes can be easily derived from labNodes, but not vice versa.
    Graph(..),
    DynGraph(..),
    -- * Operations
    -- ** Graph Folds and Maps
    ufold,gmap,nmap,emap,
    -- ** Graph Projection
    nodes,edges,toEdge,edgeLabel,toLEdge,newNodes,gelem,
    -- ** Graph Construction and Destruction
    insNode,insEdge,delNode,delEdge,delLEdge,delAllLEdge,
    insNodes,insEdges,delNodes,delEdges,
    buildGr,mkUGraph,
    -- ** Graph Inspection
    context,lab,neighbors,
    suc,pre,lsuc,lpre,
    out,inn,outdeg,indeg,deg,
    equal,
    -- ** Context Inspection
    node',lab',labNode',neighbors',
    suc',pre',lpre',lsuc',
    out',inn',outdeg',indeg',deg',
    -- * Pretty-printing
    prettify,
    prettyPrint
) where

import Control.Arrow (first)
import Data.Function (on)
import Data.List     (delete, foldl', groupBy, sortBy, (\\))
import Data.Maybe    (fromMaybe, isJust)

-- | Unlabeled node
type  Node   = Int
-- | Labeled node
type LNode a = (Node,a)
-- | Quasi-unlabeled node
type UNode   = LNode ()

-- | Unlabeled edge
type  Edge   = (Node,Node)
-- | Labeled edge
type LEdge b = (Node,Node,b)
-- | Quasi-unlabeled edge
type UEdge   = LEdge ()

-- | Unlabeled path
type Path    = [Node]
-- | Labeled path
newtype LPath a = LP [LNode a]

instance (Show a) => Show (LPath a) where
  show (LP xs) = show xs

instance (Eq a) => Eq (LPath a) where
  (LP [])        == (LP [])        = True
  (LP ((_,x):_)) == (LP ((_,y):_)) = x==y
  (LP _)         == (LP _)         = False

instance (Ord a) => Ord (LPath a) where
  compare (LP [])        (LP [])        = EQ
  compare (LP ((_,x):_)) (LP ((_,y):_)) = compare x y
  compare _ _ = error "LPath: cannot compare two empty paths"

-- | Quasi-unlabeled path
type UPath   = [UNode]

-- | Labeled links to or from a 'Node'.
type Adj b        = [(b,Node)]
-- | Links to the 'Node', the 'Node' itself, a label, links from the 'Node'.
type Context a b  = (Adj b,Node,a,Adj b) -- Context a b "=" Context' a b "+" Node
type MContext a b = Maybe (Context a b)
-- | 'Graph' decomposition - the context removed from a 'Graph', and the rest
-- of the 'Graph'.
type Decomp g a b = (MContext a b,g a b)
-- | The same as 'Decomp', only more sure of itself.
type GDecomp g a b  = (Context a b,g a b)

-- | Unlabeled context.
type UContext     = ([Node],Node,[Node])
-- | Unlabeled decomposition.
type UDecomp g    = (Maybe UContext,g)

-- | Minimum implementation: 'empty', 'isEmpty', 'match', 'mkGraph', 'labNodes'
class Graph gr where
  {-# MINIMAL empty, isEmpty, match, mkGraph, labNodes #-}

  -- | An empty 'Graph'.
  empty     :: gr a b

  -- | True if the given 'Graph' is empty.
  isEmpty   :: gr a b -> Bool

  -- | Decompose a 'Graph' into the 'MContext' found for the given node and the
  -- remaining 'Graph'.
  match     :: Node -> gr a b -> Decomp gr a b

  -- | Create a 'Graph' from the list of 'LNode's and 'LEdge's.
  --
  --   For graphs that are also instances of 'DynGraph', @mkGraph ns
  --   es@ should be equivalent to @('insEdges' es . 'insNodes' ns)
  --   'empty'@.
  mkGraph   :: [LNode a] -> [LEdge b] -> gr a b

  -- | A list of all 'LNode's in the 'Graph'.
  labNodes  :: gr a b -> [LNode a]

  -- | Decompose a graph into the 'Context' for an arbitrarily-chosen 'Node'
  -- and the remaining 'Graph'.
  matchAny  :: gr a b -> GDecomp gr a b
  matchAny g = case labNodes g of
                 []      -> error "Match Exception, Empty Graph"
                 (v,_):_ -> (c,g')
                   where
                     (Just c,g') = match v g

  -- | The number of 'Node's in a 'Graph'.
  noNodes   :: gr a b -> Int
  noNodes = length . labNodes

  -- | The minimum and maximum 'Node' in a 'Graph'.
  nodeRange :: gr a b -> (Node,Node)
  nodeRange g
    | isEmpty g = error "nodeRange of empty graph"
    | otherwise = (minimum vs, maximum vs)
    where
      vs = nodes g

  -- | A list of all 'LEdge's in the 'Graph'.
  labEdges  :: gr a b -> [LEdge b]
  labEdges = ufold (\(_,v,_,s)->((map (\(l,w)->(v,w,l)) s)++)) []

class (Graph gr) => DynGraph gr where
  -- | Merge the 'Context' into the 'DynGraph'.
  (&) :: Context a b -> gr a b -> gr a b

-- | Fold a function over the graph.
ufold :: (Graph gr) => ((Context a b) -> c -> c) -> c -> gr a b -> c
ufold f u g
  | isEmpty g = u
  | otherwise = f c (ufold f u g')
  where
    (c,g') = matchAny g

-- | Map a function over the graph.
gmap :: (DynGraph gr) => (Context a b -> Context c d) -> gr a b -> gr c d
gmap f = ufold (\c->(f c&)) empty

-- | Map a function over the 'Node' labels in a graph.
nmap :: (DynGraph gr) => (a -> c) -> gr a b -> gr c b
nmap f = gmap (\(p,v,l,s)->(p,v,f l,s))

-- | Map a function over the 'Edge' labels in a graph.
emap :: (DynGraph gr) => (b -> c) -> gr a b -> gr a c
emap f = gmap (\(p,v,l,s)->(map1 f p,v,l,map1 f s))
  where
    map1 g = map (first g)

-- | List all 'Node's in the 'Graph'.
nodes :: (Graph gr) => gr a b -> [Node]
nodes = map fst . labNodes

-- | List all 'Edge's in the 'Graph'.
edges :: (Graph gr) => gr a b -> [Edge]
edges = map toEdge . labEdges

-- | Drop the label component of an edge.
toEdge :: LEdge b -> Edge
toEdge (v,w,_) = (v,w)

-- | Add a label to an edge.
toLEdge :: Edge -> b -> LEdge b
toLEdge (v,w) l = (v,w,l)

-- | The label in an edge.
edgeLabel :: LEdge b -> b
edgeLabel (_,_,l) = l

-- | List N available 'Node's, i.e. 'Node's that are not used in the 'Graph'.
newNodes :: (Graph gr) => Int -> gr a b -> [Node]
newNodes i g
  | isEmpty g = [0..i-1]
  | otherwise = [n+1..n+i]
  where
    (_,n) = nodeRange g

-- | 'True' if the 'Node' is present in the 'Graph'.
gelem :: (Graph gr) => Node -> gr a b -> Bool
gelem v = isJust . fst . match v

-- | Insert a 'LNode' into the 'Graph'.
insNode :: (DynGraph gr) => LNode a -> gr a b -> gr a b
insNode (v,l) = (([],v,l,[])&)
{-# NOINLINE [0] insNode #-}

-- | Insert a 'LEdge' into the 'Graph'.
insEdge :: (DynGraph gr) => LEdge b -> gr a b -> gr a b
insEdge (v,w,l) g = (pr,v,la,(l,w):su) & g'
  where
    (Just (pr,_,la,su),g') = match v g

-- | Remove a 'Node' from the 'Graph'.
delNode :: (Graph gr) => Node -> gr a b -> gr a b
delNode v = delNodes [v]

-- | Remove an 'Edge' from the 'Graph'.
--
--   NOTE: in the case of multiple edges, this will delete /all/ such
--   edges from the graph as there is no way to distinguish between
--   them.  If you need to delete only a single such edge, please use
--   'delLEdge'.
delEdge :: (DynGraph gr) => Edge -> gr a b -> gr a b
delEdge (v,w) g = case match v g of
                    (Nothing,_)          -> g
                    (Just (p,v',l,s),g') -> (p,v',l,filter ((/=w).snd) s) & g'

-- | Remove an 'LEdge' from the 'Graph'.
--
--   NOTE: in the case of multiple edges with the same label, this
--   will only delete the /first/ such edge.  To delete all such
--   edges, please use 'delAllLedges'.
delLEdge :: (DynGraph gr, Eq b) => LEdge b -> gr a b -> gr a b
delLEdge = delLEdgeBy delete

-- | Remove all edges equal to the one specified.
delAllLEdge :: (DynGraph gr, Eq b) => LEdge b -> gr a b -> gr a b
delAllLEdge = delLEdgeBy (filter . (/=))

delLEdgeBy :: (DynGraph gr, Eq b) => ((b,Node) -> Adj b -> Adj b)
              -> LEdge b -> gr a b -> gr a b
delLEdgeBy f (v,w,b) g = case match v g of
                           (Nothing,_)          -> g
                           (Just (p,v',l,s),g') -> (p,v',l,f (b,w) s) & g'

-- | Insert multiple 'LNode's into the 'Graph'.
insNodes   :: (DynGraph gr) => [LNode a] -> gr a b -> gr a b
insNodes vs g = foldl' (flip insNode) g vs

-- | Insert multiple 'LEdge's into the 'Graph'.
insEdges :: (DynGraph gr) => [LEdge b] -> gr a b -> gr a b
insEdges es g = foldl' (flip insEdge) g es

-- | Remove multiple 'Node's from the 'Graph'.
delNodes :: (Graph gr) => [Node] -> gr a b -> gr a b
delNodes vs g = foldl' (snd .: flip match) g vs

-- | Remove multiple 'Edge's from the 'Graph'.
delEdges :: (DynGraph gr) => [Edge] -> gr a b -> gr a b
delEdges es g = foldl' (flip delEdge) g es

-- | Build a 'Graph' from a list of 'Context's.
--
--   The list should be in the order such that earlier 'Context's
--   depend upon later ones (i.e. as produced by @'ufold' (:) []@).
buildGr :: (DynGraph gr) => [Context a b] -> gr a b
buildGr = foldr (&) empty

-- | Build a quasi-unlabeled 'Graph'.
mkUGraph :: (Graph gr) => [Node] -> [Edge] -> gr () ()
mkUGraph vs es = mkGraph (labUNodes vs) (labUEdges es)
   where
     labUEdges = map (`toLEdge` ())
     labUNodes = map (flip (,) ())

-- | Find the context for the given 'Node'.  Causes an error if the 'Node' is
-- not present in the 'Graph'.
context :: (Graph gr) => gr a b -> Node -> Context a b
context g v = fromMaybe (error ("Match Exception, Node: "++show v))
                        (fst (match v g))

-- | Find the label for a 'Node'.
lab :: (Graph gr) => gr a b -> Node -> Maybe a
lab g v = fmap lab' . fst $ match v g

-- | Find the neighbors for a 'Node'.
neighbors :: (Graph gr) => gr a b -> Node -> [Node]
neighbors = maybe [] (\(p,_,_,s) -> map snd (p++s)) .: mcontext

-- | Find all 'Node's that have a link from the given 'Node'.
suc :: (Graph gr) => gr a b -> Node -> [Node]
suc = map snd .: context4l

-- | Find all 'Node's that link to to the given 'Node'.
pre :: (Graph gr) => gr a b -> Node -> [Node]
pre = map snd .: context1l

-- | Find all 'Node's that are linked from the given 'Node' and the label of
-- each link.
lsuc :: (Graph gr) => gr a b -> Node -> [(Node,b)]
lsuc = map flip2 .: context4l

-- | Find all 'Node's that link to the given 'Node' and the label of each link.
lpre :: (Graph gr) => gr a b -> Node -> [(Node,b)]
lpre = map flip2 .: context1l

-- | Find all outward-bound 'LEdge's for the given 'Node'.
out :: (Graph gr) => gr a b -> Node -> [LEdge b]
out g v = map (\(l,w)->(v,w,l)) (context4l g v)

-- | Find all inward-bound 'LEdge's for the given 'Node'.
inn :: (Graph gr) => gr a b -> Node -> [LEdge b]
inn g v = map (\(l,w)->(w,v,l)) (context1l g v)

-- | The outward-bound degree of the 'Node'.
outdeg :: (Graph gr) => gr a b -> Node -> Int
outdeg = length .: context4l

-- | The inward-bound degree of the 'Node'.
indeg :: (Graph gr) => gr a b -> Node -> Int
indeg  = length .: context1l

-- | The degree of the 'Node'.
deg :: (Graph gr) => gr a b -> Node -> Int
deg = deg' .: context

-- | The 'Node' in a 'Context'.
node' :: Context a b -> Node
node' (_,v,_,_) = v

-- | The label in a 'Context'.
lab' :: Context a b -> a
lab' (_,_,l,_) = l

-- | The 'LNode' from a 'Context'.
labNode' :: Context a b -> LNode a
labNode' (_,v,l,_) = (v,l)

-- | All 'Node's linked to or from in a 'Context'.
neighbors' :: Context a b -> [Node]
neighbors' (p,_,_,s) = map snd p++map snd s

-- | All 'Node's linked to in a 'Context'.
suc' :: Context a b -> [Node]
suc' = map snd . context4l'

-- | All 'Node's linked from in a 'Context'.
pre' :: Context a b -> [Node]
pre' = map snd . context1l'

-- | All 'Node's linked from in a 'Context', and the label of the links.
lsuc' :: Context a b -> [(Node,b)]
lsuc' = map flip2 . context4l'

-- | All 'Node's linked from in a 'Context', and the label of the links.
lpre' :: Context a b -> [(Node,b)]
lpre' = map flip2 . context1l'

-- | All outward-directed 'LEdge's in a 'Context'.
out' :: Context a b -> [LEdge b]
out' c@(_,v,_,_) = map (\(l,w)->(v,w,l)) (context4l' c)

-- | All inward-directed 'LEdge's in a 'Context'.
inn' :: Context a b -> [LEdge b]
inn' c@(_,v,_,_) = map (\(l,w)->(w,v,l)) (context1l' c)

-- | The outward degree of a 'Context'.
outdeg' :: Context a b -> Int
outdeg' = length . context4l'

-- | The inward degree of a 'Context'.
indeg' :: Context a b -> Int
indeg' = length . context1l'

-- | The degree of a 'Context'.
deg' :: Context a b -> Int
deg' (p,_,_,s) = length p+length s

----------------------------------------------------------------------
-- GRAPH EQUALITY
----------------------------------------------------------------------

slabNodes :: (Eq a,Graph gr) => gr a b -> [LNode a]
slabNodes = sortBy (compare `on` fst) . labNodes

glabEdges :: (Eq b, Graph gr) => gr a b -> [GroupEdges b]
glabEdges = map (GEs . groupLabels)
            . groupBy ((==) `on` toEdge)
            . sortBy (compare `on` toEdge)
            . labEdges
  where
    groupLabels les = toLEdge (toEdge (head les)) (map edgeLabel les)

equal :: (Eq a,Eq b,Graph gr) => gr a b -> gr a b -> Bool
equal g g' = slabNodes g == slabNodes g' && glabEdges g == glabEdges g'
-- This assumes that nodes aren't repeated (which shouldn't happen for
-- sane graph instances).  If node IDs are repeated, then the usage of
-- slabNodes cannot guarantee stable ordering.

-- Newtype wrapper just to test for equality of multiple edges.  This
-- is needed because without an Ord constraint on `b' it is not
-- possible to guarantee a stable ordering on edge labels.
newtype GroupEdges b = GEs (LEdge [b])
  deriving (Show, Read)

instance (Eq b) => Eq (GroupEdges b) where
  (GEs (v1,w1,bs1)) == (GEs (v2,w2,bs2)) = v1 == v2
                                           && w1 == w2
                                           && eqLists bs1 bs2

eqLists :: (Eq a) => [a] -> [a] -> Bool
eqLists xs ys = null (xs \\ ys) && null (ys \\ xs)
-- OK to use \\ here as we want each value in xs to cancel a *single*
-- value in ys.

----------------------------------------------------------------------
-- UTILITIES
----------------------------------------------------------------------

-- auxiliary functions used in the implementation of the
-- derived class members
--
(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
-- f .: g = \x y->f (g x y)
-- f .: g = (f .) . g
-- (.:) f = ((f .) .)
-- (.:) = (.) (.) (.)
(.:) = (.) . (.)

flip2 :: (a,b) -> (b,a)
flip2 (x,y) = (y,x)

-- projecting on context elements
--
context1l :: (Graph gr) => gr a b -> Node -> Adj b
context1l = maybe [] context1l' .: mcontext

context4l :: (Graph gr) => gr a b -> Node -> Adj b
context4l = maybe [] context4l' .: mcontext

mcontext :: (Graph gr) => gr a b -> Node -> MContext a b
mcontext = fst .: flip match

context1l' :: Context a b -> Adj b
context1l' (p,v,_,s) = p++filter ((==v).snd) s

context4l' :: Context a b -> Adj b
context4l' (p,v,_,s) = s++filter ((==v).snd) p

----------------------------------------------------------------------
-- PRETTY PRINTING
----------------------------------------------------------------------

-- | Pretty-print the graph.  Note that this loses a lot of
--   information, such as edge inverses, etc.
prettify :: (DynGraph gr, Show a, Show b) => gr a b -> String
prettify g = foldr (showsContext . context g) id (nodes g) ""
  where
    showsContext (_,n,l,s) sg = shows n . (':':) . shows l
                                . showString "->" . shows s
                                . ('\n':) . sg

-- | Pretty-print the graph to stdout.
prettyPrint :: (DynGraph gr, Show a, Show b) => gr a b -> IO ()
prettyPrint = putStr . prettify
