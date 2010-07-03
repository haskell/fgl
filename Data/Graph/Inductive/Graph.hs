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
    --		Static means that a graph itself cannot be changed
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
    nodes,edges,newNodes,gelem,
    -- ** Graph Construction and Destruction
    insNode,insEdge,delNode,delEdge,delLEdge,
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
) where


import Data.List (sortBy)


{- Signatures:

-- basic operations
empty      ::    Graph gr => gr a b
isEmpty    ::    Graph gr => gr a b -> Bool
match      ::    Graph gr => Node -> gr a b -> Decomp gr a b
mkGraph    ::    Graph gr => [LNode a] -> [LEdge b] -> gr a b
(&)        :: DynGraph gr => Context a b -> gr a b -> gr a b

-- graph folds and maps
ufold      :: Graph gr => ((Context a b) -> c -> c) -> c -> gr a b -> c
gmap       :: Graph gr => (Context a b -> Context c d) -> gr a b -> gr c d
nmap       :: Graph gr => (a -> c) -> gr a b -> gr c b
emap       :: Graph gr => (b -> c) -> gr a b -> gr a c

-- graph projection
matchAny   :: Graph gr => gr a b -> GDecomp g a b
nodes      :: Graph gr => gr a b -> [Node]
edges      :: Graph gr => gr a b -> [Edge]
labNodes   :: Graph gr => gr a b -> [LNode a]
labEdges   :: Graph gr => gr a b -> [LEdge b]
newNodes   :: Graph gr => Int -> gr a b -> [Node]
noNodes    :: Graph gr => gr a b -> Int
nodeRange  :: Graph gr => gr a b -> (Node,Node)
gelem      :: Graph gr => Node -> gr a b -> Bool

-- graph construction & destruction
insNode    :: DynGraph gr => LNode a   -> gr a b -> gr a b
insEdge    :: DynGraph gr => LEdge b   -> gr a b -> gr a b
delNode    ::    Graph gr => Node      -> gr a b -> gr a b
delEdge    :: DynGraph gr => Edge      -> gr a b -> gr a b
delLEdge   :: (DynGraph gr, Eq b) =>
                             LEdge b   -> gr a b -> gr a b
insNodes   :: DynGraph gr => [LNode a] -> gr a b -> gr a b
insEdges   :: DynGraph gr => [LEdge b] -> gr a b -> gr a b
delNodes   ::    Graph gr => [Node]    -> gr a b -> gr a b
delEdges   :: DynGraph gr => [Edge]    -> gr a b -> gr a b
buildGr    :: DynGraph gr => [Context a b] -> gr a b
mkUGraph   :: DynGraph gr => [Node] -> [Edge] -> gr () ()

-- graph inspection
context    :: Graph gr => gr a b -> Node -> Context a b
lab        :: Graph gr => gr a b -> Node -> Maybe a
neighbors  :: Graph gr => gr a b -> Node -> [Node] 
suc        :: Graph gr => gr a b -> Node -> [Node]
pre        :: Graph gr => gr a b -> Node -> [Node] 
lsuc       :: Graph gr => gr a b -> Node -> [(Node,b)]
lpre       :: Graph gr => gr a b -> Node -> [(Node,b)] 
out        :: Graph gr => gr a b -> Node -> [LEdge b] 
inn        :: Graph gr => gr a b -> Node -> [LEdge b] 
outdeg     :: Graph gr => gr a b -> Node -> Int
indeg      :: Graph gr => gr a b -> Node -> Int
deg        :: Graph gr => gr a b -> Node -> Int

-- context inspection
node'      :: Context a b -> Node
lab'       :: Context a b -> a
labNode'   :: Context a b -> LNode a
neighbors' :: Context a b -> [Node] 
suc'       :: Context a b -> [Node]
pre'       :: Context a b -> [Node] 
lpre'      :: Context a b -> [(Node,b)] 
lsuc'      :: Context a b -> [(Node,b)]
out'       :: Context a b -> [LEdge b] 
inn'       :: Context a b -> [LEdge b] 
outdeg'    :: Context a b -> Int
indeg'     :: Context a b -> Int
deg'       :: Context a b -> Int

-}

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

instance Show a => Show (LPath a) where
  show (LP xs) = show xs

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
  -- essential operations
  -- | An empty 'Graph'.
  empty     :: gr a b
  -- | True if the given 'Graph' is empty.
  isEmpty   :: gr a b -> Bool
  -- | Decompose a 'Graph' into the 'MContext' found for the given node and the
  -- remaining 'Graph'.
  match     :: Node -> gr a b -> Decomp gr a b
  -- | Create a 'Graph' from the list of 'LNode's and 'LEdge's.
  mkGraph   :: [LNode a] -> [LEdge b] -> gr a b
  -- | A list of all 'LNode's in the 'Graph'.
  labNodes  :: gr a b -> [LNode a]
  -- derived operations
  -- | Decompose a graph into the 'Context' for an arbitrarily-chosen 'Node'
  -- and the remaining 'Graph'.
  matchAny  :: gr a b -> GDecomp gr a b
  -- | The number of 'Node's in a 'Graph'.
  noNodes   :: gr a b -> Int
  -- | The minimum and maximum 'Node' in a 'Graph'.
  nodeRange :: gr a b -> (Node,Node)
  -- | A list of all 'LEdge's in the 'Graph'.
  labEdges  :: gr a b -> [LEdge b]
  -- default implementation of derived operations
  matchAny g = case labNodes g of
                 []      -> error "Match Exception, Empty Graph"
                 (v,_):_ -> (c,g') where (Just c,g') = match v g 
  noNodes = length . labNodes 
  nodeRange g = (minimum vs,maximum vs) where vs = map fst (labNodes g)
  labEdges = ufold (\(_,v,_,s)->((map (\(l,w)->(v,w,l)) s)++)) []


class Graph gr => DynGraph gr where
  -- | Merge the 'Context' into the 'DynGraph'.
  (&) :: Context a b -> gr a b -> gr a b


-- | Fold a function over the graph.
ufold :: Graph gr => ((Context a b) -> c -> c) -> c -> gr a b -> c
ufold f u g | isEmpty g = u
            | otherwise = f c (ufold f u g') 
            where (c,g') = matchAny g

-- | Map a function over the graph.
gmap :: DynGraph gr => (Context a b -> Context c d) -> gr a b -> gr c d
gmap f = ufold (\c->(f c&)) empty

-- | Map a function over the 'Node' labels in a graph.
nmap :: DynGraph gr => (a -> c) -> gr a b -> gr c b
nmap f = gmap (\(p,v,l,s)->(p,v,f l,s))

-- | Map a function over the 'Edge' labels in a graph.
emap :: DynGraph gr => (b -> c) -> gr a b -> gr a c
emap f = gmap (\(p,v,l,s)->(map1 f p,v,l,map1 f s))
         where map1 g = map (\(l,v)->(g l,v))

-- | List all 'Node's in the 'Graph'.
nodes :: Graph gr => gr a b -> [Node]
nodes = map fst . labNodes

-- | List all 'Edge's in the 'Graph'.
edges :: Graph gr => gr a b -> [Edge]
edges = map (\(v,w,_)->(v,w)) . labEdges

-- | List N available 'Node's, i.e. 'Node's that are not used in the 'Graph'.
newNodes :: Graph gr => Int -> gr a b -> [Node]
newNodes i g = [n+1..n+i] where (_,n) = nodeRange g

-- | 'True' if the 'Node' is present in the 'Graph'.
gelem :: Graph gr => Node -> gr a b -> Bool
gelem v g = case match v g of {(Just _,_) -> True; _ -> False}

-- | Insert a 'LNode' into the 'Graph'.
insNode :: DynGraph gr => LNode a -> gr a b -> gr a b
insNode (v,l) = (([],v,l,[])&)

-- | Insert a 'LEdge' into the 'Graph'.
insEdge :: DynGraph gr => LEdge b -> gr a b -> gr a b
insEdge (v,w,l) g = (pr,v,la,(l,w):su) & g'
                    where (Just (pr,_,la,su),g') = match v g

-- | Remove a 'Node' from the 'Graph'.
delNode :: Graph gr => Node -> gr a b -> gr a b
delNode v = delNodes [v]

-- | Remove an 'Edge' from the 'Graph'.
delEdge :: DynGraph gr => Edge -> gr a b -> gr a b
delEdge (v,w) g = case match v g of
                  (Nothing,_)        -> g
                  (Just (p,v',l,s),g') -> (p,v',l,filter ((/=w).snd) s) & g'

-- | Remove an 'LEdge' from the 'Graph'.
delLEdge :: (DynGraph gr, Eq b) => LEdge b -> gr a b -> gr a b
delLEdge (v,w,b) g = case match v g of
                  (Nothing,_)        -> g
                  (Just (p,v',l,s),g') -> (p,v',l,filter (\(x,n) -> x /= b || n /= w) s) & g'

-- | Insert multiple 'LNode's into the 'Graph'.
insNodes   :: DynGraph gr => [LNode a] -> gr a b -> gr a b
insNodes vs g = foldr insNode g vs

-- | Insert multiple 'LEdge's into the 'Graph'.
insEdges :: DynGraph gr => [LEdge b] -> gr a b -> gr a b
insEdges es g = foldr insEdge g es

-- | Remove multiple 'Node's from the 'Graph'.
delNodes :: Graph gr => [Node] -> gr a b -> gr a b
delNodes []     g = g
delNodes (v:vs) g = delNodes vs (snd (match v g))  

-- | Remove multiple 'Edge's from the 'Graph'.
delEdges :: DynGraph gr => [Edge]    -> gr a b -> gr a b
delEdges es g = foldr delEdge g es

-- | Build a 'Graph' from a list of 'Context's.
buildGr :: DynGraph gr => [Context a b] -> gr a b
buildGr = foldr (&) empty

-- mkGraph :: DynGraph gr => [LNode a] -> [LEdge b] -> gr a b
-- mkGraph vs es = (insEdges es . insNodes vs) empty

-- | Build a quasi-unlabeled 'Graph'.
mkUGraph :: Graph gr => [Node] -> [Edge] -> gr () ()
mkUGraph vs es = mkGraph (labUNodes vs) (labUEdges es) 
   where labUEdges = map (\(v,w)->(v,w,()))
         labUNodes = map (\v->(v,()))
 
-- | Find the context for the given 'Node'.  Causes an error if the 'Node' is
-- not present in the 'Graph'.
context :: Graph gr => gr a b -> Node -> Context a b
context g v = case match v g of
                (Nothing,_) -> error ("Match Exception, Node: "++show v)
                (Just c,_)  -> c 

-- | Find the label for a 'Node'.
lab :: Graph gr => gr a b -> Node -> Maybe a
lab g v = fst (match v g) >>= return.lab' 

-- | Find the neighbors for a 'Node'.
neighbors :: Graph gr => gr a b -> Node -> [Node] 
neighbors = (\(p,_,_,s) -> map snd (p++s)) .: context

-- | Find all 'Node's that have a link from the given 'Node'.
suc :: Graph gr => gr a b -> Node -> [Node]
suc = map snd .: context4l

-- | Find all 'Node's that link to to the given 'Node'.
pre :: Graph gr => gr a b -> Node -> [Node] 
pre = map snd .: context1l

-- | Find all 'Node's that are linked from the given 'Node' and the label of
-- each link.
lsuc :: Graph gr => gr a b -> Node -> [(Node,b)]
lsuc = map flip2 .: context4l

-- | Find all 'Node's that link to the given 'Node' and the label of each link.
lpre :: Graph gr => gr a b -> Node -> [(Node,b)] 
lpre = map flip2 .: context1l

-- | Find all outward-bound 'LEdge's for the given 'Node'.
out :: Graph gr => gr a b -> Node -> [LEdge b] 
out g v = map (\(l,w)->(v,w,l)) (context4l g v)

-- | Find all inward-bound 'LEdge's for the given 'Node'.
inn :: Graph gr => gr a b -> Node -> [LEdge b] 
inn g v = map (\(l,w)->(w,v,l)) (context1l g v)

-- | The outward-bound degree of the 'Node'.
outdeg :: Graph gr => gr a b -> Node -> Int
outdeg = length .: context4l

-- | The inward-bound degree of the 'Node'.
indeg :: Graph gr => gr a b -> Node -> Int
indeg  = length .: context1l

-- | The degree of the 'Node'.
deg :: Graph gr => gr a b -> Node -> Int
deg = (\(p,_,_,s) -> length p+length s) .: context

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


-- graph equality
--
nodeComp :: Eq b => LNode b -> LNode b -> Ordering
nodeComp n@(v,_) n'@(w,_) | n == n'   = EQ
                          | v<w       = LT
                          | otherwise = GT

slabNodes :: (Eq a,Graph gr) => gr a b -> [LNode a]
slabNodes = sortBy nodeComp . labNodes

edgeComp :: Eq b => LEdge b -> LEdge b -> Ordering
edgeComp e@(v,w,_) e'@(x,y,_) | e == e'              = EQ
                              | v<x || (v==x && w<y) = LT
                              | otherwise            = GT

slabEdges :: (Eq b,Graph gr) => gr a b -> [LEdge b]
slabEdges = sortBy edgeComp . labEdges

-- instance (Eq a,Eq b,Graph gr) => Eq (gr a b) where
--   g == g' = slabNodes g == slabNodes g' && slabEdges g == slabEdges g'

equal :: (Eq a,Eq b,Graph gr) => gr a b -> gr a b -> Bool
equal g g' = slabNodes g == slabNodes g' && slabEdges g == slabEdges g'


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
context1l :: Graph gr => gr a b -> Node -> Adj b
context1l = context1l' .: context

context4l :: Graph gr => gr a b -> Node -> Adj b
context4l = context4l' .: context

context1l' :: Context a b -> Adj b 
context1l' (p,v,_,s) = p++filter ((==v).snd) s

context4l' :: Context a b -> Adj b 
context4l' (p,v,_,s) = s++filter ((==v).snd) p
