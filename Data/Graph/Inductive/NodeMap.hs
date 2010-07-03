-- | Utility methods to automatically generate and keep track of a mapping
-- between node labels and 'Node's.
module Data.Graph.Inductive.NodeMap(
    -- * Functional Construction
    NodeMap,
    -- ** Map Construction
    new, fromGraph, mkNode, mkNode_, mkNodes, mkNodes_, mkEdge, mkEdges,
    -- ** Graph Construction
    -- | These functions mirror the construction and destruction functions in
    -- 'Data.Graph.Inductive.Graph', but use the given 'NodeMap' to look up
    -- the appropriate 'Node's.  Note that the 'insMapNode' family of functions
    -- will create new nodes as needed, but the other functions will not.
    insMapNode, insMapNode_, insMapEdge, delMapNode, delMapEdge, insMapNodes,
    insMapNodes_, insMapEdges, delMapNodes, delMapEdges, mkMapGraph,
    -- * Monadic Construction
    NodeMapM,
    -- | The following mirror the functional construction functions, but handle passing
    -- 'NodeMap's and 'Graph's behind the scenes.

    -- ** Map Construction
    run, run_, mkNodeM, mkNodesM, mkEdgeM, mkEdgesM,
    -- ** Graph Construction
    insMapNodeM, insMapEdgeM, delMapNodeM, delMapEdgeM, insMapNodesM,
    insMapEdgesM, delMapNodesM, delMapEdgesM
) where

import Prelude hiding (map)
import qualified Prelude as P (map)
import Control.Monad.State
import Data.Graph.Inductive.Graph
--import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Internal.FiniteMap

data (Ord a) => NodeMap a =
    NodeMap { map :: FiniteMap a Node,
	      key :: Int }
    deriving Show

-- | Create a new, empty mapping.
new :: (Ord a) => NodeMap a
new = NodeMap { map = emptyFM, key = 0 }

-- LNode = (Node, a)

-- | Generate a mapping containing the nodes in the given graph.
fromGraph :: (Ord a, Graph g) => g a b -> NodeMap a
fromGraph g =
    let ns = labNodes g
	aux (n, a) (m', k') = (addToFM m' a n, max n k')
	(m, k) = foldr aux (emptyFM, 0) ns
    in NodeMap { map = m, key = k+1 }

-- | Generate a labelled node from the given label.  Will return the same node
-- for the same label.
mkNode :: (Ord a) => NodeMap a -> a -> (LNode a, NodeMap a)
mkNode m@(NodeMap mp k) a =
    case lookupFM mp a of
	Just i	-> ((i, a), m)
	Nothing	->
	    let m' = NodeMap { map = addToFM mp a k, key = k+1 }
	    in ((k, a), m')

-- | Generate a labelled node and throw away the modified 'NodeMap'.
mkNode_ :: (Ord a) => NodeMap a -> a -> LNode a
mkNode_ m a = fst $ mkNode m a

-- | Generate a 'LEdge' from the node labels.
mkEdge :: (Ord a) => NodeMap a -> (a, a, b) -> Maybe (LEdge b)
mkEdge (NodeMap m _) (a1, a2, b) =
    do n1 <- lookupFM m a1
       n2 <- lookupFM m a2
       return (n1, n2, b)

-- | Generates a list of 'LEdge's.
mkEdges :: (Ord a) => NodeMap a -> [(a, a, b)] -> Maybe [LEdge b]
mkEdges m es = mapM (mkEdge m) es

-- | Construct a list of nodes.
mkNodes :: (Ord a) => NodeMap a -> [a] -> ([LNode a], NodeMap a)
mkNodes = map' mkNode

map' :: (a -> b -> (c, a)) -> a -> [b] -> ([c], a)
map' _ a [] = ([], a)
map' f a (b:bs) =
    let (c, a') = f a b
	(cs, a'') = map' f a' bs
    in (c:cs, a'')

-- | Construct a list of nodes and throw away the modified 'NodeMap'.
mkNodes_ :: (Ord a) => NodeMap a -> [a] -> [LNode a]
mkNodes_ m as = fst $ mkNodes m as

insMapNode :: (Ord a, DynGraph g) => NodeMap a -> a -> g a b -> (g a b, NodeMap a, LNode a)
insMapNode m a g =
    let (n, m') = mkNode m a
    in (insNode n g, m', n)

insMapNode_ :: (Ord a, DynGraph g) => NodeMap a -> a -> g a b -> g a b
insMapNode_ m a g =
    let (g', _, _) = insMapNode m a g
    in g'

insMapEdge :: (Ord a, DynGraph g) => NodeMap a -> (a, a, b) -> g a b -> g a b
insMapEdge m e g =
    let (Just e') = mkEdge m e
    in insEdge e' g

delMapNode :: (Ord a, DynGraph g) => NodeMap a -> a -> g a b -> g a b
delMapNode m a g =
    let (n, _) = mkNode_ m a
    in delNode n g

delMapEdge :: (Ord a, DynGraph g) => NodeMap a -> (a, a) -> g a b -> g a b
delMapEdge m (n1, n2) g =
    let Just (n1', n2', _) = mkEdge m (n1, n2, ())
    in delEdge (n1', n2') g

insMapNodes :: (Ord a, DynGraph g) => NodeMap a -> [a] -> g a b -> (g a b, NodeMap a, [LNode a])
insMapNodes m as g =
    let (ns, m') = mkNodes m as
    in (insNodes ns g, m', ns)

insMapNodes_ :: (Ord a, DynGraph g) => NodeMap a -> [a] -> g a b -> g a b
insMapNodes_ m as g =
    let (g', _, _) = insMapNodes m as g
    in g'

insMapEdges :: (Ord a, DynGraph g) => NodeMap a -> [(a, a, b)] -> g a b -> g a b
insMapEdges m es g =
    let Just es' = mkEdges m es
    in insEdges es' g

delMapNodes :: (Ord a, DynGraph g) => NodeMap a -> [a] -> g a b -> g a b
delMapNodes m as g =
    let ns = P.map fst $ mkNodes_ m as
    in delNodes ns g

delMapEdges :: (Ord a, DynGraph g) => NodeMap a -> [(a, a)] -> g a b -> g a b
delMapEdges m ns g =
    let Just ns' =  mkEdges m $ P.map (\(a, b) -> (a, b, ())) ns
	ns'' = P.map (\(a, b, _) -> (a, b)) ns'
    in delEdges ns'' g

mkMapGraph :: (Ord a, DynGraph g) => [a] -> [(a, a, b)] -> (g a b, NodeMap a)
mkMapGraph ns es =
    let (ns', m') = mkNodes new ns
	Just es' = mkEdges m' es
    in (mkGraph ns' es', m')

-- | Graph construction monad; handles passing both the 'NodeMap' and the
-- 'Graph'.
type NodeMapM a b g r = State (NodeMap a, g a b) r

-- | Run a construction; return the value of the computation, the modified
-- 'NodeMap', and the modified 'Graph'.
run :: (DynGraph g, Ord a) => g a b -> NodeMapM a b g r -> (r, (NodeMap a, g a b))
run g m = runState m (fromGraph g, g)

-- | Run a construction and only return the 'Graph'.
run_ :: (DynGraph g, Ord a) => g a b -> NodeMapM a b g r -> g a b
run_ g m = snd . snd $ run g m

{- not used
liftN1 :: (Ord a, DynGraph g) => (NodeMap a -> (c, NodeMap a)) -> NodeMapM a b g c
liftN1 f =
    do (m, g) <- get
       let (r, m') = f m
       put (m', g)
       return r

liftN1' :: (Ord a, DynGraph g) => (NodeMap a -> c) -> NodeMapM a b g c
liftN1' f =
    do (m, g) <- get
       return $ f m
-}
liftN2 :: (Ord a, DynGraph g) => (NodeMap a -> c -> (d, NodeMap a)) -> c -> NodeMapM a b g d
liftN2 f c =
    do (m, g) <- get
       let (r, m') = f m c
       put (m', g)
       return r

liftN2' :: (Ord a, DynGraph g) => (NodeMap a -> c -> d) -> c -> NodeMapM a b g d
liftN2' f c =
    do (m, _) <- get
       return $ f m c
{- not used
liftN3 :: (Ord a, DynGraph g) => (NodeMap a -> c -> d -> (e, NodeMap a)) -> c -> d -> NodeMapM a b g e
liftN3 f c d =
    do (m, g) <- get
       let (r, m') = f m c d
       put (m', g)
       return r

liftN3' :: (Ord a, DynGraph g) => (NodeMap a -> c -> d -> e) -> c -> d -> NodeMapM a b g e
liftN3' f c d =
    do (m, g) <- get
       return $ f m c d
-}
liftM1 :: (Ord a, DynGraph g) => (NodeMap a -> c -> g a b -> g a b) -> c -> NodeMapM a b g ()
liftM1 f c =
    do (m, g) <- get
       let g' = f m c g
       put (m, g')

liftM1' :: (Ord a, DynGraph g) => (NodeMap a -> c -> g a b -> (g a b, NodeMap a, d)) -> c -> NodeMapM a b g d
liftM1' f c =
    do (m, g) <- get
       let (g', m', r) = f m c g
       put (m', g')
       return r

-- | Monadic node construction.
mkNodeM :: (Ord a, DynGraph g) => a -> NodeMapM a b g (LNode a)
mkNodeM = liftN2 mkNode

mkNodesM :: (Ord a, DynGraph g) => [a] -> NodeMapM a b g [LNode a]
mkNodesM = liftN2 mkNodes

mkEdgeM :: (Ord a, DynGraph g) => (a, a, b) -> NodeMapM a b g (Maybe (LEdge b))
mkEdgeM = liftN2' mkEdge

mkEdgesM :: (Ord a, DynGraph g) => [(a, a, b)] -> NodeMapM a b g (Maybe [LEdge b])
mkEdgesM = liftN2' mkEdges

insMapNodeM :: (Ord a, DynGraph g) => a -> NodeMapM a b g (LNode a)
insMapNodeM = liftM1' insMapNode

insMapEdgeM :: (Ord a, DynGraph g) => (a, a, b) -> NodeMapM a b g ()
insMapEdgeM = liftM1 insMapEdge

delMapNodeM :: (Ord a, DynGraph g) => a -> NodeMapM a b g ()
delMapNodeM = liftM1 delMapNode

delMapEdgeM :: (Ord a, DynGraph g) => (a, a) -> NodeMapM a b g ()
delMapEdgeM = liftM1 delMapEdge

insMapNodesM :: (Ord a, DynGraph g) => [a] -> NodeMapM a b g [LNode a]
insMapNodesM = liftM1' insMapNodes

insMapEdgesM :: (Ord a, DynGraph g) => [(a, a, b)] -> NodeMapM a b g ()
insMapEdgesM = liftM1 insMapEdges

delMapNodesM :: (Ord a, DynGraph g) => [a] -> NodeMapM a b g ()
delMapNodesM = liftM1 delMapNodes

delMapEdgesM :: (Ord a, DynGraph g) => [(a, a)] -> NodeMapM a b g ()
delMapEdgesM = liftM1 delMapEdges
