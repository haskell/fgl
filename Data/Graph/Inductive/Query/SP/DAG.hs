-- Module written by Hunter Herman
-- Contact @ hherman1@macalester.edu
-- | An implementation of O(N + E) algorithms for finding shortest
--   and longest paths on directed acyclic graphs. Most functions
--   require the input of a topological sort of the graph and 
--   the length of the topological sort. A topological sort can
--   be obtained with the 'topsort' function in Data.Graph.Inductive.Query.DFS
--
--   It is important to note that the library does not check if
--   the input graph is a DAG. Unreliable results will be returned on 
--   any other graph. 
--   
--   It may be of interest to some that it is not necessary to output a list
--   of Nodes from these functions. The shortest/longest path functions are simple
--   combinations of runPaths, tracePath, and Data.Vector.maxIndex. 
--
--   The algorithms are adaptations of the following:
--   <https://en.wikipedia.org/wiki/Topological_sorting#Application_to_shortest_path_finding>
module Data.Graph.Inductive.Query.SP.DAG (
      shortestPathBetweenNodes
    , longestPathBetweenNodes
    , longestPathFromSource
    , longestPath
    , tracePath
    , runPaths
    , Number
) where

import Prelude hiding (read)
import Data.Graph.Inductive.Graph
import Data.Vector (Vector,maxIndex,freeze,(!))
import Data.Vector.Mutable (IOVector,write,set,read,new)
import Control.Monad (MonadPlus,mplus,mzero)
import Data.Foldable (foldl',Foldable)
import Data.Functor ((<$>))
-- | A wrapper for any number, including Infinity and NegativeInfinity
--   options.
data Number a = Infinity | NegativeInfinity | Number a deriving (Show, Eq)

instance Ord a => Ord (Number a) where
    compare Infinity Infinity = EQ
    compare Infinity _ = GT
    compare _ Infinity = LT
    compare NegativeInfinity NegativeInfinity = EQ
    compare NegativeInfinity _ = LT
    compare _ NegativeInfinity = GT
    compare (Number x) (Number y) = compare x y

instance Functor Number where
    fmap f (Number x) = Number $ f x
    fmap _ Infinity = Infinity
    fmap _ NegativeInfinity = NegativeInfinity

-- | Returns the shortest path between a start node and an end node.
shortestPathBetweenNodes :: (Graph gr, Real b, Foldable f)
    => Node -- ^ Start
    -> Node -- ^ Destination
    -> Int  -- ^ Length of topsort
    -> f Node -- ^ Topsort
    -> gr a b -- ^ Graph
    -> IO Path
shortestPathBetweenNodes s d l t g = do
    (_,predecessor) <- runPaths (Just s) (Number 0) Infinity (>) l t g
    return $ tracePath d predecessor

-- | Returns the longest path between a start node and an end node.
longestPathBetweenNodes :: (Graph gr, Real b, Foldable f)
    => Node -- ^ Start
    -> Node -- ^ Destination
    -> Int  -- ^ length of topsort
    -> f Node -- ^ topsort
    -> gr a b -- ^ Graph
    -> IO Path
longestPathBetweenNodes s d l t g = do
    (_,predecessor) <- runPaths (Just s) (Number 0) NegativeInfinity (<) l t g
    return $ tracePath d predecessor

-- | Returns the longest possible path from a source node.
longestPathFromSource :: (Graph gr, Real b, Foldable f)
    => Node -- ^ Source node
    -> Int  -- ^ Length of topsort
    -> f Node -- ^ Topsort
    -> gr a b -- ^ Graph
    -> IO Path
longestPathFromSource s l t g = do
    (dist,predecessor) <- runPaths (Just s) (Number 0) NegativeInfinity (<) l t g
    return $ tracePath (maxIndex dist) predecessor

-- | Returns the longest possible path in the graph.
longestPath :: (Graph gr, Real b, Foldable f)
    => Int  -- ^ length of topsort
    -> f Node -- ^ topsort
    -> gr a b
    -> IO Path
longestPath l t g = do
    (dist,predecessor) <- runPaths Nothing (Number 0) (Number 0) (<) l t g
    return $ tracePath (maxIndex dist) predecessor

-- | Traces a path backwards through a vector of predecessors,
--   where a vector of predecessors is a vector such that the value 
--   in the vector at any position n where n is a node in the graph 
--   is the predecessor to that node.
tracePath :: MonadPlus m 
    => Node -- ^ End node of path
    -> Vector (Maybe Node) -- ^ Vector of predecessors
    -> m Node -- ^ MonadPlus of Path
tracePath n v = 
    case v ! n of
         Just node 
              -> mplus (return n) $ tracePath node v
         Nothing
              -> mplus (return n) mzero

-- | A reasonably generalized function used for generating 
--   a vector of distances and predecessors obtained for a given
--   comparison operation, an optional source node, and so on.
runPaths :: (Graph gr, Real b, Foldable f)
    => Maybe Node -- Source node
    -> Number b -- ^ Source node initialization value
    -> Number b -- ^ Distances vector init values
    -> (Number b -> Number b -> Bool) -- ^ Weight multiplie
    -> Int -- ^ Length of the top sort
    -> f Node -- ^ Topological sorting of the graph in some foldable format
    -> gr a b -- ^ A graph.
-- | A vector of distances and predecessors. The distances represent maximum or
--   minimum distance to a given node, and the predecessors represent the node that
--   linked to that node on the maximum or minimum path.
    -> IO (Vector (Number b), Vector (Maybe Node))
runPaths s v ds c l t g = 
    freezeVecs =<< runPaths' s v ds c l t g

freezeVecs ::
    (IOVector a, IOVector b)
    -> IO (Vector a, Vector b)
freezeVecs (v1,v2) = do
    vec1 <- freeze v1
    vec2 <- freeze v2
    return (vec1,vec2)

runPaths' :: (Graph gr, Real b, Foldable f)
    => Maybe Node -- Source node
    -> Number b -- ^ Source node initialization value
    -> Number b -- ^ Distances vector init values
    -> (Number b -> Number b -> Bool) -- ^ Weight modifier
    -> Int -- ^ Length of the top sort
    -> f Node -- ^ Topological sorting of the graph in some foldable format
    -> gr a b
    -> IO (IOVector (Number b), IOVector (Maybe Node))
runPaths' s v distV m n t g = do
    (distances,predecessors) <- genVectors n distV Nothing
    case s of 
         Just source -> write distances source v
         Nothing -> return ()
    (newDistances,newPredecessors) <- pathsByComparison distances predecessors m t g
    return (newDistances,newPredecessors)

genVectors :: 
    Int -- ^ Length of the vectors, ie, length of the graph or topsort
    -> a
    -> b
    -> IO (IOVector a, IOVector b)
genVectors n val1 val2 = do
    distances <- new (n + 1)
    predecessors <- new (n + 1)
    set distances val1
    set predecessors val2
    return (distances,predecessors)

pathsByComparison :: (Graph gr, Real b, Foldable f)
    => IOVector (Number b) -- ^ Vector that will hold accumulated weights 
    -> IOVector (Maybe Node)
    -> (Number b -> Number b -> Bool)
    -> f Node -- Topological sorting of the graph in some foldable format
    -> gr a b
    -> IO (IOVector (Number b), IOVector (Maybe Node))
pathsByComparison dist predecessor m top graph =
    foldl' (\s node
        -> s >> updateSuccessors dist predecessor m node graph)
        (return (dist, predecessor)) top 


updateSuccessors :: (Graph gr, Real b)
    => IOVector (Number b)
    -> IOVector (Maybe Node)
    -> (Number b -> Number b -> Bool)
    -> Node
    -> gr a b
    -> IO (IOVector (Number b), IOVector (Maybe Node))
updateSuccessors distances predecessors comp t g = 
    let successors = lsuc g t in do
    sequence_ $ map (\(successor,weight) -> do
        ds <- read distances successor
        dt <- read distances t
        if comp ds ((+weight) <$> dt)
            then do
                write distances successor $ (+weight) <$> dt
                write predecessors successor $ Just t
            else return ())
        successors
    return (distances,predecessors)
