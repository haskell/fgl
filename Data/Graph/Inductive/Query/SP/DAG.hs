module Data.Graph.Inductive.Query.SP.DAG (
      shortestPathBetweenNodes
    , shortestPathFromSource
    , shortestPath
    , longestPathBetweenNodes
    , longestPathFromSource
    , longestPath
    , tracePath
    , runPaths
) where

import Prelude hiding (read)
import Data.Graph.Inductive.Graph
import Data.Vector (Vector,minIndex,maxIndex,freeze,(!))
import Data.Vector.Mutable (IOVector,write,set,read,new)
import Control.Monad (MonadPlus,mplus,mzero)
import Data.Foldable (foldl')

shortestPathBetweenNodes :: (Graph gr, Real b, Foldable f)
    => Node -- ^ Start
    -> Node -- ^ Destination
    -> Int  -- ^ length of topsort
    -> f Node -- ^ topsort
    -> gr a b
    -> IO Path
shortestPathBetweenNodes s d l t g = do
    (_,predecessor) <- runPaths (Just s) (Just 0) Nothing (<) l t g
    return $ tracePath d predecessor

shortestPathFromSource :: (Graph gr, Real b, Foldable f)
    => Node -- ^ Source
    -> Int  -- ^ length of topsort
    -> f Node -- ^ topsort
    -> gr a b
    -> IO Path
shortestPathFromSource s l t g = do
    (dist,predecessor) <- runPaths (Just s) (Just 0) Nothing (<) l t g
    return $ tracePath (minIndex dist) predecessor

shortestPath :: (Graph gr, Real b, Foldable f)
    => Int  -- ^ length of topsort
    -> f Node -- ^ topsort
    -> gr a b
    -> IO Path
shortestPath l t g = do
    (dist,predecessor) <- runPaths Nothing Nothing (Just 0) (<) l t g
    return $ tracePath (minIndex dist) predecessor

longestPathBetweenNodes :: (Graph gr, Real b, Foldable f)
    => Node -- ^ Start
    -> Node -- ^ Destination
    -> Int  -- ^ length of topsort
    -> f Node -- ^ topsort
    -> gr a b
    -> IO Path
longestPathBetweenNodes s d l t g = do
    (_,predecessor) <- runPaths (Just s) (Just 0) Nothing (>) l t g
    return $ tracePath d predecessor

longestPathFromSource :: (Graph gr, Real b, Foldable f)
    => Node -- ^ Source
    -> Int  -- ^ length of topsort
    -> f Node -- ^ topsort
    -> gr a b
    -> IO Path
longestPathFromSource s l t g = do
    (dist,predecessor) <- runPaths (Just s) (Just 0) Nothing (>) l t g
    return $ tracePath (maxIndex dist) predecessor

longestPath :: (Graph gr, Real b, Foldable f)
    => Int  -- ^ length of topsort
    -> f Node -- ^ topsort
    -> gr a b
    -> IO Path
longestPath l t g = do
    (dist,predecessor) <- runPaths Nothing Nothing (Just 0) (>) l t g
    return $ tracePath (maxIndex dist) predecessor

tracePath :: MonadPlus m 
    => Node -- ^ End node of path
    -> Vector (Maybe Node) -- ^ Vector of predecessors
    -> m Node
tracePath n v = 
    case v ! n of
         Just node 
              -> mplus (return node) $ tracePath node v
         Nothing 
              -> mzero

runPaths :: (Graph gr, Real b, Foldable f)
    => Maybe Node -- Source node
    -> Maybe b -- ^ Source node initialization value
    -> Maybe b -- ^ Distances vector init values
    -> (Maybe b -> Maybe b -> Bool) -- ^ Comparison to use on nodes
    -> Int -- ^ Length of the top sort
    -> f Node -- ^ Topological sorting of the graph in some foldable format
    -> gr a b
    -> IO (Vector (Maybe b), Vector (Maybe Node))
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
    -> Maybe b -- ^ Source node initialization value
    -> Maybe b -- ^ Distances vector init values
    -> (Maybe b -> Maybe b -> Bool) -- ^ Comparison to use on nodes
    -> Int -- ^ Length of the top sort
    -> f Node -- ^ Topological sorting of the graph in some foldable format
    -> gr a b
    -> IO (IOVector (Maybe b), IOVector (Maybe Node))
runPaths' s v distV comp n t g = do
    (distances,predecessors) <- genVectors n distV Nothing
    case s of 
         Just source -> write distances source v
         Nothing -> return ()
    (newDistances,newPredecessors) <- pathsByComparison distances predecessors comp t g
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
    => IOVector (Maybe b) -- ^ Vector that will hold accumulated weights 
    -> IOVector (Maybe Node)
    -> (Maybe b -> Maybe b -> Bool)
    -> f Node -- Topological sorting of the graph in some foldable format
    -> gr a b
    -> IO (IOVector (Maybe b), IOVector (Maybe Node))
pathsByComparison dist predecessor comp top graph =
    foldl' (\_ node
        -> updateSuccessors dist predecessor comp node graph)
        (return (dist, predecessor)) top 


updateSuccessors :: (Graph gr, Real b)
    => IOVector (Maybe b)
    -> IOVector (Maybe Node)
    -> (Maybe b -> Maybe b -> Bool)
    -> Node
    -> gr a b
    -> IO (IOVector (Maybe b), IOVector (Maybe Node))
updateSuccessors distances predecessors comparison t g = 
    let successors = lsuc g t in do
    sequence_ $ map (\(successor,weight) -> do
        ds <- read distances successor
        dt <- read distances t
        if comparison ds ((+weight) <$> dt)
            then do
                write distances successor $ (+weight) <$> dt
                write predecessors successor $ Just t
            else return ())
        successors
    return (distances,predecessors)
