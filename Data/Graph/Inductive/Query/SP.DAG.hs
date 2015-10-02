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

import Data.Graph.Inductive.Graph
import Data.Vector (minIndex,maxIndex,freeze,(!))
import Data.Mutable.Vector (IOVector,write,set,read,new)
import Control.Monad (MonadPlus,mplus,mzero)
import Data.Foldable (foldl',Foldable)

shortestPathBetweenNodes :: (Graph gr, Real b, Foldable f)
    => Node -- ^ Start
    -> Node -- ^ Destination
    -> Int  -- ^ length of topsort
    -> f Node -- ^ topsort
    -> gr a b
    -> IO Path
shortestPathBetweenNodes s t l top g = do
    (_,pred) <- runPaths (Just s) (Just 0) Nothing (<) l top g
    return $ tracePath t pred

shortestPathFromSource :: (Graph gr, Real b, Foldable f)
    => Node -- ^ Source
    -> Int  -- ^ length of topsort
    -> f Node -- ^ topsort
    -> gr a b
    -> IO Path
shortestPathFromSource s l top g = do
    (dist,pred) <- runPaths (Just s) (Just 0) Nothing (<) l top g
    return $ tracePath (minIndex dist) pred

shortestPath :: (Graph gr, Real b, Foldable f)
    -> Int  -- ^ length of topsort
    -> f Node -- ^ topsort
    -> gr a b
    -> IO Path
shortestPath l t g = do
    (dist,pred) <- runPaths Nothing Nothing (Just 0) (<) l top g
    return $ tracePath (minIndex dist) pred

longestPathBetweenNodes :: (Graph gr, Real b, Foldable f)
    => Node -- ^ Start
    -> Node -- ^ Destination
    -> Int  -- ^ length of topsort
    -> f Node -- ^ topsort
    -> gr a b
    -> IO Path
longestPathBetweenNodes s t l top g = do
    (_,pred) <- runPaths (Just s) (Just 0) Nothing (>) l top g
    return $ tracePath t pred

longestPathFromSource :: (Graph gr, Real b, Foldable f)
    => Node -- ^ Source
    -> Int  -- ^ length of topsort
    -> f Node -- ^ topsort
    -> gr a b
    -> IO Path
longestPathFromSource s l top g = do
    (dist,pred) <- runPaths (Just s) (Just 0) Nothing (>) l top g
    return $ tracePath (maxIndex dist) pred

longestPath :: (Graph gr, Real b, Foldable f)
    -> Int  -- ^ length of topsort
    -> f Node -- ^ topsort
    -> gr a b
    -> IO Path
longestPath l t g = do
    (dist,pred) <- runPaths Nothing Nothing (Just 0) (>) l top g
    return $ tracePath (maxIndex dist) pred

tracePath :: MonadPlus m 
    => Node -- ^ End node of path
    -> Vector (Maybe Node) -- ^ Vector of predecessors
    -> m Node
tracePath n v = 
    case v ! n of
         Just node 
              -> mplus node $ tracepath node v
         Nothing 
              -> mzero

runPaths = freezeVecs <$> runPaths'

freezeVecs ::
    (IOVector a, IOVector b)
    -> IO (Vector a, Vector b)
freezeVecs (v1,v2) = do
    vec1 <- freeze v1
    vec2 <- freeze v2
    return (vec1,vec2)

runPaths' :: (Graph gr, Real b, Foldable f)
    => Maybe Node -- Source node
    -> Maybe Int -- ^ Source node initialization value
    -> Maybe Int -- ^ Distances vector init values
    -> (Int -> Int -> Bool) -- ^ Comparison to use on nodes
    -> Int -- ^ Length of the top sort
    -> f Node -- ^ Topological sorting of the graph in some foldable format
    -> gr a b
    -> IO (IOVector (Maybe Int), IOVector (Maybe Node))
runPaths' s v distV n comp t g = do
    (distances,predecessors) <- genVectors n distV Nothing
    case s of 
         Just source -> write distances source v
         Nothing -> return ()
    pathsByComparison distances predecessors comp t g
    return (distances,predecessors)

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
    => IOVector (Maybe Int) -- ^ Vector that will hold accumulated weights 
    -> IOVector (Maybe Node)
    -> (Int -> Int -> Bool)
    -> f Node -- Topological sorting of the graph in some foldable format
    -> gr a b
    -> IO (IOVector (Maybe Weight), IOVector (Maybe Node))
pathsByComparison dist pred comp top graph =
    foldl' (\_ node
        -> updateSuccessors dist pred comp node graph)
        (return (dist, pred)) top 


updateSuccessors :: (Graph gr, Real b)
    => IOVector (Maybe Int)
    -> IOVector (Maybe Node)
    -> (Int -> Int -> Bool)
    -> Node
    -> gr a b
    -> IO (IOVector (Maybe Int), IOVector (Maybe Node))
updateSuccessors distances predecessors comparison t g = 
    let successors = lsuc g t in do
    sequence_ $ map (\(successor,weight) -> do
        ds <- read distances successor
        dt <- read distances t
        if comparison ds (dt + weight)
            then do
                write distances successor $ Just (dt + weight)
                write predecessors successor $ Just t
            else return ())
        outs
    return (distances,predecessors)
