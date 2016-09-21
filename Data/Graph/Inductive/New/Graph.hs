{-# LANGUAGE FlexibleContexts, StandaloneDeriving, TypeFamilies,
             UndecidableInstances #-}

{- |
   Module      : Data.Graph.Inductive.New.Graph
   Description : Graph data structure representations
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Data.Graph.Inductive.New.Graph where

import           Data.Graph.Inductive.New.SmallSet (Set)
import qualified Data.Graph.Inductive.New.SmallSet as SS

import           Data.Map (Map)
import qualified Data.Map as M

--------------------------------------------------------------------------------

class (Eq (Vertex g), Eq (Edge g)) => Graph g where
  type Vertex g :: *

  type VertexLabel g :: *
  type VertexLabel g = ()

  -- | Actually a half-edge
  type Edge g :: *

  type EdgeLabel g :: *
  type EdgeLabel g = ()

  type IncidenceColl g :: * -> *
  type IncidenceColl g = Set

  empty :: g

  isEmpty :: g -> Bool
  isEmpty = (0==) . order

  order :: g -> Int
  order = length . vertices

  size :: g -> Int
  size = (`quot` 2) . length . edges
  -- Divide by 2 as we have 2 unique IDs for every actual edge.

  vertices :: g -> [Vertex g]

  vertexLabels :: g -> [(Vertex g, VertexLabel g)]

  edges :: g -> [Edge g]

  edgeLabels :: g -> [(Edge g, EdgeLabel g)]

  hasVertex :: g -> Vertex g -> Bool
  hasVertex g v = v `elem` vertices g

  hasEdge :: g -> Edge g -> Bool
  hasEdge g e = e `elem` edges g

  inverseEdge :: g -> Edge g -> Maybe (Edge g)

  incidentTo :: g -> Edge g -> Maybe (Incidence g)

  isIncidentTo :: g -> Edge g -> Vertex g -> Bool
  isIncidentTo g e v = maybe False (elem e) (incident g v)

  adjacency :: g -> Vertex g -> Maybe [Adjacency g]
  -- TOOD: write a default using incident, inverseEdge and incidentTo

  incident :: g -> Vertex g -> Maybe [Edge g]
  incident g v = map adjEdge <$> (adjacency g v)

  neighbours :: g -> Vertex g -> Maybe [Vertex g]
  neighbours g v = map neighbour <$> (adjacency g v)

  isNeighbourOf :: g -> Vertex g -> Vertex g -> Bool
  isNeighbourOf g v w = maybe False (elem v) (neighbours g w)

  -- | >>> sum (mapMaybe (degree g) (vertices g)) == 2 * size g
  degree :: g -> Vertex g -> Maybe Int
  degree g v = length <$> incident g v

  -- TODO: minDegree/maxDegree/avDegree (?)

data Adjacency g = Adj { adjEdge   :: Edge g
                       , invEdge   :: Edge g
                       , neighbour :: Vertex g
                       }

deriving instance (Graph g, Eq   (Vertex g), Eq   (Edge g)) => Eq   (Adjacency g)
deriving instance (Graph g, Ord  (Vertex g), Ord  (Edge g)) => Ord  (Adjacency g)
deriving instance (Graph g, Show (Vertex g), Show (Edge g)) => Show (Adjacency g)
deriving instance (Graph g, Read (Vertex g), Read (Edge g)) => Read (Adjacency g)

data Incidence g = Inc { halfIncidence :: Vertex g
                       , allIncidence  :: IncidenceColl g (Vertex g)
                       }

deriving instance (Graph g, Eq   (Vertex g), Eq   (IncidenceColl g (Vertex g))) => Eq   (Incidence g)
deriving instance (Graph g, Show (Vertex g), Show (IncidenceColl g (Vertex g))) => Show (Incidence g)
deriving instance (Graph g, Read (Vertex g), Read (IncidenceColl g (Vertex g))) => Read (Incidence g)

--------------------------------------------------------------------------------

-- Sample implementation

data Gr a b = Gr { grVertices :: Map GrVertex (GrVertexInfo a)
                 , grEdges    :: Map GrEdge   (GrEdgeInfo b)
                 }
  deriving (Eq, Show, Read)

newtype GrVertex = GVer { unGVer :: Int }
  deriving (Eq, Ord, Show, Read)

data GrVertexInfo a = GVerI { grVerLab :: a
                            , grAdj    :: [GrEdge]
                            }
  deriving (Eq, Show, Read)
  -- TODO: Eq instance incorrect due to []

newtype GrEdge = GEdg { unGEdg :: Int }
  deriving (Eq, Ord, Show, Read)

data GrEdgeInfo b = GEdgI { grEdgLab :: b
                          , grEdgVer :: GrVertex
                          , grEdgInv :: GrEdge
                          }
  deriving (Eq, Ord, Show, Read)

instance Graph (Gr a b) where
  type Vertex (Gr a b) = GrVertex

  type VertexLabel (Gr a b) = a

  type Edge (Gr a b) = GrEdge

  type EdgeLabel (Gr a b) = b

  empty = Gr M.empty M.empty

  isEmpty = M.null . grVertices

  order = M.size . grVertices

  size = (`quot` 2) . M.size . grEdges

  vertices = M.keys . grVertices

  vertexLabels = M.assocs . M.map grVerLab . grVertices

  edges = M.keys . grEdges

  edgeLabels = M.assocs . M.map grEdgLab . grEdges

  hasVertex g v = v `M.member` grVertices g

  hasEdge g e = e `M.member` grEdges g

  inverseEdge g e = grEdgInv <$> M.lookup e (grEdges g)

  incidentTo g e = do ei  <- M.lookup e (grEdges g)
                      let v = grEdgVer ei
                      ei' <- M.lookup (grEdgInv ei) (grEdges g)
                      let col = SS.fromList [v, grEdgVer ei']
                      return (Inc v col)

  isIncidentTo g e v = maybe False ((v==) . grEdgVer)
                             (M.lookup e (grEdges g))

  adjacency g v = map (mkAdj g) <$> incident g v

  incident g v = grAdj <$> (v `M.lookup` grVertices g)

  neighbours g v = map (snd . grAdjNbr g) <$> incident g v

  -- default definition of isNeighbourOf suffices.

  degree g v = length . grAdj <$> M.lookup v (grVertices g)

mkAdj :: Gr a b -> GrEdge -> Adjacency (Gr a b)
mkAdj g e = let (e', nbr) = grAdjNbr g e
            in Adj e e' nbr

grAdjNbr :: Gr a b -> GrEdge -> (GrEdge, GrVertex)
grAdjNbr g e = (e', nbr)
  where
    getEInfo = (grEdges g M.!)

    e' = grEdgInv (getEInfo e)

    nbr = grEdgVer (getEInfo e')
