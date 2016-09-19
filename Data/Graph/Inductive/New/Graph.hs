{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{- |
   Module      : Data.Graph.Inductive.New.Graph
   Description : Graph data structure representations
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Data.Graph.Inductive.New.Graph where

import           Data.Map (Map)
import qualified Data.Map as M

--------------------------------------------------------------------------------

class (Eq (Vertex g), Eq (Edge g)) => Graph g where
  type Vertex g :: *

  type Edge g :: *

  empty :: g

  isEmpty :: g -> Bool
  isEmpty = (0==) . order

  order :: g -> Int
  order = length . vertices

  size :: g -> Int
  size = (`quot` 2) . length . edges
  -- Divide by 2 as we have 2 unique IDs for every actual edge.

  vertices :: g -> [Vertex g]

  edges :: g -> [Edge g]

  hasVertex :: g -> Vertex g -> Bool
  hasVertex g v = v `elem` vertices g

  hasEdge :: g -> Edge g -> Bool
  hasEdge g e = e `elem` edges g

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

  type Edge (Gr a b) = GrEdge

  empty = Gr M.empty M.empty

  isEmpty = M.null . grVertices

  order = M.size . grVertices

  size = (`quot` 2) . M.size . grEdges

  vertices = M.keys . grVertices

  edges = M.keys . grEdges

  hasVertex g v = v `M.member` grVertices g

  hasEdge g e = e `M.member` grEdges g
