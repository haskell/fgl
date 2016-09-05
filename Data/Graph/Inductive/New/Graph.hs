{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{- |
   Module      : Data.Graph.Inductive.New.Graph
   Description : Graph data structure representations
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Data.Graph.Inductive.New.Graph where

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
