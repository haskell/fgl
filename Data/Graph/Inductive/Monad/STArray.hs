{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

-- (c) 2002 by Martin Erwig [see file COPYRIGHT]
-- | Static IOArray-based Graphs
module Data.Graph.Inductive.Monad.STArray(
    -- * Graph Representation
    SGr(..), GraphRep, Context', USGr,
    defaultGraphSize, emptyN,
    -- * Utilities
    removeDel,
) where

import Data.Graph.Inductive.Monad.Primitive
