{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

-- (c) 2002 by Martin Erwig [see file COPYRIGHT]
-- | Static IOArray-based Graphs
module Data.Graph.Inductive.Monad.IOArray(
    -- * Graph Representation
    SGr(..), GraphRep, Context', USGr,
    defaultGraphSize, emptyN,
    -- * Utilities
    removeDel,
) where

import qualified Data.Graph.Inductive.Monad.Primitive as Prim
import Data.Graph.Inductive.Monad.Primitive hiding (SGr)
import Control.Monad.Primitive (RealWorld)

----------------------------------------------------------------------
-- GRAPH REPRESENTATION
----------------------------------------------------------------------

type SGr = Prim.SGr RealWorld
