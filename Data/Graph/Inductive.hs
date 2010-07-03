------------------------------------------------------------------------------
--  
--  Inductive.hs -- Functional Graph Library  
--
--  (c) 1999-2007 by Martin Erwig [see file COPYRIGHT]
--
------------------------------------------------------------------------------

module Data.Graph.Inductive(
    module Data.Graph.Inductive.Graph,
    module Data.Graph.Inductive.Tree,
    module Data.Graph.Inductive.Basic,
    module Data.Graph.Inductive.Monad,
    module Data.Graph.Inductive.Monad.IOArray,
    module Data.Graph.Inductive.Query,
    module Data.Graph.Inductive.Graphviz,
    module Data.Graph.Inductive.NodeMap,
    -- * Version Information
    version
) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Monad
import Data.Graph.Inductive.Monad.IOArray
import Data.Graph.Inductive.Query
import Data.Graph.Inductive.Graphviz
import Data.Graph.Inductive.NodeMap

-- | Version info
version :: IO ()
version = putStrLn "\nFGL - Functional Graph Library, April 2007"
