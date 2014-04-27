------------------------------------------------------------------------------
--
--  Inductive.hs -- Functional Graph Library
--
--  (c) 1999-2007 by Martin Erwig [see file COPYRIGHT]
--
------------------------------------------------------------------------------

module Data.Graph.Inductive(
    module Data.Graph.Inductive.Graph,
    module Data.Graph.Inductive.PatriciaTree,
    module Data.Graph.Inductive.Basic,
    module Data.Graph.Inductive.Monad,
    module Data.Graph.Inductive.Monad.IOArray,
    module Data.Graph.Inductive.Query,
    module Data.Graph.Inductive.NodeMap,
    -- * Version Information
    version
) where

import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Monad
import Data.Graph.Inductive.Monad.IOArray
import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query

import           Data.Version (showVersion)
import qualified Paths_fgl    as Paths (version)

-- | Version info
version :: IO ()
version = putStrLn $ "\nFGL - Functional Graph Library, version "
                      ++ showVersion Paths.version
