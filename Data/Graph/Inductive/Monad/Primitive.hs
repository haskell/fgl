{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
    MagicHash, ScopedTypeVariables, TypeFamilies #-}

-- (c) 2002 by Martin Erwig [see file COPYRIGHT]
-- | Static IOArray-based Graphs
module Data.Graph.Inductive.Monad.Primitive(
    -- * Graph Representation
    SGr(..), GraphRep, Context', USGr,
    defaultGraphSize, emptyN,
    -- * Utilities
    removeDel
) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Monad
import qualified Data.Graph.Inductive.PatriciaTree as PT

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.IO.Class
import Data.Array
import Data.Primitive.Array hiding (Array)
import GHC.Exts (Int(..))
import GHC.Prim (sizeofMutableArray#)
import Data.Foldable (foldr)
import Prelude hiding (foldr)


----------------------------------------------------------------------
-- GRAPH REPRESENTATION
----------------------------------------------------------------------

newtype SGr s a b = SGr (GraphRep s a b)

type GraphRep s a b = (Int,Array Node (Context' a b),MutableArray s Bool)
type Context'   a b = Maybe (Adj b,a,Adj b)

type USGr s = SGr s () ()

sizeofMutableArray :: MutableArray s a -> Int
sizeofMutableArray (MutableArray a) = I# (sizeofMutableArray# a)

-- GraphM
--
instance (PrimMonad m, s ~ PrimState m) => GraphM m (SGr s) where
  emptyM = emptyN defaultGraphSize
  isEmptyM g = do {SGr (n,_,_) <- g; return (n==0)}
  matchM v g = do g'@(SGr (n,a,m)) <- g
                  case a!v of
                    Nothing -> return (Nothing,g')
                    Just (pr,l,su) ->
                       do b <- readArray m v
                          if b then return (Nothing,g') else
                             do s  <- removeDel m su
                                p' <- removeDel m pr
                                let p = filter ((/=v).snd) p'
                                writeArray m v True
                                return (Just (p,v,l,s),SGr (n-1,a,m))
  mkGraphM vs es = do m <- newArray n False
                      return (SGr (n,pr,m))
          where nod  = array bnds (map (\(v,l)->(v,Just ([],l,[]))) vs)
                su   = accum addSuc nod (map (\(v,w,l)->(v,(l,w))) es)
                pr   = accum addPre su (map (\(v,w,l)->(w,(l,v))) es)
                bnds = (minimum vs',maximum vs')
                vs'  = map fst vs
                n    = length vs
                addSuc (Just (p,l',s)) (l,w) = Just (p,l',(l,w):s)
                addSuc Nothing _ = error "mkGraphM (SGr): addSuc Nothing"
                addPre (Just (p,l',s)) (l,w) = Just ((l,w):p,l',s)
                addPre Nothing _ = error "mkGraphM (SGr): addPre Nothing"
  labNodesM g = do (SGr (_,a,m)) <- g
                   let getLNode vs (_,Nothing)      = return vs
                       getLNode vs (v,Just (_,l,_)) =
                           do b <- readArray m v
                              return (if b then vs else (v,l):vs)
                   foldM getLNode [] (assocs a)

defaultGraphSize :: Int
defaultGraphSize = 100

emptyN :: PrimMonad m => Int -> m (SGr (PrimState m) a b)
emptyN n = do m <- newArray n False
              return (SGr (0,array (1,n) [(i,Nothing) | i <- [1..n]],m))

----------------------------------------------------------------------
-- UTILITIES
----------------------------------------------------------------------

-- | filter list (of successors\/predecessors) through a boolean ST array
-- representing deleted marks
removeDel :: PrimMonad m => MutableArray (PrimState m) Bool -> Adj b -> m (Adj b)
removeDel m = filterM (\(_,v)->do {b<-readArray m v;return (not b)})
