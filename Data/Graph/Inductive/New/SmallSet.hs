{- |
   Module      : Data.Graph.Inductive.New.SmallSet
   Description : Small sets
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com

   A module for sets of small size.

 -}
module Data.Graph.Inductive.New.SmallSet
  ( Set
  , toList
  , fromList
  , empty
  , null
  , size
  , map
  , insert
  , member
  , notMember
  , delete
  , union
  , unions
  , difference
  , (\\)
  ) where

import           Prelude hiding (map, null)
import qualified Prelude as P

import Data.List (concatMap, sort)

--------------------------------------------------------------------------------

newtype Set a = Set { toList :: [a] }
  deriving (Eq, Ord)

instance (Show a) => Show (Set a) where
  showsPrec d (Set as) = showParen (d > 10) $
                           showString "fromList "
                           . shows as

instance (Ord a, Read a) => Read (Set a) where
  readsPrec p = readParen (p > 10) $ \r -> do
    ("fromList", s) <- lex r
    (as, t) <- reads s
    return (fromList as, t)

fromList :: (Ord a) => [a] -> Set a
fromList = Set . sort

empty :: Set a
empty = Set []

null :: Set a -> Bool
null = P.null . toList

size :: Set a -> Int
size = length . toList

map :: (Ord b) => (a -> b) -> Set a -> Set b
map f = fromList . P.map f . toList

insert :: (Ord a) => a -> Set a -> Set a
insert v = Set . go . toList
  where
    -- Can't use Data.List.insert as it results in duplicates
    go []         = [v]
    go as@(a:as') = case compare a v of
                      LT -> a : go as'
                      EQ -> as
                      GT -> v : as

member :: (Ord a) => a -> Set a -> Bool
member v = go . toList
  where
    go [] = False
    go (a:as) = case compare a v of
                  LT -> go as
                  EQ -> True
                  GT -> False

notMember :: (Ord a) => a -> Set a -> Bool
notMember = (not .) . member

delete :: (Ord a) => a -> Set a -> Set a
delete v = Set . go . toList
  where
    go []         = []
    go as@(a:as') = case compare a v of
                      LT -> a : go as'
                      EQ -> as'
                      GT -> as

union :: (Ord a) => Set a -> Set a -> Set a
union s1 s2 = unions [s1, s2]

unions :: (Ord a) => [Set a] -> Set a
unions = fromList . concatMap toList
-- Inefficient, but since they should all be small...

difference :: (Ord a) => Set a -> Set a -> Set a
difference (Set as) (Set bs) = Set (go as bs)
  where
    go []         _          = []
    go xs         []         = xs
    go xs@(x:xs') ys@(y:ys') = case compare x y of
                                 LT -> x : go xs' ys
                                 EQ -> go xs' ys'
                                 GT -> go xs ys'

(\\) :: (Ord a) => Set a -> Set a -> Set a
(\\) = difference

infixl 9 \\
