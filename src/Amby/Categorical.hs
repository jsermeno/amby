{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Amby.Categorical
  (
  -- * Types
    Category(..)

  -- * Methods
  , toCat
  , toCatOrdered
  , getCategoryLabels
  , getCategoryLabelFromVal
  , getCategoryOrder
  , getCategoryList
  , catSize
  , catValsLength
  , filterMask
  , groupByCategory
  , groupCategoryBy
  , getGroupAt
  , getGroupWithFilterMask
  ) where

import qualified Data.Foldable as Foldable
import qualified Data.List.Extra as L
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple (swap)

import Control.Lens
import Safe

data Category = Category
  { _categoryOrder :: [Int]
  , _categoryValues :: [Int]
  , _categoryTable :: Map Int String
  , _categoryGroups :: [[Int]]
  }
  | DefaultCategory
  deriving (Show, Eq)

-- | Get list of category labels in order.
getCategoryLabels :: Category -> [String]
getCategoryLabels c = map lookupLabel intOrder
  where
    labelMap = _categoryTable c
    intOrder = _categoryOrder c
    lookupLabel i = case Map.lookup i labelMap of
      Just a -> a
      Nothing -> modErr "getCategoryLabels" "Value does not exist in category"

getCategoryLabelFromVal :: Category -> Int -> String
getCategoryLabelFromVal c i = case Map.lookup i (_categoryTable c) of
  Just a -> a
  Nothing -> modErr "getCategoryLabelFromVal" "Value does not exist in category"

getCategoryList :: Category -> [Int]
getCategoryList = _categoryValues

getCategoryOrder :: Category -> [Int]
getCategoryOrder = _categoryOrder

-- | Group by category.
--
-- Examples:
--
-- >>> groupByCategory [1..5] (toCat [1, 1, 3, 2, 4])
-- [[1,2],[4],[3],[5]]
groupByCategory :: [a] -> Category -> [[a]]
groupByCategory xs cat
  | length xs /= length (getCategoryList cat) =
    modErr "groupByCategory" "Can only group data with equivalent sized category"
  | otherwise =
      map (map fst)
    $ L.groupSortOn snd
    $ zip xs (_categoryValues cat)

-- | Group category internally
groupCategoryBy :: Category -> Category -> Category
groupCategoryBy cat grouper = cat
    { _categoryGroups = groupByCategory dat grouper
    }
  where
    dat = _categoryValues cat

getGroupAt :: Category -> Int -> Category
getGroupAt cat i = cat
    { _categoryValues = groupValues
    }
  where
    groupValues = getGroupValues cat i

getGroupWithFilterMask :: Category -> Int -> [Bool] -> Category
getGroupWithFilterMask cat i mask = cat
    { _categoryValues = filterMask groupValues mask
    }
  where
    groupValues = getGroupValues cat i

getGroupValues :: Category -> Int -> [Int]
getGroupValues cat i = groupValues
  where
    dat = _categoryGroups cat
    groupValues = case dat `atMay` i of
      Just a -> a
      Nothing -> modErr "getGroupAt" ("No group at index: " ++ show i)

-- | Filter list based on another equal sized list of bools.
--
-- Examples:
--
-- >>>  filterMask [1..5] [True, False, True, False, False]
-- [1,3]
filterMask :: [a] -> [Bool] -> [a]
filterMask xs ts
  | length xs /= length ts =
    modErr "filterMask" "mask length must match data length."
  | otherwise = (map fst . filter snd . zip xs) ts

-- | Find number of distinct elements in a category.
--
-- Examples:
--
-- >>> catSize $ toCat ["dog", "cat", "dog"]
-- 2
catSize :: Category -> Int
catSize DefaultCategory = 1
catSize cat = Map.size (_categoryTable cat)

-- | Find the number of elements in a category.
catValsLength :: Category -> Int
catValsLength c = length $ getCategoryList c

-- | Convert 'Foldable' into a 'Category'.
toCat :: (Foldable f, Ord a, Show a) => f a -> Category
toCat f = listToCat list order
  where
    list = Foldable.toList f
    order = (L.sort . L.nubOrd) list

toCatOrdered :: (Foldable f, Ord a, Show a) => f a -> f a -> Category
toCatOrdered f order = listToCat (Foldable.toList f) (Foldable.toList order)

listToCat :: (Ord a, Show a) => [a] -> [a] -> Category
listToCat xs order
  | L.length (L.nubOrd order) /= L.length order =
    modErr "listToCat" "Order cannot contain duplicates"
  | otherwise = Category
    { _categoryOrder = map snd orderedPairs
    , _categoryValues = map replaceWithIdx xs
    , _categoryTable = map swap orderedPairs
      & mapped . _2 %~ toString
      & Map.fromList
    , _categoryGroups = []
    }
  where
    toString = filter (/= '"') . show
    orderedPairs = zip order [0..]
    idxMap = Map.fromList orderedPairs
    replaceWithIdx x = case Map.lookup x idxMap of
      Just a -> a
      Nothing -> modErr "listToCat" "No idx created for category value"

modErr :: String -> String -> a
modErr f err = error
  $ showString "Amby.Container."
  $ showString f
  $ showString ": " err
