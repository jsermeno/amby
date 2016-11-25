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
  , catSize
  , filterByCategory
  , filterByCategory2
  , filterByCategories
  , listToRose
  , zipRoseLeaves
  , roseLengths
  , roseToList
  , nextLevelAreLeaves
  , groupSortRose
  ) where

import qualified Data.Foldable as Foldable
import qualified Data.List.Extra as L
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple (swap)
import qualified Data.Vector.Generic as G

import Control.Lens

data Category = Category
  { _categoryOrder :: [Int]
  , _categoryValues :: [Int]
  , _categoryTable :: Map Int String
  }
  | DefaultCategory
  deriving (Show, Eq)

-- | Get list of category labels in order.
getCategoryLabels :: Category -> [String]
getCategoryLabels c = map (labelMap Map.!) intOrder
  where
    labelMap = _categoryTable c
    intOrder = _categoryOrder c

-- | Segment vector by category
filterByCategory :: G.Vector v Double => v Double -> Category -> [v Double]
filterByCategory xs cat =
    map (G.fromList . map leafToValue)
  $ map roseChildList
  $ roseChildList
  $ filterByCategories xs [cat]

filterByCategory2 :: G.Vector v Double => v Double -> Category -> Category
                  -> [[v Double]]
filterByCategory2 xs cat1 cat2 =
    map (map (G.fromList . map leafToValue))
  $ map (map roseChildList)
  $ map roseChildList
  $ roseChildList
  $ filterByCategories xs [cat1, cat2]

filterByCategories :: G.Vector v Double => v Double -> [Category] -> Rose Double
filterByCategories vec cs = go cs xs
  where
    xs = listToRose $ G.toList vec
    go [] r = r
    go (c:rest) r
      | roseLength r /= length (_categoryValues c) =
        modErr "filterByCategories" "Category and data must be of the same size"
      | otherwise = go rest segmented
      where
        segmented = groupSortRose $ zipRoseLeaves r (_categoryValues c)

data Rose a where
  Leaf :: Show a => a -> Rose a
  Rose :: Show a => Int -> [Rose a] -> Rose a
deriving instance Show (Rose a)

-- | Create initial layer of rose tree.
--
-- Examples:
--
-- >>> listToRose [1..5]
-- Rose 5 [Leaf 1,Leaf 2,Leaf 3,Leaf 4,Leaf 5]
listToRose :: Show a => [a] -> Rose a
listToRose xs = Rose (length xs) (map Leaf xs)

-- | Convert rose tree to list.
--
-- Examples:
--
-- >>> let r = listToRose [1..3]
-- >>> roseToList r
-- [1,2,3]
roseToList :: Rose a -> [a]
roseToList (Rose _ rs) = concatMap roseToList rs
roseToList (Leaf l) = [l]

-- | Group and sort rose tree based in first element in tuple.
--
-- Examples:
--
-- >>> let r = listToRose [1..5]
-- >>> let cat1 = [1, 2, 1, 2, 1]
-- >>> let cat2 = [3, 4, 1, 3, 2]
-- >>> groupSortRose (zipRoseLeaves r cat1)
-- Rose 5 [Rose 4 [Leaf 1,Leaf 3,Leaf 5],Rose 2 [Leaf 2,Leaf 4]]
--
-- >>> let r' = groupSortRose (zipRoseLeaves r cat1)
-- >>> groupSortRose (zipRoseLeaves r' cat2)
-- Rose 5 [Rose 4 [],Rose 2[]]
groupSortRose :: Show a => Rose (Int, a) -> Rose a
groupSortRose r@(Rose i rs)
  | nextLevelAreLeaves r =
      Rose i
    $ map (listToRose . (map snd))
    $ L.groupSortOn fst
    $ map leafToValue rs
  | otherwise = Rose i (map groupSortRose rs)
groupSortRose _ = modErr "groupSortRose" "Invalid rose tree shape"

-- | Zip operation on leaves of rose tree.
--
-- Examples:
--
-- >>> let r = listToRose [1..3]
-- >>> zipRoseLeaves r [1, 2, 1]
-- Rose 3 [Leaf (1,1),Leaf (2,2),Leaf (1,3)]
zipRoseLeaves :: Rose a -> [Int] -> Rose (Int, a)
zipRoseLeaves (Rose n rs) cs = Rose n zipped
  where
    intChunks = splitChunks cs (roseLengths rs)
    zipped = zipWith zipRoseLeaves rs intChunks
zipRoseLeaves (Leaf l) [c] = Leaf (c, l)
zipRoseLeaves _ _ = modErr "zipRoseLeafs" "Invalid branch"

roseLengths :: [Rose a] -> [Int]
roseLengths = map roseLength

roseLength :: Rose a -> Int
roseLength (Rose i _) = i
roseLength (Leaf _) = 1

nextLevelAreLeaves :: Rose a -> Bool
nextLevelAreLeaves (Rose _ rs) = areLeaves rs
nextLevelAreLeaves _ = False

areLeaves :: [Rose a] -> Bool
areLeaves [] = True
areLeaves ((Leaf _):_) = True
areLeaves _ = False

leafToValue :: Rose a -> a
leafToValue (Leaf l) = l
leafToValue _ = modErr "leafToValue" "Rose is not a leaf"

roseChildList :: Rose a -> [Rose a]
roseChildList (Rose _ rs) = rs
roseChildList _ = modErr "roseChildList" "Invalid rose tree shape"

-- | Split list into chunks.
--
-- Examples:
--
-- splitChunks [0, 1, 2, 3, 4] [2, 3]
-- [[0,1],[2,3,4]]
splitChunks :: [a] -> [Int] -> [[a]]
splitChunks xs chunkList
  | length xs /= L.foldl' (+) 0 chunkList =
    modErr "splitChunks" "Chunks must sum to list length"
  | otherwise = go chunkList xs
  where
    go [] _ = []
    go (c:cs) ds = take c ds : go cs (drop c ds)

-- | Find number of distinct elements in a category.
--
-- Examples:
--
-- >>> catSize $ toCat ["dog", "cat", "dog"]
-- 2
catSize :: Category -> Int
catSize DefaultCategory = 1
catSize cat = Map.size (_categoryTable cat)

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
    }
  where
    toString = filter (/= '"') . show
    orderedPairs = zip order [0..]
    idxMap = Map.fromList orderedPairs
    replaceWithIdx x = idxMap Map.! x

modErr :: String -> String -> a
modErr f err = error
  $ showString "Amby.Container."
  $ showString f
  $ showString ": " err
