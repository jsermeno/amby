{-# LANGUAGE FlexibleContexts #-}
module Amby.Numeric
  ( contDistrDomain
  , contDistrRange
  , linspace
  , arange
  ) where

import Data.Either.Combinators
import Data.Scientific
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Statistics.Distribution

contDistrDomain :: (ContDistr d) => d -> Int -> U.Vector Double
contDistrDomain d num = linspace (quantile d 0.0001) (quantile d 0.9999) num

contDistrRange :: (ContDistr d) => d -> U.Vector Double -> U.Vector Double
contDistrRange d x = U.map (density d) x

linspace :: Double -> Double -> Int -> U.Vector Double
linspace start stop num
  | num < 0 = error ("Number of samples, " ++ show num ++ ", must be non-negative.")
  | num == 0 || num == 1 = addStart $ U.generate num ((* delta) . fromIntegral)
  | otherwise = addStart $ U.generate num ((* step) . fromIntegral)
  where
    delta = stop - start
    step = delta / fromIntegral (num - 1)
    addStart = U.map (realToFrac . (+ start))

arange :: Double -> Double -> Double -> U.Vector Double
arange start stop step = U.fromList [start,(start + step)..stop]
