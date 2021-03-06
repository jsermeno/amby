{-# LANGUAGE FlexibleContexts #-}
---------------------------------------------------------------------------------
-- |
-- Module       : Amby.Numeric
-- Copyright    : (C) 2016 Justin Sermeno
-- License      : BSD3
-- Maintainer   : Justin Sermeno
--
-- Functions for working with numerical ranges and statistics.
---------------------------------------------------------------------------------
module Amby.Numeric
  ( -- * Ranges
    contDistrDomain
  , contDistrRange
  , linspace
  , arange
  , random

  -- * Frequencies
  , scoreAtPercentile
  , interquartileRange
  , freedmanDiaconisBins

  ) where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

import Statistics.Distribution
import qualified Statistics.Quantile as Stats
import System.Random.MWC (withSystemRandom, asGenST)

-- $setup
-- >>> let demoData = V.fromList $ concat $ zipWith replicate [5, 4, 3, 7] [0..3]

---------------------
-- Ranges
---------------------

-- | @contDistrDomain d n@ generates a domain of 'n' evenly spaced points
-- for the continuous distribution 'd'.
contDistrDomain :: (ContDistr d) => d -> Int -> U.Vector Double
contDistrDomain d num = linspace (quantile d 0.0001) (quantile d 0.9999) num

-- | @contDistrRange d xs@ generates the pdf value of the continious distribution
-- 'd' for each value in 'xs'.
contDistrRange :: (ContDistr d) => d -> U.Vector Double -> U.Vector Double
contDistrRange d x = U.map (density d) x

-- | @linspace s e n@ generates 'n' evenly spaced values between ['s', 'e'].
linspace :: Double -> Double -> Int -> U.Vector Double
linspace start stop num
  | num < 0 = error ("Number of samples, " ++ show num ++ ", must be non-negative.")
  | num == 0 || num == 1 = addStart $ U.generate num ((* delta) . fromIntegral)
  | otherwise = addStart $ U.generate num ((* step) . fromIntegral)
  where
    delta = stop - start
    step = delta / fromIntegral (num - 1)
    addStart = U.map (realToFrac . (+ start))

-- | @arange s e i@ generates numbers between ['s', 'e'] spaced by amount 'i'.
-- 'arange' is the equivalent of haskell's range notation except that it generates
-- a 'Vector'. As a result, the last element may be greater than less than, or
-- greater than the stop point.
arange :: Double -> Double -> Double -> U.Vector Double
arange start stop step = U.fromList [start,(start + step)..stop]

-- | Generates an unboxed vectors of random numbers from a distribution
-- that is an instance of 'ContGen'. This function is meant for ease of use
-- and is expensive.
random :: (ContGen d) => d -> Int -> IO (U.Vector Double)
random d n = withSystemRandom . asGenST $ \gen ->
  U.replicateM n $ genContVar d gen

---------------------
-- Frequencies
---------------------

-- | @scoreAtPercentile xs p@ calculates the score at percentile 'p'.
--
-- Examples:
--
-- >>> let a = arange 0 99 1
--
-- >>> scoreAtPercentile a 50
-- 49.5
scoreAtPercentile :: (G.Vector v Double) => v Double -> Int -> Double
scoreAtPercentile xs p
  | G.length xs == 0 =
    modErr "scoreAtPercentile" "Cannot find percentile of empyt list"
  | otherwise = Stats.weightedAvg p 100 xs

-- | Calculate the interquartile range.
--
-- Examples:
--
-- >>> interquartileRange demoData
-- 2.5
interquartileRange :: (G.Vector v Double) => v Double -> Double
interquartileRange xs = scoreAtPercentile xs 75 - scoreAtPercentile xs 25
{-# SPECIALIZE interquartileRange :: U.Vector Double -> Double #-}
{-# SPECIALIZE interquartileRange :: V.Vector Double -> Double #-}

-- | Estimate a good default bin size.
--
-- Examples:
--
-- >>> freedmanDiaconisBins demoData
-- 2
freedmanDiaconisBins :: (G.Vector v Double) => v Double -> Int
freedmanDiaconisBins xs =
    if h == 0
      then floor $ sqrt $ (fromIntegral n :: Double)
      else ceiling $ (G.maximum xs - G.minimum xs) / h
  where
    n = G.length xs
    h = 2 * interquartileRange xs / fromIntegral n ** (1 / 3)

modErr :: String -> String -> a
modErr f err = error
  $ showString "Amby.Numeric."
  $ showString f
  $ showString ": " err
