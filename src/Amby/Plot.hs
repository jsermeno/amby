{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Amby.Plot
  ( plot
  , plotEq
  , getEC
  , save
  , saveSvg

  , cairoDefSave
  , diagramsDefSave
  ) where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Vector.Generic as G
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Data.Colour as Colour
import Data.Default.Class
import Graphics.Rendering.Chart.Easy hiding (plot)
import qualified Graphics.Rendering.Chart.Easy as Chart
import qualified Graphics.Rendering.Chart.Backend.Cairo as Cairo
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Diagrams
import Statistics.Sample.KernelDensity.Simple as Stats
import Statistics.Sample as Stats

import Amby.Types
import Amby.Numeric

--------------------------
-- Instance declarations
--------------------------

instance AmbyContainer (V.Vector Double) Double where
  plot = plotVec
  plotEq = plotEqVec
  distPlot = distPlotVec

instance AmbyContainer (U.Vector Double) Double where
  plot = plotVec
  plotEq = plotEqVec
  distPlot = distPlotVec

instance (Real a) => AmbyContainer [a] a where
  plot :: [a] -> [a] -> AmbyChart ()
  plot x y = plotList $ L.zipWith (\a b -> (realToFrac a, realToFrac b)) x y

  plotEq :: [a] -> (a -> a) -> AmbyChart ()
  plotEq x fn = plotList
    $ L.zipWith (\a b -> (realToFrac a, realToFrac b)) x
    $ map fn x

  distPlot :: [a] -> AmbyChart ()
  distPlot xs =
      plotHistogram numBins valsVec
    where
      valsVec = (V.map realToFrac . V.fromList) xs
      numBins = min 50 $ freedmanDiaconisBins valsVec

plotVec :: (G.Vector v Double, G.Vector v (Double, Double))
        => v Double -> v Double -> AmbyChart ()
plotVec x y = plotList $ G.toList (G.zip x y)

plotEqVec :: (G.Vector v Double, G.Vector v (Double, Double))
          => v Double -> (Double -> Double)
          -> AmbyChart ()
plotEqVec x fn = plotList $ G.toList (G.zip x (G.map fn x))

distPlotVec :: (G.Vector v Double) => v Double -> AmbyChart ()
distPlotVec xs =
    plotHistogram numBins xs
  where
    numBins = min 50 $ freedmanDiaconisBins xs

plotHistogram :: (G.Vector v Double) => Int -> v Double -> AmbyChart ()
plotHistogram numBins plotVals = do
    layout <- takeLayout
    let minX = G.minimum plotVals
        maxX = G.maximum plotVals
        iqr = interquartileRange plotVals
        std = Stats.stdDev plotVals
        n = fromIntegral $ G.length plotVals
        a = min std (iqr / 1.34)
        bw = 1.059 * a * n ** ((-1) / 5)
        clipLeft = minX - 3 * bw
        clipRight = maxX + 3 * bw
        gridSize = 2 ^ (ceiling (log (max 512 n) / log 2) :: Int)
        xVals = Points $ linspace clipLeft clipRight gridSize
        kdeResult = Stats.estimatePDF Stats.gaussianKernel bw plotVals xVals
        kdeVals = U.toList $ U.zip (Stats.fromPoints xVals) kdeResult
    putLayout $ do
      layout
      nextColor <- takeColor
      histPlot nextColor
      linePlot nextColor kdeVals
  where
    histPlot :: AlphaColour Double -> EC (Layout Double Double) ()
    histPlot color = Chart.plot $ fmap histToPlot $ liftEC $ do
      plot_hist_bins .= numBins
      plot_hist_drop_lines .= True
      plot_hist_values .= G.toList plotVals
      plot_hist_line_style . line_width .= 2.5
      plot_hist_line_style . line_color .= Colour.dissolve 0.6 color
      plot_hist_fill_style . fill_color .= Colour.dissolve 0.4 color

plotList :: [(Double, Double)] -> AmbyChart ()
plotList plotVals = do
    layout <- takeLayout
    putLayout $ do
      layout
      nextColor <- takeColor
      linePlot nextColor plotVals

linePlot :: AlphaColour Double -> [(Double, Double)] -> EC (Layout Double Double) ()
linePlot color plotVals = Chart.plot $ liftEC $ do
  plot_lines_values .= [plotVals]
  plot_lines_style . line_width .= 2.5
  plot_lines_style . line_color .= color

--------------------------
-- Rendering
--------------------------

cairoDefSave :: FilePath
cairoDefSave = ".__amby.png"

diagramsDefSave :: FilePath
diagramsDefSave = ".__amby.svg"

getEC :: AmbyChart () -> EC (Layout Double Double) ()
getEC compute = getLayout $ execState compute def

getState :: AmbyChart () -> AmbyState
getState compute = execState compute def

-- | Quick render.
-- Short-hand to render to png file using Cairo backend.
save :: AmbyChart () -> IO ()
save chart = Cairo.toFile
    def { Cairo._fo_size = newSize }
    cairoDefSave
    (getLayout st)
  where
    st = getState chart
    newSize = getSize st

-- | Short-hand to render to svg using Cairo backend
saveSvg :: AmbyChart () -> IO ()
saveSvg chart = Diagrams.toFile
    def { Diagrams._fo_size = join (***) fromIntegral newSize }
    diagramsDefSave
    (getLayout st)
  where
    st = getState chart
    newSize = getSize st
