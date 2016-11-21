{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Amby.Plot
  ( getEC
  , save
  , saveSvg

  , cairoDefSave
  , diagramsDefSave
  ) where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Maybe as Maybe
import Data.Tuple (swap)
import qualified Data.Vector.Generic as G
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Control.Lens
import Data.Colour as Colour
import Data.Default.Class
import Graphics.Rendering.Chart.Easy (Layout, EC)
import qualified Graphics.Rendering.Chart.Easy as Chart
import qualified Graphics.Rendering.Chart.Backend.Cairo as Cairo
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Diagrams
import Safe (maximumMay)
import qualified Statistics.Sample.KernelDensity.Simple as Stats
import qualified Statistics.Sample as Stats

import Amby.Types
import Amby.Numeric
import Amby.Compatibility.HistogramPlot

--------------------------
-- Instance declarations
--------------------------

instance AmbyContainer (V.Vector Double) where
  type Value (V.Vector Double) = Double
  plot = plotVec
  plotEq = plotEqVec
  distPlot = distPlotVec
  distPlot' = distPlotVec'
  kdePlot = kdePlotVec
  kdePlot' = kdePlotVec'
  rugPlot = rugPlotVec
  rugPlot' = rugPlotVec'

instance AmbyContainer (U.Vector Double) where
  type Value (U.Vector Double) = Double
  plot = plotVec
  plotEq = plotEqVec
  distPlot = distPlotVec
  distPlot' = distPlotVec'
  kdePlot = kdePlotVec
  kdePlot' = kdePlotVec'
  rugPlot = rugPlotVec
  rugPlot' = rugPlotVec'

instance (Real a) => AmbyContainer [a] where
  type Value [a] = a
  plot :: [a] -> [a] -> AmbyChart ()
  plot x y = plotList $ L.zipWith (\a b -> (realToFrac a, realToFrac b)) x y

  plotEq :: [a] -> (a -> a) -> AmbyChart ()
  plotEq x fn = plotList
    $ L.zipWith (\a b -> (realToFrac a, realToFrac b)) x
    $ map fn x

  distPlot :: [a] -> State DistPlotOpts () -> AmbyChart ()
  distPlot xs optsState = distPlotVec valsVec optsState
    where
      valsVec = (V.map realToFrac . V.fromList) xs

  distPlot' :: [a] -> AmbyChart ()
  distPlot' xs = distPlot xs $ return ()

  kdePlot :: [a] -> State KdePlotOpts () -> AmbyChart ()
  kdePlot xs optsState = kdePlotVec valsVec optsState
    where
      valsVec = (V.map realToFrac . V.fromList) xs

  kdePlot' :: [a] -> AmbyChart ()
  kdePlot' xs = kdePlot xs $ return ()

  rugPlot :: [a] -> State RugPlotOpts () -> AmbyChart ()
  rugPlot xs optsState = rugPlotVec valsVec optsState
    where
      valsVec = (V.map realToFrac . V.fromList) xs

  rugPlot' :: [a] -> AmbyChart ()
  rugPlot' xs = rugPlot xs $ return ()

--------------------------
-- Generic vec plotters
--------------------------

plotVec :: (G.Vector v Double, G.Vector v (Double, Double))
        => v Double -> v Double -> AmbyChart ()
plotVec x y = plotList $ G.toList (G.zip x y)

plotEqVec :: (G.Vector v Double, G.Vector v (Double, Double))
          => v Double -> (Double -> Double)
          -> AmbyChart ()
plotEqVec x fn = plotList $ G.toList (G.zip x (G.map fn x))

distPlotVec :: (G.Vector v Double) => v Double -> State DistPlotOpts ()
            -> AmbyChart ()
distPlotVec xs optsState = plotDistribution xs $ execState optsState def

distPlotVec' :: (G.Vector v Double) => v Double -> AmbyChart ()
distPlotVec' xs = distPlotVec xs $ return ()

kdePlotVec :: (G.Vector v Double) => v Double -> State KdePlotOpts ()
           -> AmbyChart ()
kdePlotVec xs optsState = do
  layout <- takeLayout
  putLayout $ do
    layout
    nextColor <- Chart.takeColor
    plotKde nextColor xs $ execState optsState def

kdePlotVec' :: (G.Vector v Double) => v Double -> AmbyChart ()
kdePlotVec' xs = kdePlotVec xs $ return ()

rugPlotVec :: (G.Vector v Double)
           => v Double -> State RugPlotOpts ()
           -> AmbyChart ()
rugPlotVec xs optsState = do
  layout <- takeLayout
  putLayout $ do
    layout
    nextColor <- Chart.takeColor
    plotRug nextColor xs $ execState optsState def

rugPlotVec' :: (G.Vector v Double)
            => v Double -> AmbyChart ()
rugPlotVec' xs = rugPlotVec xs $ return ()

--------------------------
-- Generic plotters
--------------------------

plotDistribution :: (G.Vector v Double) => v Double -> DistPlotOpts -> AmbyChart ()
plotDistribution xs opts = do
    layout <- takeLayout
    putLayout $ do
      layout
      nextColor <- Chart.takeColor
      when (opts ^. hist) $ plotHist opts nextColor numBins xs
      when (opts ^. kde) $ plotKde nextColor xs $ def
        & bw .~ (opts ^. bw)
        & shade .~ (opts ^. shade)
        & gridsize .~ (opts ^. gridsize)
        & axis .~ (opts ^. axis)
      when (opts ^. rug) $ plotRug nextColor xs $ def
        & height .~ (opts ^. rugHeight)
        & axis .~ (opts ^. axis)
  where
    numBins = if opts ^. bins /= 0
      then opts ^. bins
      else min 50 $ freedmanDiaconisBins xs

plotList :: [(Double, Double)] -> AmbyChart ()
plotList xs = do
    layout <- takeLayout
    putLayout $ do
      layout
      nextColor <- Chart.takeColor
      plotLine nextColor 2.5 xs

--------------------------
-- Composable plotters
--------------------------

plotHist :: (G.Vector v Double) => DistPlotOpts -> AlphaColour Double -> Int -> v Double
         -> EC (Layout Double Double) ()
plotHist opts color numBins xs =
  Chart.plot $ fmap histToPlot $ Chart.liftEC $ do
    plot_hist_bins .= numBins
    plot_hist_drop_lines .= True
    plot_hist_values .= G.toList xs
    plot_hist_line_style . Chart.line_width .= 2.5
    plot_hist_line_style . Chart.line_color .= Colour.dissolve 0.6 color
    plot_hist_fill_style . Chart.fill_color .= Colour.dissolve 0.4 color
    plot_hist_vertical .= (opts ^. axis == Y)

plotRug :: (G.Vector v Double)
        => AlphaColour Double -> v Double -> RugPlotOpts
        -> EC (Layout Double Double) ()
plotRug color xs opts = do
      maxMay <- getPlotMaxHeightMay
      h <- calcRugHeight maxMay
      G.foldM (plotSingleLine h) () xs
      updatePlotHeight maxMay
    where
      getPlotMaxHeightMay :: EC (Layout Double Double) (Maybe Double)
      getPlotMaxHeightMay = do
        layout <- get
        layout ^.. Chart.layout_plots . each . Chart.plot_all_points
          & concatMap (if opts ^. axis == X then snd else fst)
          & maximumMay
          & return

      calcRugHeight :: (Maybe Double) -> EC (Layout Double Double) Double
      calcRugHeight maxMay =
        return $ (Maybe.fromMaybe 1.0 maxMay) * (opts ^. height)

      plotSingleLine :: Double -> () -> Double -> EC (Layout Double Double) ()
      plotSingleLine h _ x = if opts ^. axis == X
        then plotLine color 1.2 [(x, 0), (x, h)]
        else plotLine color 1.2 [(0, x), (h, x)]

      updatePlotHeight :: (Maybe Double)
                       -> EC (Layout Double Double) ()
      updatePlotHeight maxMay = do
        layout2 <- get
        layout2 & Chart.layout_plots . mapped . Chart.plot_all_points
          %~ updatePlotPoints maxMay
          & put

      updatePlotPoints :: (Maybe Double) -> ([Double], [Double])
                       -> ([Double], [Double])
      updatePlotPoints maxMay (as, bs) =
        if | maxMay == Nothing && opts ^. axis == X -> (0.0:as, 1.0:bs)
           | maxMay == Nothing && opts ^. axis == Y -> (1.0:as, 0.0:bs)
           | otherwise -> (as, bs)

plotKde :: (G.Vector v Double) => AlphaColour Double -> v Double -> KdePlotOpts
        -> EC (Layout Double Double) ()
plotKde color xs opts = do
    plotLine color 2.5 kdeVals
    when (opts ^. shade) $ plotShade color kdeVals
  where
    minX = G.minimum xs
    maxX = G.maximum xs
    iqr = interquartileRange xs
    std = Stats.stdDev xs
    n = fromIntegral $ G.length xs
    a = min std (iqr / 1.34)
    bandwidth = if | opts ^. bw == Scott -> 1.059 * a * n ** ((-1) / 5)
                   | (BwScalar b) <- opts ^. bw -> b
    clipLeft = minX - 3 * bandwidth
    clipRight = maxX + 3 * bandwidth
    sizeEstimate = fromIntegral $ opts ^. gridsize :: Double
    numEstimates = 2 ^ (ceiling (log sizeEstimate / log 2) :: Int)
    xVals = Stats.Points $ linspace clipLeft clipRight numEstimates
    kdeResult = Stats.estimatePDF Stats.gaussianKernel bandwidth xs xVals
    kdeVals' = U.zip (Stats.fromPoints xVals) kdeResult
    kdeVals = if opts ^. axis == Y
      then U.toList $ U.map swap kdeVals'
      else U.toList kdeVals'

plotLine :: AlphaColour Double -> Double -> [(Double, Double)]
         -> EC (Layout Double Double) ()
plotLine color lineWidth xs = Chart.plot $ Chart.liftEC $ do
  Chart.plot_lines_values .= [xs]
  Chart.plot_lines_style . Chart.line_width .= lineWidth
  Chart.plot_lines_style . Chart.line_color .= color

plotShade :: AlphaColour Double -> [(Double, Double)]
          -> EC (Layout Double Double) ()
plotShade color xs = Chart.plot $ Chart.liftEC $ do
  Chart.plot_fillbetween_values .= (xs & mapped . _2 %~ \y -> (0, y))
  Chart.plot_fillbetween_style .= Chart.solidFillStyle (Colour.dissolve 0.25 color)

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
