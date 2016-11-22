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

import Amby.Compatibility.HistogramPlot
import Amby.Numeric
import Amby.Theme (toColour)
import Amby.Types

--------------------------
-- Instance declarations
--------------------------

instance AmbyContainer (V.Vector Double) where
  type Value (V.Vector Double) = Double
  plot = plotVec
  plot' = plotVec'
  plotEq = plotEqVec
  plotEq' = plotEqVec'
  distPlot = distPlotVec
  distPlot' = distPlotVec'
  kdePlot = kdePlotVec
  kdePlot' = kdePlotVec'
  rugPlot = rugPlotVec
  rugPlot' = rugPlotVec'

instance AmbyContainer (U.Vector Double) where
  type Value (U.Vector Double) = Double
  plot = plotVec
  plot' = plotVec'
  plotEq = plotEqVec
  plotEq' = plotEqVec'
  distPlot = distPlotVec
  distPlot' = distPlotVec'
  kdePlot = kdePlotVec
  kdePlot' = kdePlotVec'
  rugPlot = rugPlotVec
  rugPlot' = rugPlotVec'

instance (Real a) => AmbyContainer [a] where
  type Value [a] = a

  plot :: [a] -> [a] -> State PlotOpts () -> AmbyChart ()
  plot x y optsState = plotList vals opts
    where
      opts = execState optsState def
      vals = L.zipWith (\a b -> (realToFrac a, realToFrac b)) x y

  plot' :: [a] -> [a] -> AmbyChart ()
  plot' x y = plot x y $ return ()

  plotEq :: [a] -> (a -> a) -> State PlotEqOpts () -> AmbyChart ()
  plotEq x fn optsState = plotList vals $ def
      & color .~ (opts ^. color)
    where
      opts = execState optsState def
      vals = L.zipWith (\a b -> (realToFrac a, realToFrac b)) x $ map fn x

  plotEq' :: [a] -> (a -> a) -> AmbyChart ()
  plotEq' x fn = plotEq x fn $ return ()

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
        => v Double -> v Double -> State PlotOpts () -> AmbyChart ()
plotVec x y optsState = plotList vals opts
  where
    opts = execState optsState def
    vals = G.toList $ G.zip x y

plotVec' :: (G.Vector v Double, G.Vector v (Double, Double))
         => v Double -> v Double -> AmbyChart ()
plotVec' x y = plotVec x y $ return ()

plotEqVec :: (G.Vector v Double, G.Vector v (Double, Double))
          => v Double -> (Double -> Double) -> State PlotEqOpts ()
          -> AmbyChart ()
plotEqVec x fn optsState = plotList vals $ def
    & color .~ (opts ^. color)
  where
    opts = execState optsState def
    vals = G.toList $ G.zip x (G.map fn x)

plotEqVec' :: (G.Vector v Double, G.Vector v (Double, Double))
           => v Double -> (Double -> Double) -> AmbyChart ()
plotEqVec' x fn = plotEqVec x fn $ return ()

distPlotVec :: (G.Vector v Double, G.Vector v (Double, Double))
            => v Double -> State DistPlotOpts ()
            -> AmbyChart ()
distPlotVec xs optsState = plotDistribution xs $ execState optsState def

distPlotVec' :: (G.Vector v Double, G.Vector v (Double, Double))
             => v Double -> AmbyChart ()
distPlotVec' xs = distPlotVec xs $ return ()

kdePlotVec :: (G.Vector v Double) => v Double -> State KdePlotOpts ()
           -> AmbyChart ()
kdePlotVec xs optsState = do
    layout <- takeLayout
    putLayout $ do
      layout
      nextColor <- toColour (opts ^. color) <$> Chart.takeColor
      plotKde nextColor xs opts
  where
    opts = execState optsState def

kdePlotVec' :: (G.Vector v Double) => v Double -> AmbyChart ()
kdePlotVec' xs = kdePlotVec xs $ return ()

rugPlotVec :: (G.Vector v Double, G.Vector v (Double, Double))
           => v Double -> State RugPlotOpts ()
           -> AmbyChart ()
rugPlotVec xs optsState = do
    layout <- takeLayout
    putLayout $ do
      layout
      nextColor <- toColour (opts ^. color) <$> Chart.takeColor
      plotRug nextColor xs opts
  where
    opts = execState optsState def

rugPlotVec' :: (G.Vector v Double, G.Vector v (Double, Double))
            => v Double -> AmbyChart ()
rugPlotVec' xs = rugPlotVec xs $ return ()

--------------------------
-- Generic plotters
--------------------------

plotDistribution :: (G.Vector v Double, G.Vector v (Double, Double))
                 => v Double -> DistPlotOpts -> AmbyChart ()
plotDistribution xs opts = do
    layout <- takeLayout
    putLayout $ do
      layout
      nextColor <- toColour (opts ^. color) <$> Chart.takeColor
      when (opts ^. hist) $ plotHist opts nextColor numBins xs
      when (opts ^. kde) $ plotKde nextColor xs $ def
        & bw .~ (opts ^. bw)
        & shade .~ (opts ^. shade)
        & gridsize .~ (opts ^. gridsize)
        & axis .~ (opts ^. axis)
        & linewidth .~ (opts ^. kdeLinewidth)
        & cut .~ (opts ^. cut)
      when (opts ^. rug) $ plotRug nextColor xs $ def
        & height .~ (opts ^. rugHeight)
        & axis .~ (opts ^. axis)
        & linewidth .~ (opts ^. rugLinewidth)
  where
    numBins = if opts ^. bins /= 0
      then opts ^. bins
      else min 50 $ freedmanDiaconisBins xs

plotList :: [(Double, Double)] -> PlotOpts -> AmbyChart ()
plotList xs opts = do
    layout <- takeLayout
    putLayout $ do
      layout
      nextColor <- toColour (opts ^. color) <$> Chart.takeColor
      plotLine nextColor (opts ^. linewidth) xs

--------------------------
-- Composable plotters
--------------------------

plotHist :: (G.Vector v Double) => DistPlotOpts -> AlphaColour Double -> Int -> v Double
         -> EC (Layout Double Double) ()
plotHist opts c numBins xs =
  Chart.plot $ fmap histToPlot $ Chart.liftEC $ do
    plot_hist_bins .= numBins
    plot_hist_drop_lines .= True
    plot_hist_values .= G.toList xs
    plot_hist_line_style . Chart.line_width .= (opts ^. histLinewidth)
    plot_hist_line_style . Chart.line_color .= Colour.dissolve 0.5 c
    plot_hist_fill_style . Chart.fill_color .= Colour.dissolve 0.4 c
    plot_hist_vertical .= (opts ^. axis == YAxis)

plotRug :: forall v. (G.Vector v Double, G.Vector v (Double, Double))
        => AlphaColour Double -> v Double -> RugPlotOpts
        -> EC (Layout Double Double) ()
plotRug c xs opts = do
      maxMay <- getPlotMaxHeightMay
      h <- calcRugHeight maxMay
      Chart.plot $ return $ Chart.Plot
        { Chart._plot_render = renderPlotRug h
        , Chart._plot_legend = [("", const $ return ())]
        , Chart._plot_all_points = join (***) G.toList
          $ G.unzip
          $ G.concatMap (toPoints h) xs
        }
      updatePlotHeight maxMay
    where
      getPlotMaxHeightMay :: EC (Layout Double Double) (Maybe Double)
      getPlotMaxHeightMay = do
        layout <- get
        layout ^.. Chart.layout_plots . each . Chart.plot_all_points
          & concatMap (if opts ^. axis == XAxis then snd else fst)
          & maximumMay
          & return

      toPoints :: Double -> Double -> v (Double, Double)
      toPoints h x = if opts ^. axis == XAxis
        then G.fromList [(x, 0), (x, h)]
        else G.fromList [(0, x), (h, x)]

      calcRugHeight :: (Maybe Double) -> EC (Layout Double Double) Double
      calcRugHeight maxMay =
        return $ (Maybe.fromMaybe 1.0 maxMay) * (opts ^. height)

      renderPlotRug :: Double -> Chart.PointMapFn Double Double
                    -> BackendProgram ()
      renderPlotRug h pmap =
        Chart.withLineStyle lineStyle $ do
          Chart.alignStrokePath (plotPath h pmap) >>= Chart.strokePath
        where
          lineStyle = def
            & Chart.line_width .~ (opts ^. linewidth)
            & Chart.line_color .~ c

      plotPath :: Double -> Chart.PointMapFn Double Double -> Chart.Path
      plotPath h pmap =
          go (G.toList xs)
        where
          go [] = Chart.End
          go [b] = Chart.MoveTo (pt b 0) $ Chart.LineTo (pt b h) $ Chart.End
          go (b:rest) = Chart.MoveTo (pt b 0) $ Chart.LineTo (pt b h) $ go rest
          pt x y = pmap (Chart.LValue x, Chart.LValue y)

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
        if | maxMay == Nothing && opts ^. axis == XAxis -> (0.0:as, 1.0:bs)
           | maxMay == Nothing && opts ^. axis == YAxis -> (1.0:as, 0.0:bs)
           | otherwise -> (as, bs)

plotKde :: (G.Vector v Double) => AlphaColour Double -> v Double -> KdePlotOpts
        -> EC (Layout Double Double) ()
plotKde c xs opts = do
    plotLine c (opts ^. linewidth) kdeVals
    when (opts ^. shade) $ plotShade c kdeVals
  where
    minX = G.minimum xs
    maxX = G.maximum xs
    iqr = interquartileRange xs
    std = Stats.stdDev xs
    n = fromIntegral $ G.length xs
    a = min std (iqr / 1.34)
    bandwidth = if | opts ^. bw == Scott -> 1.059 * a * n ** ((-1) / 5)
                   | (BwScalar b) <- opts ^. bw -> b
    clipLeft = minX - (opts ^. cut) * bandwidth
    clipRight = maxX + (opts ^. cut) * bandwidth
    sizeEstimate = fromIntegral $ opts ^. gridsize :: Double
    numEstimates = 2 ^ (ceiling (log sizeEstimate / log 2) :: Int)
    xVals = Stats.Points $ linspace clipLeft clipRight numEstimates
    kdeResult = Stats.estimatePDF Stats.gaussianKernel bandwidth xs xVals
    kdeVals' = U.zip (Stats.fromPoints xVals) kdeResult
    kdeVals = if opts ^. axis == YAxis
      then U.toList $ U.map swap kdeVals'
      else U.toList kdeVals'

plotLine :: AlphaColour Double -> Double -> [(Double, Double)]
         -> EC (Layout Double Double) ()
plotLine c lineWidth xs = Chart.plot $ Chart.liftEC $ do
  Chart.plot_lines_values .= [xs]
  Chart.plot_lines_style . Chart.line_width .= lineWidth
  Chart.plot_lines_style . Chart.line_color .= c

plotShade :: AlphaColour Double -> [(Double, Double)]
          -> EC (Layout Double Double) ()
plotShade c xs = Chart.plot $ Chart.liftEC $ do
  Chart.plot_fillbetween_values .= (xs & mapped . _2 %~ \y -> (0, y))
  Chart.plot_fillbetween_style .= Chart.solidFillStyle (Colour.dissolve 0.25 c)

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
