{-# LANGUAGE FlexibleContexts #-}
module Amby.BoxPlot
  ( boxPlotVec
  , boxPlotVec'
  ) where

import Control.Arrow ((***))
import Control.Monad
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Vector.Generic as G

import Control.Lens
import Data.Colour (transparent, opaque)
import Data.Colour.SRGB (sRGB)
import Graphics.Rendering.Chart.Easy (Layout, EC)
import Graphics.Rendering.Chart.Easy as Chart
import qualified Statistics.Quantile as Stats

import Amby.Categorical
import Amby.Types
import Amby.Theme
import Amby.Style (categoricalAxisData, scaledAxisCustom)
import Amby.Numeric

boxPlotVec :: (G.Vector v Double, G.Vector v (Double, Double))
           => v Double -> State BoxPlotOpts () -> AmbyChart ()
boxPlotVec xs optsState = do
    layout <- takeLayout
    putLayout $ do
      layout
      palette <- Chart.liftCState $ do
        cs <- use Chart.colors
        return cs
      drawBoxPlot palette xs opts
  where
    opts = execState optsState def

boxPlotVec' :: (G.Vector v Double, G.Vector v (Double, Double))
            => v Double -> AmbyChart ()
boxPlotVec' xs = boxPlotVec xs $ return ()

drawBoxPlot :: (G.Vector v Double, G.Vector v (Double, Double))
            => Palette -> v Double -> BoxPlotOpts
            -> EC (Layout Double Double) ()
drawBoxPlot palette xs opts
  -- Boxplot for univariate distribution
  | opts ^. cat == DefaultCategory = do
    drawUnivariate (head catPalette) lineGray startPos barHeight xs opts

    -- Axis changes
    axisGetter . Chart.laxis_generate .= scaledAxisCustom def
      ( startPos - (barHeight / 2) - (1/8) * barHeight
      , startPos + (barHeight / 2) + (1/8) * barHeight
      )
    axisGetter . Chart.laxis_override .= categoricalAxisData []

  -- Boxplot against categorical data
  | opts ^. hue == DefaultCategory = do
    let groups = filterByCategory xs (opts ^. cat)
        maxIdx = length groups - 1
    forM_ (zip3 groups catPalette [0..]) $ \(g, c, i) ->
      drawUnivariate c lineGray (catMidPos i) barHeight g opts

    -- Axis changes
    axisGetter . Chart.laxis_generate .= scaledAxisCustom def
      ( catMidPos maxIdx - catMargin - (barHeight / 2)
      , startPos + (barHeight / 2) + catMargin
      )
    axisGetter . Chart.laxis_override .= categoricalAxisData catLabelPos

  -- Boxplot against two categories
  | opts ^. col == DefaultCategory = do
    let groups = filterByCategory2 xs (opts ^. cat) (opts ^. hue)
    forM_ (zip groups [0..]) $ \(g, i) ->
      forM_ (zip3 g catPalette [0..]) $ \(gCat, c, j) ->
        drawUnivariate c lineGray (hueMidPos i j) barHeight gCat opts

    -- Axis changes
    axisGetter . Chart.laxis_generate .= scaledAxisCustom def
      ( hueMidPos (nCats - 1) (nHues - 1) - (barHeight / 2) - hueMargin
      , startPos + (barHeight / 2) + hueMargin
      )
    axisGetter . Chart.laxis_override .= categoricalAxisData hueLabelPos

  -- Boxplot against three categories
  | otherwise = undefined
  where
    catLabels = getCategoryLabels (opts ^. cat)
    nCats = catSize (opts ^. cat)
    nHues = catSize (opts ^. hue)
    _nCols = catSize (opts ^. col)
    cUser = opts ^. color
    sat = opts ^. saturation
    (catPalette, lineGray) = getCategoricalPalette palette cUser nCats nHues sat

    barHeight = 1.0
    startPos = 1.0

    catMidSpacing = barHeight / 4.0
    catMidPos :: Int -> Double
    catMidPos i = startPos - (barHeight + catMidSpacing) * fromIntegral i
    catMargin = barHeight / 4.0
    catLabelPos = zipWith (\i l -> (catMidPos i, l)) [0..] catLabels

    hueMidSpacing = barHeight / 2.0
    hueMargin = barHeight / 2.0
    hueCatSize = fromIntegral nHues * barHeight
    hueSpan = (fromIntegral (nHues - 1) * barHeight) / 2
    hueMidPos :: Int -> Int -> Double
    hueMidPos i j =
        startPos
      - (fromIntegral j * barHeight)
      - (fromIntegral i * (hueCatSize + hueMidSpacing))
    hueLabelMidPos i = startPos - hueSpan - i * (hueCatSize + hueMidSpacing)
    hueLabelPos = zipWith (\i l -> (hueLabelMidPos i, l)) [0..] catLabels

    axisGetter = if (opts ^. axis) == XAxis
      then Chart.layout_y_axis
      else Chart.layout_x_axis

drawUnivariate :: (G.Vector v Double, G.Vector v (Double, Double))
               => AlphaColour Double -> AlphaColour Double
               -> Double -> Double -> v Double
               -> BoxPlotOpts -> EC (Layout Double Double) ()
drawUnivariate c lineGray midY yHeight xs opts = do
    Chart.plot $ return $ Chart.Plot
      { _plot_render = renderBoxPlot
      , _plot_legend = [("", const $ return ())]
      , _plot_all_points = unzip allPoints
      }
    Chart.plot $ Chart.liftEC $ do
      Chart.plot_points_values .= outlierPts
      Chart.plot_points_style . Chart.point_radius .= 4
      Chart.plot_points_style . Chart.point_shape .= Chart.PointShapePolygon 4 True
      Chart.plot_points_style . Chart.point_color .= lineGray
  where
    whiskLimit = interquartileRange xs * 1.5
    firstQuartile = Stats.weightedAvg 1 4 xs
    thirdQuartile = Stats.weightedAvg 3 4 xs
    median = Stats.weightedAvg 50 100 xs
    startWhiskFoldFn a b = if b < a && b >= firstQuartile - whiskLimit
      then b
      else a
    endWhiskFoldFn a b = if b > a && b <= thirdQuartile + whiskLimit
      then b
      else a
    startWhisk = G.foldl' startWhiskFoldFn firstQuartile xs
    endWhisk = G.foldl' endWhiskFoldFn thirdQuartile xs
    outliers = G.filter (\x -> x < startWhisk || x > endWhisk) xs
    pt x y = if opts ^. axis == XAxis then (x, y) else (y, x)
    yt = midY + (yHeight / 2)
    yb = midY - (yHeight / 2)
    yt4 = midY + (yHeight / 4)
    yb4 = midY - (yHeight / 4)

    outlierPts = G.toList $ G.map (\x -> pt x midY) outliers

    allPoints =
      [ pt startWhisk yt4, pt startWhisk yb4, pt startWhisk midY
      , pt firstQuartile midY
      , pt firstQuartile yt, pt firstQuartile yb, pt median yt, pt median yb
      , pt thirdQuartile yt, pt thirdQuartile yb, pt thirdQuartile midY
      , pt endWhisk midY, pt endWhisk yt4, pt endWhisk yb4
      ] ++ outlierPts

    renderBoxPlot pmap = do
      Chart.withFillStyle fillStyle $ do
        Chart.alignFillPath (plotBox pmap) >>= Chart.fillPath
      Chart.withLineStyle lineStyle $ do
        Chart.alignStrokePath (plotWhiskers pmap) >>= Chart.strokePath
        Chart.alignStrokePath (plotBox pmap) >>= Chart.strokePath
      where
        lineStyle = def
          & Chart.line_width .~ (opts ^. linewidth)
          & Chart.line_color .~ lineGray
        fillStyle = def
          & Chart.fill_color .~ c

    plotWhiskers pmap =
        Chart.MoveTo (ptr startWhisk yt4)
      $ Chart.LineTo (ptr startWhisk yb4)
      $ Chart.MoveTo (ptr startWhisk midY)
      $ Chart.LineTo (ptr firstQuartile midY)
      $ Chart.MoveTo (ptr thirdQuartile midY)
      $ Chart.LineTo (ptr endWhisk midY)
      $ Chart.MoveTo (ptr endWhisk yt4)
      $ Chart.LineTo (ptr endWhisk yb4)
      $ Chart.End
      where
        ptr x y = pmap $ join (***) Chart.LValue $ pt x y

    plotBox pmap =
        Chart.MoveTo (ptr median yt)
      $ Chart.LineTo (ptr firstQuartile yt)
      $ Chart.LineTo (ptr firstQuartile yb)
      $ Chart.LineTo (ptr median yb)
      $ Chart.LineTo (ptr median yt)
      $ Chart.LineTo (ptr thirdQuartile yt)
      $ Chart.LineTo (ptr thirdQuartile yb)
      $ Chart.LineTo (ptr median yb)
      $ Chart.End
      where
        ptr x y = pmap $ join (***) Chart.LValue $ pt x y

getCategoricalPalette :: Palette -> AmbyColor -> Int -> Int -> Double
                      -> (Palette, AlphaColour Double)
getCategoricalPalette palette cUser n nHues sat = (desatColors, lineGray)
  where
    cUserAlpha = toColour cUser transparent
    effN = if nHues > 1 then nHues else n
    iniColors
      -- Passing specific color
      | cUser /= DefaultColor && nHues <= 1 = replicate effN cUserAlpha
      | cUser /= DefaultColor = lightPalette cUserAlpha nHues

      -- Using default colors
      | (length . L.nub . take effN) palette < effN =
        huslPalette n Nothing Nothing (Just 0.7)
      | otherwise = take effN palette
    desatColors = map (desaturate sat) iniColors
    l = (* 0.6) . minimum $ map ((^. _3) . alphaToHsl) desatColors
    lineGray = opaque $ sRGB l l l
