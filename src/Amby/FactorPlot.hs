{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
module Amby.FactorPlot
  ( factorPlotVec
  ) where

import Control.Lens
import Control.Monad.State
import Data.Default (def)
import qualified Data.Vector as V

import qualified Graphics.Rendering.Chart.Grid as Chart
import Safe

import Amby.BoxPlot
import Amby.Categorical
import Amby.Theme
import Amby.Types

factorPlotVec :: V.Vector Double -> State FactorPlotOpts () -> AmbyGrid ()
factorPlotVec xs optsState
  -- Single plot
  | rows == DefaultCategory && cols == DefaultCategory = do
    activeTheme <- takeGridTheme
    ambyChart <- case opts ^. kind of
      Box -> return $ do
        theme activeTheme
        boxPlotVec xs $ do
          facL .= (opts ^. facL)
          hueL .= (opts ^. hueL)
          color .= opts ^. color
          saturation .= opts ^. saturation
          axis .= opts ^. axis
          facLabel .= opts ^. facLabel
          datLabel .= opts ^. datLabel
    setGrid $ chartToGrid ambyChart

  -- Row chart
  | rows /= DefaultCategory && cols == DefaultCategory =
    drawThirdFactor xs rows Chart.aboveN (catSize rows) 1 (opts ^. rowLabel) opts

  -- Col chart
  | rows == DefaultCategory && cols /= DefaultCategory = do
    drawThirdFactor xs cols Chart.besideN 1 (catSize cols) (opts ^. colLabel) opts

  -- Row and col chart
  | otherwise =
    drawFourthFactor xs rows cols opts
  where
    cols = opts ^. colL
    rows = opts ^. rowL
    opts = execState optsState def

drawThirdFactor :: V.Vector Double -> Category
                -> ([ChartGrid] -> ChartGrid) -> Int -> Int -> String
                -> FactorPlotOpts -> AmbyGrid ()
drawThirdFactor xs grouper gridGrouper nRows nCols gLabel opts = do
  let cats = opts ^. facL
      hues = opts ^. hueL
      datGroups = groupByCategory (V.toList xs) grouper
      catGroups = cats `groupCategoryBy` grouper
      hueGroups = hues `groupCategoryBy` grouper
      factorOrder = getCategoryOrder grouper
      plotKind = opts ^. kind
      drawAxis = opts ^. axis
  activeTheme <- takeGridTheme
  setGrid
    $ gridGrouper
    $ (`map` zip factorOrder [0..])
    $ \(factorVal, i) -> do
      let datGroup = case datGroups `atMay` i of
            Just a -> V.fromList a
            Nothing -> modErr "drawThirdFactor" $ "No group at index: " ++ show i
      case plotKind of
        Box -> chartToGrid $ do
          theme activeTheme
          boxPlotVec datGroup $ do
            facL .= catGroups `getGroupAt` i
            hueL .= hueGroups `getGroupAt` i
            facLegend .= if nCols > 1
              then i == 0 || drawAxis == YAxis
              else i == nRows - 1 || drawAxis == XAxis
            hueLegend .= if nCols > 1
              then i == 0
              else i == nRows - 1
            color .= opts ^. color
            saturation .= opts ^. saturation
            axis .= opts ^. axis
            facLabel .=
              if   (drawAxis == XAxis && nCols > 1 && i == 0)
                || (drawAxis == XAxis && nRows > 1)
                || (drawAxis == YAxis && nCols > 1)
                || (drawAxis == YAxis && nRows > 1 && i == nRows - 1)
                then opts ^. facLabel
                else ""
            datLabel .=
              if   (drawAxis == XAxis && nCols > 1)
                || (drawAxis == XAxis && nRows > 1 && i == nRows - 1)
                || (drawAxis == YAxis && nCols > 1 && i == 0)
                || (drawAxis == YAxis && nRows > 1)
                then opts ^. datLabel
                else ""
          title
            $ mkGridLabel gLabel
            ++ getCategoryLabelFromVal grouper factorVal
  gridScale (fromIntegral nCols, fromIntegral nRows)

drawFourthFactor :: V.Vector Double -> Category -> Category -> FactorPlotOpts
                 -> AmbyGrid ()
drawFourthFactor xs rows cols opts = do
    activeTheme <- takeGridTheme
    setGrid
      $ Chart.aboveN
      $ (`map` zip rowOrder [0..])
      $ \rowIdx ->
          Chart.besideN
        $ (`map` zip colOrder [0..]) $ \colIdx ->
          drawGridCell rowIdx colIdx activeTheme
    gridScale (fromIntegral nCols, fromIntegral nRows)
  where
    cats = opts ^. facL
    hues = opts ^. hueL
    datGroups = groupByCategory (V.toList xs) rows
    catGroups = cats `groupCategoryBy` rows
    hueGroups = hues `groupCategoryBy` rows
    colGroups = cols `groupCategoryBy` rows
    rowOrder = getCategoryOrder rows
    colOrder = getCategoryOrder cols
    nRows = length rowOrder
    nCols = length colOrder
    plotKind = opts ^. kind
    rLabel = opts ^. rowLabel
    cLabel = opts ^. colLabel
    drawAxis = opts ^. axis

    drawGridCell :: (Int, Int) -> (Int, Int) -> Theme -> ChartGrid
    drawGridCell (rowVal, i) (colVal, j) activeTheme = do
      let rowColGroup = colGroups `getGroupAt` i
          colMask = map (== colVal) (getCategoryList rowColGroup)
      let rowDatGroup = case datGroups `atMay` i of
            Nothing -> modErr "drawFourthFactor" $ "No group at index: " ++ show i
            Just a -> V.fromList $ filterMask a colMask
          rowCatGroup = getGroupWithFilterMask catGroups i colMask
          rowHueGroup = getGroupWithFilterMask hueGroups i colMask

      case plotKind of
        Box -> chartToGrid $ do
          theme activeTheme
          boxPlotVec rowDatGroup $ do
            facL .= rowCatGroup
            hueL .= rowHueGroup
            facLegend .= if drawAxis == XAxis
              then j == 0
              else i == nRows - 1
            hueLegend .= (j == 0 && i == nRows - 1)
            color .= opts ^. color
            saturation .= opts ^. saturation
            axis .= opts ^. axis
            facLabel .=
              if   (j == 0 && drawAxis == XAxis)
                || (i == nRows - 1 && drawAxis == YAxis)
                then opts ^. facLabel
                else ""
            datLabel .=
              if   (i == nRows - 1 && drawAxis == XAxis)
                || (j == 0 && drawAxis == YAxis)
                then opts ^. datLabel
                else ""
          title
            $ mkGridLabel rLabel
            ++ getCategoryLabelFromVal rows rowVal
            ++ " | "
            ++ mkGridLabel cLabel
            ++ getCategoryLabelFromVal cols colVal

mkGridLabel :: String -> String
mkGridLabel s = if null s
  then s
  else s ++ " = "

modErr :: String -> String -> a
modErr f err = error
  $ showString "Amby.FactorPlot."
  $ showString f
  $ showString ": " err
