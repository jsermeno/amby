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
import Amby.Types

factorPlotVec :: V.Vector Double -> State FactorPlotOpts () -> AmbyGrid ()
factorPlotVec xs optsState
  -- Single plot
  | rows == DefaultCategory && cols == DefaultCategory = do
    ambyChart <- case opts ^. kind of
      Box -> return $ boxPlotVec xs $ do
        catL .= (opts ^. catL)
        hueL .= (opts ^. hueL)
        color .= opts ^. color
        saturation .= opts ^. saturation
        axis .= opts ^. axis
    setGrid $ chartToGrid ambyChart

  -- Row chart
  | rows /= DefaultCategory && cols == DefaultCategory =
    drawThirdFactor xs rows Chart.aboveN (catSize rows) 1 opts

  -- Col chart
  | rows == DefaultCategory && cols /= DefaultCategory = do
    drawThirdFactor xs cols Chart.besideN 1 (catSize cols) opts

  -- Row and col chart
  | otherwise =
    drawFourthFactor xs rows cols opts
  where
    cols = opts ^. colL
    rows = opts ^. rowL
    opts = execState optsState def

drawThirdFactor :: V.Vector Double -> Category
                -> ([ChartGrid] -> ChartGrid) -> Int -> Int
                -> FactorPlotOpts -> AmbyGrid ()
drawThirdFactor xs grouper gridGrouper nRows nCols opts = do
  let cats = opts ^. catL
      hues = opts ^. hueL
      datGroups = groupByCategory (V.toList xs) grouper
      catGroups = cats `groupCategoryBy` grouper
      hueGroups = hues `groupCategoryBy` grouper
      factorOrder = getCategoryOrder grouper
      plotKind = opts ^. kind
  setGrid
    $ gridGrouper
    $ (`map` zip factorOrder [0..])
    $ \(factorVal, i) -> do
      let datGroup = case datGroups `atMay` i of
            Just a -> V.fromList a
            Nothing -> modErr "drawThirdFactor" $ "No group at index: " ++ show i
      case plotKind of
        Box -> chartToGrid $ do
          boxPlotVec datGroup $ do
            catL .= catGroups `getGroupAt` i
            hueL .= hueGroups `getGroupAt` i
            catLegend .= if nCols > 1
              then i == 0 || opts ^. axis == YAxis
              else i == nRows - 1 || opts ^. axis == XAxis
            hueLegend .= if nCols > 1
              then i == 0
              else i == nRows - 1
            color .= opts ^. color
            saturation .= opts ^. saturation
            axis .= opts ^. axis
          title (getCategoryLabelFromVal grouper factorVal)
  gridScale (fromIntegral nCols, fromIntegral nRows)

drawFourthFactor :: V.Vector Double -> Category -> Category -> FactorPlotOpts
                 -> AmbyGrid ()
drawFourthFactor xs rows cols opts =
      setGrid
    $ Chart.aboveN
    $ (`map` zip rowOrder [0..])
    $ \rowIdx ->
        Chart.besideN
      $ (`map` zip colOrder [0..]) $ \colIdx -> drawGridCell rowIdx colIdx
  where
    cats = opts ^. catL
    hues = opts ^. hueL
    datGroups = groupByCategory (V.toList xs) rows
    catGroups = cats `groupCategoryBy` rows
    hueGroups = hues `groupCategoryBy` rows
    colGroups = cols `groupCategoryBy` rows
    rowOrder = getCategoryOrder rows
    colOrder = getCategoryOrder cols
    plotKind = opts ^. kind

    drawGridCell :: (Int, Int) -> (Int, Int) -> ChartGrid
    drawGridCell (rowVal, i) (colVal, _) = do
      let rowColGroup = colGroups `getGroupAt` i
          colMask = map (== colVal) (getCategoryList rowColGroup)
      let rowDatGroup = case datGroups `atMay` i of
            Nothing -> modErr "drawFourthFactor" $ "No group at index: " ++ show i
            Just a -> V.fromList $ filterMask a colMask
          rowCatGroup = getGroupWithFilterMask catGroups i colMask
          rowHueGroup = getGroupWithFilterMask hueGroups i colMask

      case plotKind of
        Box -> chartToGrid $ do
          boxPlotVec rowDatGroup $ do
            catL .= rowCatGroup
            hueL .= rowHueGroup
            --catLegend .= if nCols > 1
            --  then i == 0 || opts ^. axis == YAxis
            --  else i == nRows - 1 || opts ^. axis == XAxis
            --hueLegend .= if nCols > 1
            --  then i == 0
            --  else i == nRows - 1
            color .= opts ^. color
            saturation .= opts ^. saturation
            axis .= opts ^. axis
          title
            $ getCategoryLabelFromVal rows rowVal
            ++ " | "
            ++ getCategoryLabelFromVal cols colVal

modErr :: String -> String -> a
modErr f err = error
  $ showString "Amby.FactorPlot."
  $ showString f
  $ showString ": " err
