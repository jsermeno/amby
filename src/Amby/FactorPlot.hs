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
        boxOpts <- get
        put $ boxOpts
          { _bpoCat = _fpoCat opts
          , _bpoHue = _fpoHue opts
          , _boxPlotOptsColor = opts ^. color
          , _boxPlotOptsSaturation = opts ^. saturation
          , _boxPlotOptsAxis = opts ^. axis
          }
    setGrid $ chartToGrid ambyChart

  -- Row chart
  | rows /= DefaultCategory && cols == DefaultCategory =
    drawThirdFactor xs rows Chart.aboveN (catSize rows) 1 opts

  -- Col chart
  | rows == DefaultCategory && cols /= DefaultCategory = do
    drawThirdFactor xs cols Chart.besideN 1 (catSize cols) opts

  -- Row and col chart
  | otherwise = undefined
  where
    cols = _fpoCol opts
    rows = _fpoRow opts
    opts = execState optsState def

drawThirdFactor :: V.Vector Double -> Category
                -> ([ChartGrid] -> ChartGrid) -> Int -> Int
                -> FactorPlotOpts -> AmbyGrid ()
drawThirdFactor xs grouper gridGrouper nRows nCols opts = do
  let cats = _fpoCat opts
      hues = _fpoHue opts
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
            boxOpts <- get
            put $ boxOpts
              { _bpoCat = catGroups `getGroupAt` i
              , _bpoHue = hueGroups `getGroupAt` i
              , _boxPlotOptsCatLegend = if nCols > 1
                then i == 0 || opts ^. axis == YAxis
                else i == nRows - 1 || opts ^. axis == XAxis
              , _boxPlotOptsHueLegend = if nCols > 1
                then i == 0
                else i == nRows - 1
              , _boxPlotOptsColor = opts ^. color
              , _boxPlotOptsSaturation = opts ^. saturation
              , _boxPlotOptsAxis = opts ^. axis
              }
          title (getCategoryLabelFromVal grouper factorVal)
  gridScale (fromIntegral nCols, fromIntegral nRows)

modErr :: String -> String -> a
modErr f err = error
  $ showString "Amby.FactorPlot."
  $ showString f
  $ showString ": " err
