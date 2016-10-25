{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Amby.Style
  ( roundAxisData
  , noEdgeAxisData
  , scaledPaddedAxis
  , setThemeStyles
  )
  where

import Safe
import qualified Data.List as L
import Numeric

import Graphics.Rendering.Chart.Easy hiding (plot)

import Amby.Theme

-- | Equalizes precision for all labels.
-- For example the labels [0, 0.2] become [0.0, 0.2].
roundAxisData :: AxisData x -> AxisData x
roundAxisData axisData = axisData & axis_labels %~ go
  where
    go xs = map go' xs
    go' xs = xs
        & mapped . _2
        %~ \x -> case readMay x of
          Just a -> showFFloat (Just precision) a ""
          Nothing -> x
      where
        precision = maximum $ map (countAfterDecimal . snd) xs

noEdgeAxisData :: AxisData x -> AxisData x
noEdgeAxisData axisData = axisData & axis_grid %~ removeEdges
  where
    removeEdges xs
      | length xs < 2 = []
      | otherwise = (init . tail) xs

scaledPaddedAxis :: forall x. (RealFloat x, PlotValue x) => [x] -> AxisData x
scaledPaddedAxis axisPoints = defaultAxis
    & axis_grid %~ (\xs -> gridMin : xs ++ [gridMax])
  where
    getGridLimits xs
      | length xs < 1 = error "Not enough grid points"
      | otherwise = (head xs - spacing, last xs + spacing)
      where
        getSpacing (x:y:xs) = y - x
        spacing = getSpacing xs
    defaultAxis = autoAxis axisPoints
    (gridMin, gridMax) = getGridLimits (defaultAxis ^. axis_grid)

countAfterDecimal :: String -> Int
countAfterDecimal xs = case L.findIndex (== '.') xs of
  Nothing -> 0
  Just idx -> (length xs - 1) - idx

setThemeStyles :: Theme -> EC (Layout Double Double) ()
setThemeStyles theme = do
  layout_background .= (FillStyleSolid $ getBgColor theme)
  layout_plot_background .= Just (FillStyleSolid $ getPlotBgColor theme)
  layout_all_font_styles . font_name .= (getFontFamily theme)
  layout_all_font_styles . font_size .= (getFontSize theme)
  layout_axes_styles . axis_line_style . line_width .= 0.0
  layout_axes_styles . axis_grid_style . line_color .= (getGridLineColor theme)
  layout_axes_styles . axis_grid_style . line_width .= 1.5
  layout_axes_styles . axis_grid_style . line_dashes .= []
  layout_axes_styles . axis_label_gap .= 8
  layout_x_axis . laxis_override .= roundAxisData
  layout_y_axis . laxis_override .= roundAxisData
  layout_margin .= 10
