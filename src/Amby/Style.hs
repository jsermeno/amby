{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
module Amby.Style
  ( setThemeStyles
  , scaledAxisCustom
  , categoricalAxisData
  )
  where

import qualified Data.List as L
import Data.Ord (comparing)
import Numeric

import Control.Lens
import Data.Default
import Safe

import Graphics.Rendering.Chart.Easy (EC, Layout, AxisData(..))
import qualified Graphics.Rendering.Chart.Easy as Chart
import Graphics.Rendering.Chart.Utils (isValidNumber)
import Graphics.Rendering.Chart.Axis.Floating (LinearAxisParams(..))

import Amby.Theme

countAfterDecimal :: String -> Int
countAfterDecimal xs = case L.findIndex (== '.') xs of
  Nothing -> 0
  Just idx -> (length xs - 1) - idx

setThemeStyles :: Theme -> EC (Layout x y) ()
setThemeStyles theme = do
  Chart.layout_background .= (Chart.FillStyleSolid $ theme ^. bgColor)
  Chart.layout_plot_background .= Just (Chart.FillStyleSolid $ theme ^. plotBgColor)
  Chart.layout_all_font_styles . Chart.font_name .= (theme ^. fontFamily)
  Chart.layout_all_font_styles . Chart.font_size .= (theme ^. fontSize)
  Chart.layout_axes_styles . Chart.axis_line_style . Chart.line_width .= 0.0
  Chart.layout_axes_styles . Chart.axis_grid_style . Chart.line_color
    .= (theme ^. gridLineColor)
  Chart.layout_axes_styles . Chart.axis_grid_style . Chart.line_width .= 1.5
  Chart.layout_axes_styles . Chart.axis_grid_style . Chart.line_dashes .= []
  Chart.layout_axes_styles . Chart.axis_label_gap .= 8
  Chart.layout_x_axis . Chart.laxis_override .= roundAxisData
  Chart.layout_y_axis . Chart.laxis_override .= roundAxisData
  Chart.layout_margin .= 10

--- | Equalizes precision for all labels.
--- For example the labels [0, 0.2] become [0.0, 0.2].
roundAxisData :: Chart.AxisData x -> Chart.AxisData x
roundAxisData axisData = axisData & Chart.axis_labels %~ go
  where
    go xs = map go' xs
    go' xs = xs
        & mapped . _2
        %~ \x -> case readMay x :: Maybe Double of
          Just a -> showFFloat (Just precision) a ""
          Nothing -> x
      where
        precision = maximum $ map (countAfterDecimal . snd) xs

categoricalAxisData :: [(x, String)] -> Chart.AxisData x -> Chart.AxisData x
categoricalAxisData labels axisData = axisData
  & Chart.axis_grid .~ []
  & Chart.axis_labels .~ [labels]

------------------------
-- Scaling
------------------------

-- | Taken from the 'Chart' package with minor modifications.
-- Needed to enforce 'xlim' and 'ylim' maximum and minimum limits.
-- Default version snaps to next available grid position.
scaledAxisCustom :: RealFloat a => Chart.LinearAxisParams a -> (a, a)
                 -> Chart.AxisFn a
scaledAxisCustom lap rs@(minV,maxV) ps0 = makeAxisCustom'
    realToFrac
    realToFrac
    labelFn
    (labelvs, tickvs, gridvs)
    rs
  where
#if MIN_VERSION_Chart(1,7,0)
    labelFn   = _la_labelf lap
#else
    labelFn   = map (_la_labelf lap)
#endif

    ps        = filter isValidNumber ps0
    range []  = (0,1)
    range _   | minV == maxV = if minV==0 then (-1,1) else
                               let d = abs (minV * 0.01) in (minV-d,maxV+d)
              | otherwise    = rs
    labelvs   = map fromRational $ steps (fromIntegral (_la_nLabels lap)) r
    tickvs    = map fromRational $ steps (fromIntegral (_la_nTicks lap))
                                         (minimum labelvs,maximum labelvs)
    gridvs    = labelvs
    r         = range ps

steps :: RealFloat a => a -> (a,a) -> [Rational]
steps nSteps rs@(minV,maxV) = map ((s*) . fromIntegral) [min' .. max']
  where
    s    = chooseStep nSteps rs
    min' :: Integer
    min' = floor   $ realToFrac minV / s
    max' = ceiling $ realToFrac maxV / s

chooseStep :: RealFloat a => a -> (a,a) -> Rational
chooseStep nsteps (x1,x2) = L.minimumBy (comparing proximity) stepVals
  where
    delta = x2 - x1
    mult  | delta == 0 = 1  -- Otherwise the case below will use all of memory
          | otherwise  = 10 ^^ ((floor $ log $ delta / nsteps)::Integer)
    stepVals = map (mult*) [0.1,0.2,0.25,0.5,1.0,2.0,2.5,5.0,10,20,25,50]
    proximity x = abs $ delta / realToFrac x - nsteps

makeAxisCustom' :: Ord x => (x -> Double) -> (Double -> x) -> ([x] -> [String])
                   -> ([x],[x],[x]) -> (x, x) -> AxisData x
makeAxisCustom' t f labelf (labelvs, tickvs, gridvs) (minX, minY) = AxisData {
    _axis_visibility = def,
    _axis_viewport = Chart.linMap t (minX, minY),
    _axis_tropweiv = Chart.invLinMap f t (minimum labelvs, maximum labelvs),
    _axis_ticks    = zip tickvs (repeat 2)  ++  zip labelvs (repeat 5),
    _axis_grid     = gridvs,
    _axis_labels   =
      let zipWithLengthCheck (x:xs) (y:ys) = (x,y) : zipWithLengthCheck xs ys
          zipWithLengthCheck [] [] = []
          zipWithLengthCheck _ _ =
            error "makeAxis': label function returned the wrong number of labels"
      in [zipWithLengthCheck labelvs (labelf labelvs)]
    }
