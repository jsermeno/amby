{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Amby.Types
  ( AmbyContainer(..)
  , AmbyState
  , AmbyChart

  -- * Accessors
  , takeTheme
  , theme
  , xlim
  , ylim
  , size
  , takeLayout
  , getLayout
  , getSize
  , putLayout
  )
  where

import Control.Monad.State
import Data.Default.Class
import Graphics.Rendering.Chart.Easy hiding (plot)
import Graphics.Rendering.Chart.Backend.Cairo (FileOptions(..))

import Amby.Theme
import Amby.Style

data AmbyState = AmbyState
  { _asThemeState :: Theme
  , _asLayoutState :: EC (Layout Double Double) ()
  , _asSize :: (Int, Int)
  }

$( makeLenses ''AmbyState)

type AmbyChart a = State AmbyState a

class AmbyContainer c a | c -> a where
  plot :: c -> c -> AmbyChart ()
  plotEq :: c -> (a -> a) -> AmbyChart ()
  distPlot :: c -> AmbyChart ()

getLayout :: AmbyState -> EC (Layout Double Double) ()
getLayout s = s ^. asLayoutState

getSize :: AmbyState -> (Int, Int)
getSize s = s ^. asSize

takeTheme :: AmbyChart Theme
takeTheme = do
  t <- use asThemeState
  return t

takeLayout :: AmbyChart (EC (Layout Double Double) ())
takeLayout = do
  l <- use asLayoutState
  return l

putLayout :: EC (Layout Double Double) () -> AmbyChart ()
putLayout l = do
  asLayoutState .= l

theme :: Theme -> AmbyChart ()
theme t = do
  l <- use asLayoutState
  asLayoutState .= do
    l
    setColors (getColorCycle t)
    setThemeStyles t
  asThemeState .= t

xlim :: (Double, Double) -> AmbyChart ()
xlim rs = do
  l <- use asLayoutState
  asLayoutState .= do
    l
    layout_x_axis . laxis_generate .= scaledAxis def rs

ylim :: (Double, Double) -> AmbyChart ()
ylim rs = do
  l <- use asLayoutState
  asLayoutState .= do
    l
    layout_y_axis . laxis_generate .= scaledAxis def rs

size :: (Int, Int) -> AmbyChart ()
size rs = asSize .= rs

--------------------
-- Default instances
--------------------

instance Default AmbyState where
  def = AmbyState
    { _asThemeState = def
    , _asLayoutState = do
      setColors (getColorCycle def)
      setThemeStyles def
    , _asSize = _fo_size def
    }

instance Default (PlotHist x Double) where
  def = PlotHist
    { _plot_hist_bins = 20
    , _plot_hist_title       = ""
    , _plot_hist_values      = []
    , _plot_hist_no_zeros    = False
    , _plot_hist_range       = Nothing
    , _plot_hist_drop_lines  = False
    , _plot_hist_line_style  = def
    , _plot_hist_fill_style  = def
    , _plot_hist_norm_func   = (\a b -> fromIntegral b / a)
    }

