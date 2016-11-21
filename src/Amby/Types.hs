{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Amby.Types
  ( AmbyContainer(..)
  , AmbyState
  , AmbyChart

  -- * General accessors
  , takeTheme
  , theme
  , xlim
  , ylim
  , size
  , takeLayout
  , getLayout
  , getSize
  , putLayout

  -- * Plot options
  , DistPlotOpts
  , KdePlotOpts
  , RugPlotOpts
  , Bandwidth(..)
  , Axis(..)
  , bins
  , hist
  , rug
  , rugHeight
  , shade
  , kde
  , axis
  , height
  , gridsize
  , bw
  )
  where

import Control.Monad.State

import Control.Lens
import Data.Default.Class
import Graphics.Rendering.Chart.Easy (EC, Layout)
import qualified Graphics.Rendering.Chart.Easy as Chart
import Graphics.Rendering.Chart.Backend.Cairo (FileOptions(..))

import Amby.Compatibility.HistogramPlot
import Amby.Theme
import Amby.Style

-----------------------------------
-- Parameter option types
-----------------------------------

data Axis = X | Y deriving (Show, Eq)
data Bandwidth = Scott | BwScalar Double deriving (Show, Eq)

data DistPlotOpts = DistPlotOpts
  { _distPlotOptsRug :: Bool
  , _distPlotOptsHist :: Bool
  , _distPlotOptsKde :: Bool
  , _distPlotOptsBins :: Int

  -- kde opts
  , _distPlotOptsShade :: Bool
  , _distPlotOptsBw :: Bandwidth
  , _distPlotOptsAxis :: Axis
  , _distPlotOptsGridsize :: Int

  -- rug opts
  , _distPlotOptsRugHeight :: Double
  } deriving (Show)
makeFields ''DistPlotOpts

data KdePlotOpts = KdePlotOpts
  { _kdePlotOptsShade :: Bool
  , _kdePlotOptsBw :: Bandwidth
  , _kdePlotOptsAxis :: Axis
  , _kdePlotOptsGridsize :: Int
  } deriving (Show)
makeFields ''KdePlotOpts

data RugPlotOpts = RugPlotOpts
  { _rugPlotOptsHeight :: Double
  , _rugPlotOptsAxis :: Axis
  }
makeFields ''RugPlotOpts

-----------------------------------
-- Main types
-----------------------------------

data AmbyState = AmbyState
  { _asThemeState :: Theme
  , _asLayoutState :: EC (Layout Double Double) ()
  , _asSize :: (Int, Int)
  }
makeLenses ''AmbyState

type AmbyChart a = State AmbyState a

class AmbyContainer c where
  type Value c :: *
  plot :: c -> c -> AmbyChart ()
  plotEq :: c -> (Value c -> Value c) -> AmbyChart ()
  distPlot :: c -> State DistPlotOpts () -> AmbyChart ()
  distPlot' :: c -> AmbyChart ()
  kdePlot :: c -> State KdePlotOpts () -> AmbyChart ()
  kdePlot' :: c -> AmbyChart ()
  rugPlot :: c -> State RugPlotOpts () -> AmbyChart()
  rugPlot' :: c -> AmbyChart ()

-----------------------------------
-- General options
-----------------------------------

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
    Chart.setColors (getColorCycle t)
    setThemeStyles t
  asThemeState .= t

xlim :: (Double, Double) -> AmbyChart ()
xlim rs = do
  l <- use asLayoutState
  asLayoutState .= do
    l
    Chart.layout_x_axis . Chart.laxis_generate .= Chart.scaledAxis def rs

ylim :: (Double, Double) -> AmbyChart ()
ylim rs = do
  l <- use asLayoutState
  asLayoutState .= do
    l
    Chart.layout_y_axis . Chart.laxis_generate .= Chart.scaledAxis def rs

size :: (Int, Int) -> AmbyChart ()
size rs = asSize .= rs

--------------------
-- Default instances
--------------------

instance Default AmbyState where
  def = AmbyState
    { _asThemeState = def
    , _asLayoutState = do
      Chart.setColors (getColorCycle def)
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
    , _plot_hist_vertical    = False
    }

instance Default DistPlotOpts where
  def = DistPlotOpts
    { _distPlotOptsRug = False
    , _distPlotOptsHist = True
    , _distPlotOptsKde = True
    , _distPlotOptsBins = 0

    , _distPlotOptsShade = False
    , _distPlotOptsBw = Scott
    , _distPlotOptsGridsize = 100
    , _distPlotOptsAxis = X

    , _distPlotOptsRugHeight = 0.05
    }

instance Default KdePlotOpts where
  def = KdePlotOpts
    { _kdePlotOptsShade = False
    , _kdePlotOptsBw = Scott
    , _kdePlotOptsGridsize = 100
    , _kdePlotOptsAxis = X
    }

instance Default RugPlotOpts where
  def = RugPlotOpts
    { _rugPlotOptsHeight = 0.05
    , _rugPlotOptsAxis = X
    }
