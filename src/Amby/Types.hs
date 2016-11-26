{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTSyntax #-}
module Amby.Types
  ( AmbyContainer(..)
  , AmbyState
  , AmbyChart
  , AmbyGrid
  , ChartGrid
  , Saveable(..)

  -- * General accessors
  , takeTheme
  , theme
  , xlim
  , ylim
  , size
  , title
  , takeLayout
  , getLayout
  , getSize
  , putLayout
  , getSaveObjectRenderable

  -- * Grid
  , gridTheme
  , gridSize
  , gridScale
  , setGrid
  , chartToGrid

  -- * Plot options
  , PlotOpts
  , PlotEqOpts
  , DistPlotOpts
  , KdePlotOpts
  , RugPlotOpts
  , BoxPlotOpts(..)
  , FactorPlotOpts(..)
  , Bandwidth(..)
  , Axis(..)
  , PlotKind(..)
  , bins
  , hist
  , rug
  , rugHeight
  , cut
  , shade
  , kde
  , axis
  , height
  , gridsize
  , bw
  , color
  , linewidth
  , histLinewidth
  , kdeLinewidth
  , rugLinewidth
  , kind
  , hueLegend
  , catLegend

  -- * Categorical options
  , cat
  , hue
  , row
  , col
  , saturation
  )
  where

import Control.Monad.State

import Control.Lens
import Data.Default.Class
import Graphics.Rendering.Chart.Easy (EC, Layout, LayoutPick, Renderable)
import qualified Graphics.Rendering.Chart.Easy as Chart
import Graphics.Rendering.Chart.Grid (Grid)
import qualified Graphics.Rendering.Chart.Grid as Chart
import Graphics.Rendering.Chart.Backend.Cairo (FileOptions(..))

import Amby.Compatibility.HistogramPlot
import Amby.Theme
import Amby.Style
import Amby.Categorical

-----------------------------------
-- Parameter option types
-----------------------------------

data Axis = XAxis | YAxis deriving (Show, Eq)
data Bandwidth = Scott | BwScalar Double deriving (Show, Eq)
data PlotKind = Box deriving (Show, Eq)

data PlotOpts = PlotOpts
  { _plotOptsColor :: AmbyColor
  , _plotOptsLinewidth :: Double
  } deriving (Show)
makeFields ''PlotOpts

data PlotEqOpts = PlotEqOpts
  { _plotEqOptsColor :: AmbyColor
  , _plotEqOptsLinewidth :: Double
  } deriving (Show)
makeFields ''PlotEqOpts

data DistPlotOpts = DistPlotOpts
  { _distPlotOptsRug :: Bool
  , _distPlotOptsKde :: Bool
  , _distPlotOptsHist :: Bool
  , _distPlotOptsColor :: AmbyColor

  -- hist opts
  , _distPlotOptsHistLinewidth :: Double
  , _distPlotOptsBins :: Int

  -- kde opts
  , _distPlotOptsShade :: Bool
  , _distPlotOptsBw :: Bandwidth
  , _distPlotOptsCut :: Double
  , _distPlotOptsAxis :: Axis
  , _distPlotOptsGridsize :: Int
  , _distPlotOptsKdeLinewidth :: Double

  -- rug opts
  , _distPlotOptsRugHeight :: Double
  , _distPlotOptsRugLinewidth :: Double
  } deriving (Show)
makeFields ''DistPlotOpts

data KdePlotOpts = KdePlotOpts
  { _kdePlotOptsShade :: Bool
  , _kdePlotOptsBw :: Bandwidth
  , _kdePlotOptsAxis :: Axis
  , _kdePlotOptsGridsize :: Int
  , _kdePlotOptsColor :: AmbyColor
  , _kdePlotOptsLinewidth :: Double
  , _kdePlotOptsCut :: Double
  } deriving (Show)
makeFields ''KdePlotOpts

data RugPlotOpts = RugPlotOpts
  { _rugPlotOptsHeight :: Double
  , _rugPlotOptsAxis :: Axis
  , _rugPlotOptsColor :: AmbyColor
  , _rugPlotOptsLinewidth :: Double
  } deriving (Show)
makeFields ''RugPlotOpts

data BoxPlotOpts = BoxPlotOpts
  { _bpoCat :: Category
  , _bpoHue :: Category
  , _boxPlotOptsColor :: AmbyColor
  , _boxPlotOptsSaturation :: Double
  , _boxPlotOptsAxis :: Axis
  , _boxPlotOptsLinewidth :: Double
  , _boxPlotOptsHueLegend :: Bool
  , _boxPlotOptsCatLegend :: Bool
  } deriving (Show)
makeFields ''BoxPlotOpts

data FactorPlotOpts = FactorPlotOpts
  { _fpoCat :: Category
  , _fpoHue :: Category
  , _fpoCol :: Category
  , _fpoRow :: Category
  , _factorPlotOptsColor :: AmbyColor
  , _factorPlotOptsSaturation :: Double
  , _factorPlotOptsAxis :: Axis
  , _factorPlotOptsKind :: PlotKind
  } deriving (Show)
makeFields ''FactorPlotOpts

class HasCat s a b | s -> a where
  cat :: Setter s s a b
instance (Foldable f, Ord a, Show a) => HasCat BoxPlotOpts Category (f a) where
  cat = sets (\a b -> b { _bpoCat = (toCat . a) (_bpoCat b) })
instance HasCat BoxPlotOpts Category Category where
  cat = sets (\a b -> b { _bpoCat = a (_bpoCat b) })
instance (Foldable f, Ord a, Show a) => HasCat FactorPlotOpts Category (f a) where
  cat = sets (\a b -> b { _fpoCat = (toCat . a) (_fpoCat b) })
instance HasCat FactorPlotOpts Category Category where
  cat = sets (\a b -> b { _fpoCat = a (_fpoCat b) })

class HasHue s a b | s -> a where
  hue :: Setter s s a b
instance (Foldable f, Ord a, Show a) => HasHue BoxPlotOpts Category (f a) where
  hue = sets (\a b -> b { _bpoHue = (toCat . a) (_bpoHue b) })
instance HasHue BoxPlotOpts Category Category where
  hue = sets (\a b -> b { _bpoHue = a (_bpoHue b) })
instance (Foldable f, Ord a, Show a) => HasHue FactorPlotOpts Category (f a) where
  hue = sets (\a b -> b { _fpoHue = (toCat . a) (_fpoHue b) })
instance HasHue FactorPlotOpts Category Category where
  hue = sets (\a b -> b { _fpoHue = a (_fpoHue b) })

class HasCol s a b | s -> a where
  col :: Setter s s a b
instance (Foldable f, Ord a, Show a) => HasCol FactorPlotOpts Category (f a) where
  col = sets (\a b -> b { _fpoCol = (toCat . a) (_fpoCol b) })
instance HasCol FactorPlotOpts Category Category where
  col = sets (\a b -> b { _fpoCol = a (_fpoCol b) })

class HasRow s a b | s -> a where
  row :: Setter s s a b
instance (Foldable f, Ord a, Show a) => HasRow FactorPlotOpts Category (f a) where
  row = sets (\a b -> b { _fpoRow = (toCat . a) (_fpoRow b) })
instance HasRow FactorPlotOpts Category Category where
  row = sets (\a b -> b { _fpoRow = a (_fpoRow b) })

-----------------------------------
-- Main types
-----------------------------------

data AmbyState = AmbyState
  { _asThemeState :: Theme
  , _asLayoutState :: EC (Layout Double Double) ()
  , _asSize :: (Int, Int)
  }
makeLenses ''AmbyState

data AmbyGridState = AmbyGridState
  { _agsThemeState :: Theme
  , _agsGrid :: Grid (Renderable (LayoutPick Double Double Double))
  , _agsSize :: (Int, Int)
  }
makeLenses ''AmbyGridState

type AmbyChart a = State AmbyState a
type AmbyGrid a = State AmbyGridState a
type ChartGrid = Grid (Renderable (LayoutPick Double Double Double))

data SaveObject = SaveObject
  { _soSize :: (Int, Int)
  , _soRenderable :: Renderable (LayoutPick Double Double Double)
  }
makeLenses ''SaveObject

class Saveable a where
  toSaveObject :: a -> SaveObject

instance Saveable (AmbyChart ()) where
  toSaveObject ch = SaveObject
      { _soSize = st ^. asSize
      , _soRenderable =
        ( Chart.layoutToRenderable
        . Chart.execEC
        . (^. asLayoutState)
        ) st
      }
    where
      st = execState ch def

instance Saveable (AmbyGrid ()) where
  toSaveObject ch = SaveObject
      { _soSize = st ^. agsSize
      , _soRenderable =
        ( Chart.fillBackground def
        . Chart.gridToRenderable
        . (^. agsGrid)
        ) st
      }
    where
      st = execState ch def

class AmbyContainer c where
  type Value c :: *

  plot :: c -> c -> State PlotOpts () -> AmbyChart ()
  plot' :: c -> c -> AmbyChart ()
  plotEq :: c -> (Value c -> Value c) -> State PlotEqOpts () -> AmbyChart ()
  plotEq' :: c -> (Value c -> Value c) -> AmbyChart ()
  distPlot :: c -> State DistPlotOpts () -> AmbyChart ()
  distPlot' :: c -> AmbyChart ()
  kdePlot :: c -> State KdePlotOpts () -> AmbyChart ()
  kdePlot' :: c -> AmbyChart ()
  rugPlot :: c -> State RugPlotOpts () -> AmbyChart()
  rugPlot' :: c -> AmbyChart ()
  boxPlot :: c -> State BoxPlotOpts () -> AmbyChart ()
  boxPlot' :: c -> AmbyChart ()
  factorPlot :: c -> State FactorPlotOpts () -> AmbyGrid ()

-----------------------------------
-- General options
-----------------------------------

chartToGrid :: AmbyChart () -> Grid (Renderable (LayoutPick Double Double Double))
chartToGrid ch =
    Chart.layoutToGrid
  $ Chart.execEC
  $ getLayout
  $ execState ch def

getLayout :: AmbyState -> EC (Layout Double Double) ()
getLayout s = s ^. asLayoutState

getSize :: SaveObject -> (Int, Int)
getSize s = s ^. soSize

getSaveObjectRenderable :: SaveObject -> Renderable (LayoutPick Double Double Double)
getSaveObjectRenderable so = so ^. soRenderable

takeTheme :: AmbyChart Theme
takeTheme = use asThemeState

takeLayout :: AmbyChart (EC (Layout Double Double) ())
takeLayout = use asLayoutState

putLayout :: EC (Layout Double Double) () -> AmbyChart ()
putLayout l = do
  asLayoutState .= l

theme :: Theme -> AmbyChart ()
theme t = do
  l <- use asLayoutState
  asLayoutState .= do
    l
    Chart.setColors $ t ^. colorCycle
    setThemeStyles t
  asThemeState .= t

gridTheme :: Theme -> AmbyGrid ()
gridTheme t = do
  agsThemeState .= t

xlim :: (Double, Double) -> AmbyChart ()
xlim rs = do
  l <- use asLayoutState
  asLayoutState .= do
    l
    Chart.layout_x_axis . Chart.laxis_generate .= scaledAxisCustom def rs

ylim :: (Double, Double) -> AmbyChart ()
ylim rs = do
  l <- use asLayoutState
  asLayoutState .= do
    l
    Chart.layout_y_axis . Chart.laxis_generate .= scaledAxisCustom def rs

size :: (Int, Int) -> AmbyChart ()
size rs = asSize .= rs

title :: String -> AmbyChart ()
title t = do
  layout <- takeLayout
  putLayout $ do
    layout
    Chart.layout_title .= t
    Chart.layout_title_style . Chart.font_weight .= Chart.FontWeightNormal

gridSize :: (Int, Int) -> AmbyGrid ()
gridSize rs = agsSize .= rs

-- | Scale current grid size by percentage. Scaling will snap to
-- nearest integer point.
gridScale :: (Double, Double) -> AmbyGrid ()
gridScale (sx, sy) = do
  (x, y) <- use agsSize
  agsSize .= (round (fromIntegral x * sx), round (fromIntegral y * sy))

setGrid :: Grid (Renderable (LayoutPick Double Double Double)) -> AmbyGrid ()
setGrid g = agsGrid .= g

--------------------
-- Default instances
--------------------

instance Default AmbyState where
  def = AmbyState
    { _asThemeState = def
    , _asLayoutState = do
      Chart.setColors $ (def :: Theme) ^. colorCycle
      setDefaultThemeStyles def
    , _asSize = _fo_size def
    }

instance Default AmbyGridState where
  def = AmbyGridState
    { _agsThemeState = def
    , _agsGrid = Chart.empty
    , _agsSize = _fo_size def
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

instance Default PlotOpts where
  def = PlotOpts
    { _plotOptsColor = DefaultColor
    , _plotOptsLinewidth = 2.5
    }

instance Default PlotEqOpts where
  def = PlotEqOpts
    { _plotEqOptsColor = DefaultColor
    , _plotEqOptsLinewidth = 2.5
    }

instance Default DistPlotOpts where
  def = DistPlotOpts
    { _distPlotOptsRug = False
    , _distPlotOptsHist = True
    , _distPlotOptsKde = True
    , _distPlotOptsBins = 0
    , _distPlotOptsColor = DefaultColor
    , _distPlotOptsHistLinewidth = 2.5
    , _distPlotOptsAxis = XAxis

    , _distPlotOptsShade = False
    , _distPlotOptsBw = Scott
    , _distPlotOptsGridsize = 100
    , _distPlotOptsKdeLinewidth = 2.5
    , _distPlotOptsCut = 3

    , _distPlotOptsRugHeight = 0.05
    , _distPlotOptsRugLinewidth = 1.2
    }

instance Default KdePlotOpts where
  def = KdePlotOpts
    { _kdePlotOptsShade = False
    , _kdePlotOptsBw = Scott
    , _kdePlotOptsGridsize = 100
    , _kdePlotOptsAxis = XAxis
    , _kdePlotOptsColor = DefaultColor
    , _kdePlotOptsLinewidth = 2.5
    , _kdePlotOptsCut = 3
    }

instance Default RugPlotOpts where
  def = RugPlotOpts
    { _rugPlotOptsHeight = 0.05
    , _rugPlotOptsAxis = XAxis
    , _rugPlotOptsColor = DefaultColor
    , _rugPlotOptsLinewidth = 1.2
    }

instance Default BoxPlotOpts where
  def = BoxPlotOpts
    { _bpoCat = DefaultCategory
    , _bpoHue = DefaultCategory
    , _boxPlotOptsColor = DefaultColor
    , _boxPlotOptsSaturation = 0.8
    , _boxPlotOptsAxis = XAxis
    , _boxPlotOptsLinewidth = 2
    , _boxPlotOptsCatLegend = True
    , _boxPlotOptsHueLegend = True
    }

instance Default FactorPlotOpts where
  def = FactorPlotOpts
    { _fpoCat = DefaultCategory
    , _fpoHue = DefaultCategory
    , _fpoCol = DefaultCategory
    , _fpoRow = DefaultCategory
    , _factorPlotOptsColor = DefaultColor
    , _factorPlotOptsSaturation = 0.8
    , _factorPlotOptsAxis = XAxis
    , _factorPlotOptsKind = Box
    }
