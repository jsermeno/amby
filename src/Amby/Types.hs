{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Amby.Types
  ( AmbyContainer(..)
  , AmbyState
  , AmbyChart

  -- * Accessors
  , takeTheme
  , theme
  , xlim
  , ylim
  , takeLayout
  , getLayout
  , putLayout
  )
  where

import Control.Monad.State
import Data.Default.Class
import Graphics.Rendering.Chart.Easy hiding (plot)

import Amby.Theme
import Amby.Style

data AmbyState = AmbyState
  { _themeState :: Theme
  , _layoutState :: EC (Layout Double Double) ()
  }

instance Default AmbyState where
  def = AmbyState
    { _themeState = def
    , _layoutState = do
      setColors (getColorCycle def)
      setThemeStyles def
    }

$( makeLenses ''AmbyState)

type AmbyChart a = State AmbyState a

class AmbyContainer c a | c -> a where
  plot :: c -> c -> AmbyChart ()
  plotEq :: c -> (a -> a) -> AmbyChart ()

getLayout :: AmbyState -> EC (Layout Double Double) ()
getLayout s = s ^. layoutState

takeTheme :: AmbyChart Theme
takeTheme = do
  t <- use themeState
  return t

takeLayout :: AmbyChart (EC (Layout Double Double) ())
takeLayout = do
  l <- use layoutState
  return l

putLayout :: EC (Layout Double Double) () -> AmbyChart ()
putLayout l = do
  layoutState .= l

theme :: Theme -> AmbyChart ()
theme t = do
  l <- use layoutState
  layoutState .= do
    l
    setColors (getColorCycle t)
    setThemeStyles t
  themeState .= t

xlim :: (Double, Double) -> AmbyChart ()
xlim rs = do
  l <- use layoutState
  layoutState .= do
    l
    layout_x_axis . laxis_generate .= scaledAxis def rs

ylim :: (Double, Double) -> AmbyChart ()
ylim rs = do
  l <- use layoutState
  layoutState .= do
    l
    layout_y_axis . laxis_generate .= scaledAxis def rs

