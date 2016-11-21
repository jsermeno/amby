{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Amby.Theme
  ( Theme(..)
  , AmbyColor(..)

  -- * Themes
  , mutedTheme
  , deepTheme
  , cleanTheme

  -- * Lenses
  , bgColor
  , plotBgColor
  , gridLineColor
  , colorCycle
  , fontFamily
  , fontSize

  -- * Color helpers
  , hexToRgb
  , hexToRgba
  , toColour
  )
  where

import Control.Lens
import Data.Colour
import Data.Colour.SRGB
import Data.Default

-- | Used to style a chart.
data Theme = Theme
  { _themeBgColor :: AlphaColour Double
  , _themePlotBgColor :: AlphaColour Double
  , _themeGridLineColor :: AlphaColour Double
  , _themeFontFamily :: String
  , _themeFontSize :: Double
  , _themeColorCycle :: [AlphaColour Double]
  }
makeFields ''Theme

instance Default Theme where
  def = deepTheme

-- | Api facing color selection.
data AmbyColor =
    DefaultColor
  | R | G | B | C | M | Y | K | W
  | CustomColor (AlphaColour Double)
  deriving (Show, Eq)

mutedTheme :: Theme
mutedTheme = Theme
  { _themeBgColor = opaque (sRGB24read "#FFFFFF")
  , _themePlotBgColor = opaque (sRGB24read "#EAEAF2")
  , _themeGridLineColor = opaque (sRGB24read "#FFFFFF")
  , _themeFontFamily = "Verdana"
  , _themeFontSize = 14
  , _themeColorCycle =
    [ opaque (sRGB24read "#4878CF")
    , opaque (sRGB24read "#6ACC65")
    , opaque (sRGB24read "#D65F5F")
    , opaque (sRGB24read "#B47CC7")
    , opaque (sRGB24read "#C4AD66")
    , opaque (sRGB24read "#77BEDB")
    ]
  }

deepTheme :: Theme
deepTheme = Theme
  { _themeBgColor = opaque (sRGB24read "#FFFFFF")
  , _themePlotBgColor = opaque (sRGB24read "#EAEAF2")
  , _themeGridLineColor = opaque (sRGB24read "#FFFFFF")
  , _themeFontFamily = "Verdana"
  , _themeFontSize = 14
  , _themeColorCycle =
    [ opaque (sRGB24read "#4C72B0")
    , opaque (sRGB24read "#55A868")
    , opaque (sRGB24read "#C44E52")
    , opaque (sRGB24read "#8172B2")
    , opaque (sRGB24read "#CCB974")
    , opaque (sRGB24read "#64B5CD")
    ]
  }

cleanTheme :: Theme
cleanTheme = def
  { _themeBgColor = opaque (sRGB24read "#FFFFFF")
  , _themePlotBgColor = opaque (sRGB24read "#FFFFFF")
  , _themeGridLineColor = opaque (sRGB24read "#EEEEEE")
  , _themeColorCycle =
    [ opaque (sRGB24read "#1776B6")
    , opaque (sRGB24read "#FF962A")
    , opaque (sRGB24read "#24A122")
    , opaque (sRGB24read "#CF0407")
    , opaque (sRGB24read "#9564BF")
    ]
  }

------------------------
-- Color helpers
------------------------

hexToRgb :: String -> AmbyColor
hexToRgb s = CustomColor $ opaque (sRGB24read s)

hexToRgba :: String -> Double -> AmbyColor
hexToRgba s a = CustomColor $ withOpacity (sRGB24read s) a

-- | Conversion from Amby Api 'Color' to  underlying 'Colour' type.
toColour :: AmbyColor -> AlphaColour Double -> AlphaColour Double
toColour DefaultColor d = d
toColour (CustomColor c) _ = c
toColour B _ = opaque (sRGB24read "#4878CF")
toColour G _ = opaque (sRGB24read "#6ACC65")
toColour R _ = opaque (sRGB24read "#D65F5F")
toColour M _ = opaque (sRGB24read "#B47CC7")
toColour Y _ = opaque (sRGB24read "#C4AD66")
toColour C _ = opaque (sRGB24read "#77BEDB")
toColour K _ = opaque (sRGB24read "#000000")
toColour W _ = opaque (sRGB24read "#FFFFFF")
