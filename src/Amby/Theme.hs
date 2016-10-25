module Amby.Theme
  ( Theme

  -- * Themes
  , mutedTheme
  , deepTheme
  , cleanTheme

  -- * Accessors
  , getBgColor
  , getPlotBgColor
  , getGridLineColor
  , getColorCycle
  , getFontFamily
  , getFontSize
  )
  where

import Data.Colour
import Data.Colour.SRGB
import Data.Default.Class

data Theme = Theme
  { bgColor :: AlphaColour Double
  , plotBgColor :: AlphaColour Double
  , gridLineColor :: AlphaColour Double
  , fontFamily :: String
  , fontSize :: Double
  , colorCycle :: [AlphaColour Double]
  }

instance Default Theme where
  def = deepTheme

mutedTheme :: Theme
mutedTheme = Theme
  { bgColor = opaque (sRGB24read "#FFFFFF")
  , plotBgColor = opaque (sRGB24read "#EAEAF2")
  , gridLineColor = opaque (sRGB24read "#FFFFFF")
  , fontFamily = "Verdana"
  , fontSize = 14
  , colorCycle =
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
  { bgColor = opaque (sRGB24read "#FFFFFF")
  , plotBgColor = opaque (sRGB24read "#EAEAF2")
  , gridLineColor = opaque (sRGB24read "#FFFFFF")
  , fontFamily = "Verdana"
  , fontSize = 14
  , colorCycle =
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
  { bgColor = opaque (sRGB24read "#FFFFFF")
  , plotBgColor = opaque (sRGB24read "#FFFFFF")
  , gridLineColor = opaque (sRGB24read "#EEEEEE")
  , colorCycle =
    [ opaque (sRGB24read "#1776B6")
    , opaque (sRGB24read "#FF962A")
    , opaque (sRGB24read "#24A122")
    , opaque (sRGB24read "#CF0407")
    , opaque (sRGB24read "#9564BF")
    ]
  }

getBgColor :: Theme -> AlphaColour Double
getBgColor = bgColor

getPlotBgColor :: Theme -> AlphaColour Double
getPlotBgColor = plotBgColor

getGridLineColor :: Theme -> AlphaColour Double
getGridLineColor = gridLineColor

getColorCycle :: Theme -> [AlphaColour Double]
getColorCycle = colorCycle

getFontFamily :: Theme -> String
getFontFamily = fontFamily

getFontSize :: Theme -> Double
getFontSize = fontSize
