{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Amby.Theme
  ( Theme(..)
  , AmbyColor(..)
  , Palette

  -- * Themes
  , mutedTheme
  , deepTheme
  , cleanTheme
  , plainTheme
  , easterTheme
  , springTheme

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
  , huslPalette
  , lightPalette
  , desaturate
  , alphaToHsl
  , hslToAlpha
  )
  where

import Control.Lens
import Data.Default
import Data.Fixed (mod')
import qualified Data.Maybe as Maybe
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

import Data.Colour (AlphaColour, opaque)
import qualified Data.Colour as Colour
import Data.Colour.SRGB (sRGB, toSRGB, sRGB24read)
import Data.Colour.RGBSpace (uncurryRGB)
import Data.Colour.RGBSpace.HSL (hsl, hslView)

import Amby.Numeric

type Palette = [AlphaColour Double]

-- | Used to style a chart.
data Theme = Theme
  { _themeBgColor :: AlphaColour Double
  , _themePlotBgColor :: AlphaColour Double
  , _themeGridLineColor :: AlphaColour Double
  , _themeFontFamily :: String
  , _themeFontSize :: Double
  , _themeColorCycle :: Palette
  } deriving (Show)
makeFields ''Theme

instance Default Theme where
  def = plainTheme

-- | Api facing color selection.
data AmbyColor =
    DefaultColor
  | R | G | B | C | M | Y | K | W
  | CustomColor (AlphaColour Double)
  deriving (Show, Eq)

plainTheme :: Theme
plainTheme = Theme
  { _themeBgColor = opaque (sRGB24read "#FFFFFF")
  , _themePlotBgColor = opaque (sRGB24read "#EAEAF2")
  , _themeGridLineColor = opaque (sRGB24read "#FFFFFF")
  , _themeFontFamily = "Verdana"
  , _themeFontSize = 14
  , _themeColorCycle =
    [ opaque (sRGB24read "#4A70B2")
    , opaque (sRGB24read "#52A966")
    , opaque (sRGB24read "#C64D4F")
    , opaque (sRGB24read "#8170B4")
    , opaque (sRGB24read "#CDBA70")
    , opaque (sRGB24read "#60B5CF")
    ]
  }

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

easterTheme :: Theme
easterTheme = Theme
  { _themeBgColor = opaque (sRGB24read "#FFFFFF")
  , _themePlotBgColor = opaque (sRGB24read "#EAEAF2")
  , _themeGridLineColor = opaque (sRGB24read "#FFFFFF")
  , _themeFontFamily = "Verdana"
  , _themeFontSize = 14
  , _themeColorCycle =
    [ opaque (sRGB24read "#8BD3C7")
    , opaque (sRGB24read "#FEFFAF")
    , opaque (sRGB24read "#BFBADA")
    , opaque (sRGB24read "#FB8071")
    , opaque (sRGB24read "#7FB0D3")
    , opaque (sRGB24read "#FEB55A")
    , opaque (sRGB24read "#B2DF60")
    , opaque (sRGB24read "#FCCCE3")
    , opaque (sRGB24read "#D8D6D8")
    , opaque (sRGB24read "#BD7FBE")
    ]
  }

springTheme :: Theme
springTheme = Theme
  { _themeBgColor = opaque (sRGB24read "#FFFFFF")
  , _themePlotBgColor = opaque (sRGB24read "#EAEAF2")
  , _themeGridLineColor = opaque (sRGB24read "#FFFFFF")
  , _themeFontFamily = "Verdana"
  , _themeFontSize = 14
  , _themeColorCycle =
    [ opaque (sRGB24read "#62C3A5")
    , opaque (sRGB24read "#FC8D5C")
    , opaque (sRGB24read "#8C9ECC")
    , opaque (sRGB24read "#E888C4")
    , opaque (sRGB24read "#A5DA48")
    , opaque (sRGB24read "#FFDA00")
    ]
  }

cleanTheme :: Theme
cleanTheme = Theme
  { _themeBgColor = opaque (sRGB24read "#FFFFFF")
  , _themePlotBgColor = opaque (sRGB24read "#FFFFFF")
  , _themeGridLineColor = opaque (sRGB24read "#EEEEEE")
  , _themeFontFamily = "Verdana"
  , _themeFontSize = 14
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
hexToRgba s a = CustomColor $ Colour.withOpacity (sRGB24read s) a

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

-- | Get a set of evenly spaced colors in the HUSL space.
huslPalette :: Int -> Maybe Double -> Maybe Double -> Maybe Double -> Palette
huslPalette n hMay sMay lMay = V.toList huesBoxed
  where
    h = Maybe.fromMaybe 0.01 hMay
    s = Maybe.fromMaybe 0.90 sMay
    l = Maybe.fromMaybe 0.65 lMay
    hues = U.init $ linspace 0 1 (n + 1)
    hues' = (`U.map` hues) $
        (* 359)
      . (`mod'` 1)
      . (+ h)
    huesBoxed = (`V.map` G.convert hues') $
        opaque
      . uncurryRGB sRGB
      . (\hi -> hsl hi s l)

-- | Get sequential palette of colors from light to dark
lightPalette :: AlphaColour Double -> Int -> Palette
lightPalette c n = blendPalette lightColor c n
  where
    (h, s, _) = alphaToHsl c
    lightColor = hslToAlpha h s 0.95

blendPalette :: AlphaColour Double -> AlphaColour Double -> Int -> Palette
blendPalette s e n
  | n < 2 = modErr "blendPalette" "Need at least two colors to blend"
  | n == 2 = [s, e]
  | otherwise =
      V.toList
    $ (`V.snoc` e)
    $ V.cons s
    $ V.map (\x -> Colour.blend x s e)
    $ G.convert
    $ U.tail
    $ U.init
    $ linspace 0 1 n

-- | Desaturate color by a proporation.
desaturate :: Double -> AlphaColour Double -> AlphaColour Double
desaturate p c
  | p < 0 || p > 1 =
    modErr "setSaturation" "Saturation proportion must be between [0, 1]"
  | otherwise = hslToAlpha h (s * p) l
  where
    (h, s, l) = alphaToHsl c

-- | Converts 'AlphaColour Double' to triplet of 'Double's in hsl encoding.
--
-- Examples:
--
-- >>> import qualified Data.Colour.Names as Colour
-- >>> alphaToHsl (opaque Colour.black)
-- (0.0,0.0,0.0)
--
-- >>> alphaToHsl (opaque Colour.blue)
-- (240.0,1.0,0.5)
alphaToHsl :: AlphaColour Double -> (Double, Double, Double)
alphaToHsl c = hslView . toSRGB . (c `Colour.over`) $ Colour.black

-- | Converts hsl triplet of 'Double's to 'AlphaColour Double'.
hslToAlpha :: Double -> Double -> Double -> AlphaColour Double
hslToAlpha h s l
  | h < 0   || s < 0 || l < 0 = modErr
    "hslToAlpha" "hsl accepts values in ([0, 365], [0,1], [0,1])"
  | h > 365 || s > 1 || l > 1 = modErr
    "hslToAlpha" "hsl accepts values in ([0, 365], [0,1], [0,1])"
  | otherwise = opaque . uncurryRGB sRGB $ hsl h s l

modErr :: String -> String -> a
modErr f err = error
  $ showString "Amby.Theme."
  $ showString f
  $ showString ": " err
