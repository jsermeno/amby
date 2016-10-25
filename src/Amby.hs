{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Amby
  ( save
  , saveSvg

  -- * Plotting methods
  , plot
  , plotEq

  -- * Accessors
  , getEC
  , theme
  , xlim
  , ylim

  -- * Modules
  , module Amby.Theme
  , module Amby.Numeric
  ) where

import Control.Monad
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Vector.Generic as G

import Data.Default.Class
import Graphics.Rendering.Chart.Easy hiding (plot)
import qualified Graphics.Rendering.Chart.Easy as Chart
import qualified Graphics.Rendering.Chart.Backend.Cairo as Cairo
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Diagrams
import Lens.Micro
import Statistics.Distribution

import Amby.Types
import Amby.Numeric
import Amby.Theme
import Amby.Style

cairoDefSave :: FilePath
cairoDefSave = "__amby.png"

diagramsDefSave :: FilePath
diagramsDefSave = "__amby.svg"

getEC :: AmbyChart () -> EC (Layout Double Double) ()
getEC compute = getLayout $ execState compute def

-- | Quick render.
-- Short-hand to render to png file using Cairo backend.
save :: AmbyChart () -> IO ()
save = Cairo.toFile def cairoDefSave . getEC

-- | Short-hand to render to svg using Cairo backend
saveSvg :: AmbyChart () -> IO ()
saveSvg = Diagrams.toFile def diagramsDefSave . getEC

-- | Basic x y line plot.
instance (G.Vector v Double, G.Vector v (Double, Double))
  => AmbyContainer (v Double) Double where
  plot :: v Double -> v Double -> AmbyChart ()
  plot x y = plotList $ G.toList (G.zip x y)

  plotEq :: v Double -> (Double -> Double) -> AmbyChart ()
  plotEq x fn = plotList $ G.toList (G.zip x (G.map fn x))

instance (Real a) => AmbyContainer [a] a where
  plot :: [a] -> [a] -> AmbyChart ()
  plot x y = plotList $ L.zipWith (\a b -> (realToFrac a, realToFrac b)) x y

  plotEq x fn = plotList $ L.zipWith (\a b -> (realToFrac a, realToFrac b)) x (map fn x)

plotList :: [(Double, Double)] -> AmbyChart ()
plotList plotVals = do
    theme <- takeTheme
    layout <- takeLayout
    putLayout $ layout >> mkLayout theme
  where

    linePlot :: EC l (PlotLines Double Double)
    linePlot = liftEC $ do
      nextColor <- takeColor
      plot_lines_values .= [plotVals]
      plot_lines_style . line_width .= 2.5
      plot_lines_style . line_color .= nextColor

    mkLayout :: Theme -> EC (Layout Double Double) ()
    mkLayout theme = do
      Chart.plot linePlot
