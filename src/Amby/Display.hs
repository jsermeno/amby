{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Amby.Display where

import Control.Monad.Catch
import Data.Default (def)
import Text.Display
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import Graphics.Rendering.Chart.Easy (Layout, EC)
import qualified Graphics.Rendering.Chart.Backend.Cairo as Cairo

import qualified Amby.Plot as Plot
import Amby.Types

instance {-# OVERLAPPING #-} Display (AmbyChart ()) where
  display a = saveAndDisplay a

instance {-# OVERLAPPING #-} Display (EC (Layout Double Double) ()) where
  display a = saveAndDisplayEC a

saveAndDisplay :: AmbyChart () -> DisplayText
saveAndDisplay chart = saveAndDisplayIO $ do
  Plot.save chart

saveAndDisplayEC :: EC (Layout Double Double) () -> DisplayText
saveAndDisplayEC chart = saveAndDisplayIO $ do
  Cairo.toFile def Plot.cairoDefSave chart

saveAndDisplayIO :: IO () -> DisplayText
{-# NOINLINE saveAndDisplayIO #-}
saveAndDisplayIO ioAction = unsafePerformIO $ do
  ioAction
  catch readImg cHandler
  where
    readImg = do
      (ec, stdout, _) <- readProcessWithExitCode "imgcat" [Plot.cairoDefSave] ""
      if | (ExitFailure _) <- ec -> return (mkDtStr "Unable to display chart.")
         | otherwise -> return (mkDtStr stdout)
    cHandler e
      | Just (_ :: SomeException) <- fromException e =
        return $ mkDt "Could not find imgcat executable."
      | otherwise =
        return $ mkDt "Unknown display error."

