{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Amby.Display where

import Control.Monad.Catch
import Text.Display
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import qualified Amby.Plot as Plot
import Amby.Types

instance {-# OVERLAPPING #-} Display (AmbyChart ()) where
  display a = saveAndDisplay a

saveAndDisplay :: AmbyChart () -> DisplayText
{-# NOINLINE saveAndDisplay #-}
saveAndDisplay chart = unsafePerformIO $ do
  Plot.save chart
  catch readImg cHandler
  where
    readImg = do
      (ec, stdout, stderr) <- readProcessWithExitCode "imgcat" [Plot.cairoDefSave] ""
      if | (ExitFailure code) <- ec -> return (mkDtStr "Unable to display chart.")
         | otherwise -> return (mkDtStr stdout)
    cHandler e
      | Just (e :: SomeException) <- fromException e =
        return $ mkDt "Could not find imgcat executable."
