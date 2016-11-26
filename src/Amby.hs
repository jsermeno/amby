{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Amby
  (
  -- * Modules
    module Amby.Categorical
  , module Amby.Theme
  , module Amby.Numeric
  , module Amby.Types
  , module Amby.Plot
  , module Amby.Util

  -- * Lens operators
  , (.=)
  ) where

import Graphics.Rendering.Chart.Easy ((.=))

import Amby.Categorical
import Amby.Types
import Amby.Numeric
import Amby.Theme
import Amby.Plot
import Amby.Display ()
import Amby.Util
