{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
---------------------------------------------------------------------------
-- |
-- Module       : Amby.Container
-- Copyright    : (C) 2016 Justin Sermeno
-- License      : BSD3
-- Maintainer   : Justin Sermeno
--
-- Functions for converting between container types.
---------------------------------------------------------------------------
module Amby.Container
  ( AmbyColumn(..)
  , labeledFrame
  -- , getLabels
  )
  where

import Data.Text (Text)

import Data.Singletons
import Data.Singletons.TypeLits
import Frames (Frame, Record, (:->), (&:))
import qualified Frames as Frames

import Amby.Types

data MyFrame :: * -> * where
  CreateMyFrame ::  Frame (Record '[(s :: Symbol) :-> i]) -> MyFrame i

labeledFrame :: String -> MyFrame Int
labeledFrame s =
    case toSing s of
      SomeSing (SSym :: Sing (n :: Symbol)) ->
        CreateMyFrame
          $ Frames.toFrame
          $ [val &: Frames.Nil] -- :: Frame (Record '[n :-> Int])
  where
    val = 4 :: Int

--getLabels :: MyFrame Int -> String
--getLabels (CreateMyFrame f) =
--  show $ Frames.frameRow f 0

--getLabels :: String
--getLabels = show $ Frames.frameRow f 0
--  where
--    f = Frames.toFrame [(4 :: Int) &: Frames.Nil]

data AmbyColumn c where
  (:--) :: (AmbyContainer c, Show c) => Text -> c -> AmbyColumn c

deriving instance Show (AmbyColumn c)
