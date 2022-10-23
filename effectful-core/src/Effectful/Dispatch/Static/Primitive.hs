-- | Primitive API for statically dispatched effects.
--
-- This module exposes internal implementation details of the 'Effectful.Eff'
-- monad. Most of the time functions from "Effectful.Dispatch.Static" are
-- sufficient.
--
-- /Warning:/ playing the so called "type tetris" with functions from this
-- module is not enough. Their misuse might lead to memory corruption or
-- segmentation faults, so make sure you understand what you're doing.
module Effectful.Dispatch.Static.Primitive
  ( -- * The environment
    Env

    -- ** Relinker
  , Relinker(..)
  , dummyRelinker

    -- ** Representation of effects
  , EffectRep

    -- ** Extending and shrinking
  , consEnv
  , unconsEnv

    -- ** Data retrieval and update
  , getEnv
  , putEnv
  , stateEnv
  , modifyEnv

    -- ** Utils
  , emptyEnv
  , cloneEnv
  , restoreEnv
  , sizeEnv
  , tailEnv
  ) where

import Effectful.Internal.Env
