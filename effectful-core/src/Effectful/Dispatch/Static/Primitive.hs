-- | Primitive API for statically dispatched effects.
--
-- /Warning:/ this module exposes internal implementation details of the 'Eff'
-- monad. Most of the time functions from "Effectful.Dispatch.Static" are
-- sufficient and safer to use.
module Effectful.Dispatch.Static.Primitive
  ( -- * The environment
    Env

    -- ** Relinker
  , Relinker(..)
  , dummyRelinker

    -- ** Representation of effects
  , EffectRep

    -- ** Extending and shrinking
  , unsafeConsEnv
  , unsafeTailEnv

    -- ** Data retrieval and update
  , getEnv
  , putEnv
  , stateEnv
  , modifyEnv

    -- ** Utils
  , emptyEnv
  , cloneEnv
  , forkEnv
  , sizeEnv
  , checkSizeEnv
  ) where

import Effectful.Internal.Env
import Effectful.Internal.Monad
