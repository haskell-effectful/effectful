module Effectful.Dispatch.Static
  ( -- * Low level API
    IdE(..)

    -- ** Running
  , runEffect
  , evalEffect
  , execEffect

    -- ** Modification
  , getEffect
  , putEffect
  , stateEffect
  , stateEffectM
  , localEffect

    -- * Primitive API
  , Env
  , Relinker(..)
  , noRelinker

    -- ** Operations
  , emptyEnv
  , cloneEnv
  , forkEnv
  , sizeEnv
  , checkSizeEnv

    -- ** Extending and shrinking
  , unsafeConsEnv
  , unsafeTailEnv

    -- ** Data retrieval and update
  , getEnv
  , putEnv
  , stateEnv
  , modifyEnv

    -- * Unsafe 'Eff' operations
  , unsafeEff
  , unsafeEff_
  , unsafeLiftMapIO
  , unsafeUnliftIO

    -- * Utils
  , unEff
  ) where

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad
