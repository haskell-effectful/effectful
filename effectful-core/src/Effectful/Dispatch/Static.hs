module Effectful.Dispatch.Static
  ( -- * Low level API
    IdA(..)

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

    -- ** Unlifts
  , seqUnliftIO
  , concUnliftIO

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

-- | Utility for lifting 'IO' operations of type
--
-- @'IO' a -> 'IO' b@
--
-- to
--
-- @'Eff' es a -> 'Eff' es b@
--
-- This function is __unsafe__ because:
--
-- - It can be used to introduce arbitrary 'IO' actions into pure 'Eff'
--   operations.
--
-- - The 'IO' operation must not run its argument in a separate thread, but it's
--   not checked anywhere.
unsafeLiftMapIO :: (IO a -> IO b) -> Eff es a -> Eff es b
unsafeLiftMapIO f m = unsafeEff $ \es -> f (unEff m es)

-- | Utility for running 'Eff' computations locally in the 'IO' monad.
--
-- This function is __unsafe__ because:
--
-- - It can be used to introduce arbitrary 'IO' actions into pure 'Eff'
--   operations.
--
-- - Unlifted 'Eff' operations must not be run in a thread distinct from the
--   caller of 'unsafeUnliftIO', but it's not checked anywhere.
unsafeUnliftIO :: ((forall r. Eff es r -> IO r) -> IO a) -> Eff es a
unsafeUnliftIO k = unsafeEff $ \es -> k (`unEff` es)
