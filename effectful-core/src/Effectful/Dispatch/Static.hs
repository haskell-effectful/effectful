module Effectful.Dispatch.Static
  ( -- * Low level API
    DataR(..)

    -- ** Extending the environment
  , runData
  , evalData
  , execData

    -- ** Data retrieval and update
  , getData
  , putData
  , stateData
  , stateDataM
  , localData

    -- ** Unlifts
  , seqUnliftIO
  , concUnliftIO

    -- ** Utils
  , unEff
  , unsafeEff
  , unsafeEff_
  , unsafeLiftMapIO
  , unsafeUnliftIO

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
  , Rep
  , getEnv
  , putEnv
  , stateEnv
  , modifyEnv
  ) where

import Effectful.Internal.Env
import Effectful.Internal.Monad

-- | Utility for lifting 'IO' computations of type
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
--   computations.
--
-- - The 'IO' computation must not run its argument in a separate thread, but
--   it's not checked anywhere.
unsafeLiftMapIO :: (IO a -> IO b) -> Eff es a -> Eff es b
unsafeLiftMapIO f m = unsafeEff $ \es -> f (unEff m es)

-- | Utility for running 'Eff' computations locally in the 'IO' monad.
--
-- This function is __unsafe__ because:
--
-- - It can be used to introduce arbitrary 'IO' actions into pure 'Eff'
--   computations.
--
-- - Unlifted 'Eff' computations must not be run in a thread distinct from the
--   caller of 'unsafeUnliftIO', but it's not checked anywhere.
unsafeUnliftIO :: ((forall r. Eff es r -> IO r) -> IO a) -> Eff es a
unsafeUnliftIO k = unsafeEff $ \es -> k (`unEff` es)
