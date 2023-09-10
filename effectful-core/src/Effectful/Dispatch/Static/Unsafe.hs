-- | Unsafe utilities for statically dispatched effects.
module Effectful.Dispatch.Static.Unsafe
  ( reallyUnsafeLiftMapIO
  , reallyUnsafeUnliftIO
  ) where

import Effectful.Internal.Monad

-- | Utility for lifting 'IO' computations of type
--
-- @'IO' a -> 'IO' b@
--
-- to
--
-- @'Eff' es a -> 'Eff' es b@
--
-- This function is __really unsafe__ because:
--
-- - It can be used to introduce arbitrary 'IO' actions into pure 'Eff'
--   computations.
--
-- - The 'IO' computation must run its argument in a way that's perceived as
--   sequential to the outside observer, e.g. in the same thread or in a worker
--   thread that finishes before the argument is run again.
--
-- __Warning:__ if you disregard the second point, you will experience weird
-- bugs, data races or internal consistency check failures.
--
-- When in doubt, use 'Effectful.Dispatch.Static.unsafeLiftMapIO', especially
-- since this version saves only a simple safety check per call of
-- @reallyUnsafeLiftMapIO f@.
reallyUnsafeLiftMapIO :: (IO a -> IO b) -> Eff es a -> Eff es b
reallyUnsafeLiftMapIO f m = unsafeEff $ \es -> f (unEff m es)

-- | Create an unlifting function.
--
-- This function is __really unsafe__ because:
--
-- - It can be used to introduce arbitrary 'IO' actions into pure 'Eff'
--   computations.
--
-- - Unlifted 'Eff' computations must be run in a way that's perceived as
--   sequential to the outside observer, e.g. in the same thread as the caller
--   of 'reallyUnsafeUnliftIO' or in a worker thread that finishes before
--   another unlifted computation is run.
--
-- __Warning:__ if you disregard the second point, you will experience weird
-- bugs, data races or internal consistency check failures.
--
-- When in doubt, use 'Effectful.Dispatch.Static.unsafeSeqUnliftIO', especially
-- since this version saves only a simple safety check per call of the unlifting
-- function.
reallyUnsafeUnliftIO :: ((forall r. Eff es r -> IO r) -> IO a) -> Eff es a
reallyUnsafeUnliftIO k = unsafeEff $ \es -> k (`unEff` es)
