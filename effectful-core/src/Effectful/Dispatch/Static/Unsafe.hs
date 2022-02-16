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
-- This function is __highly unsafe__ because:
--
-- - It can be used to introduce arbitrary 'IO' actions into pure 'Eff'
--   computations.
--
-- - The 'IO' computation must not run its argument in a different thread, but
--   it's not checked anywhere.
--
-- __If you disregard the second point, segmentation faults await.__
reallyUnsafeLiftMapIO :: (IO a -> IO b) -> Eff es a -> Eff es b
reallyUnsafeLiftMapIO f m = unsafeEff $ \es -> f (unEff m es)

-- | Create an unlifting function.
--
-- This function is __highly unsafe__ because:
--
-- - It can be used to introduce arbitrary 'IO' actions into pure 'Eff'
--   computations.
--
-- - Unlifted 'Eff' computations must not be run in a thread distinct from the
--   caller of 'reallyUnsafeUnliftIO', but it's not checked anywhere.
--
-- __If you disregard the second point, segmentation faults await.__
reallyUnsafeUnliftIO :: ((forall r. Eff es r -> IO r) -> IO a) -> Eff es a
reallyUnsafeUnliftIO k = unsafeEff $ \es -> k (`unEff` es)
