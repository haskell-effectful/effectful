-- | The dynamically dispatched variant of the 'Reader' effect.
--
-- /Note:/ unless you plan to change interpretations at runtime, it's
-- recommended to use the statically dispatched variant,
-- i.e. "Effectful.Reader.Static".
module Effectful.Reader.Dynamic
  ( -- * Effect
    Reader(..)

    -- ** Handlers
  , runReader
  , withReader

    -- ** Operations
  , ask
  , asks
  , local
  ) where

import Effectful
import Effectful.Dispatch.Dynamic

data Reader r :: Effect where
  Ask   :: Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a

type instance DispatchOf (Reader r) = Dynamic

-- | Run the 'Reader' effect with the given initial environment.
runReader
  :: HasCallStack
  => r -- ^ The initial environment.
  -> Eff (Reader r : es) a
  -> Eff es a
runReader r0 = interpret $ handler r0
  where
    handler :: r -> EffectHandler (Reader r) es
    handler r env = \case
      Ask -> pure r
      Local f action -> localSeqUnlift env $ \unlift -> do
        unlift $ interpose (handler $ f r) action

-- | Execute a computation in a modified environment.
--
-- @since 1.1.0.0
withReader
  :: HasCallStack
  => (r1 -> r2)
  -- ^ The function to modify the environment.
  -> Eff (Reader r2 : es) a
  -- ^ Computation to run in the modified environment.
  -> Eff (Reader r1 : es) a
withReader f m = do
  r <- ask
  raise $ runReader (f r) m
{-# DEPRECATED withReader "withReader doesn't work correctly for all potential interpreters" #-}

----------------------------------------
-- Operations

-- | Fetch the value of the environment.
ask :: (HasCallStack, Reader r :> es) => Eff es r
ask = send Ask

-- | Retrieve a function of the current environment.
--
-- @'asks' f ≡ f '<$>' 'ask'@
asks
  :: (HasCallStack, Reader r :> es)
  => (r -> a) -- ^ The function to apply to the environment.
  -> Eff es a
asks f = f <$> ask

-- | Execute a computation in a modified environment.
--
-- @'runReader' r ('local' f m) ≡ 'runReader' (f r) m@
--
local
  :: (HasCallStack, Reader r :> es)
  => (r -> r) -- ^ The function to modify the environment.
  -> Eff es a
  -> Eff es a
local f = send . Local f
