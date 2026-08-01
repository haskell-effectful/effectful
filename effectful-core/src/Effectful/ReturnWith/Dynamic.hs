-- | The dynamically dispatched variant of the 'ReturnWith' effect.
--
-- /Note:/ unless you plan to change interpretations at runtime, it's
-- recommended to use the statically dispatched variant,
-- i.e. "Effectful.ReturnWith.Static".
--
-- @since 2.7.0.0
module Effectful.ReturnWith.Dynamic
  ( -- * Effect
    ReturnWith(..)

    -- ** Handlers
  , runReturnWith

    -- ** Operations
  , returnWith
  ) where

import GHC.Stack (withFrozenCallStack)

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.ReturnWith.Static qualified as R

-- | Provide the ability to return early with a value of type @r@.
data ReturnWith r :: Effect where
  ReturnWith :: r -> ReturnWith r m a

type instance DispatchOf (ReturnWith r) = Dynamic

-- | Run a computation that can return early with a value of type @r@ (via
-- "Effectful.ReturnWith.Static").
runReturnWith
  :: HasCallStack
  => Eff (ReturnWith r : es) r
  -> Eff es r
runReturnWith = reinterpret_ R.runReturnWith $ \case
  ReturnWith r -> R.returnWith r

-- | Return early with the given value.
returnWith
  :: (HasCallStack, ReturnWith r :> es)
  => r
  -- ^ The value.
  -> Eff es a
returnWith = withFrozenCallStack send . ReturnWith
