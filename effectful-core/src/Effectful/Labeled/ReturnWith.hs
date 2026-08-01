{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Convenience functions for the 'Labeled' 'ReturnWith' effect.
--
-- @since 2.7.0.0
module Effectful.Labeled.ReturnWith
  ( -- * Effect
    ReturnWith(..)

    -- ** Handlers
  , runReturnWith

    -- ** Operations
  , returnWith

    -- * Re-exports
  , Labeled(..)
  ) where

import GHC.Stack (withFrozenCallStack)

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Labeled
import Effectful.ReturnWith.Dynamic (ReturnWith(..))
import Effectful.ReturnWith.Dynamic qualified as R

-- | Run a computation that can return early with a value of type @r@ (via
-- "Effectful.ReturnWith.Static").
runReturnWith
  :: forall label r es
   . HasCallStack
  => Eff (Labeled label (ReturnWith r) : es) r
  -> Eff es r
runReturnWith = runLabeled @label R.runReturnWith

-- | Return early with the given value.
returnWith
  :: forall label r es a
   . (HasCallStack, Labeled label (ReturnWith r) :> es)
  => r
  -- ^ The value.
  -> Eff es a
returnWith = withFrozenCallStack send . Labeled @label . ReturnWith
