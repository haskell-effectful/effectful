{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Convenience functions for the 'Labeled' 'Input' effect.
--
-- @since 2.7.0.0
module Effectful.Labeled.Input
  ( -- * Effect
    Input

    -- ** Handlers
  , runInput
  , runInputAction

    -- ** Operations
  , input
  , inputs
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Labeled
import Effectful.Input.Dynamic (Input(..))
import Effectful.Input.Dynamic qualified as I

-- | Run the 'Input' effect with the given value.
runInput
  :: forall label i es a
   . HasCallStack
  => i
  -- ^ The input value.
  -> Eff (Labeled label (Input i) : es) a
  -> Eff es a
runInput = runLabeled @label . I.runInput

-- | Run the 'Input' effect with the given action that supplies values.
runInputAction
  :: forall label i es a
   . HasCallStack
  => (HasCallStack => Eff es i)
  -- ^ The action for input generation.
  -> Eff (Labeled label (Input i) : es) a
  -> Eff es a
runInputAction = runLabeled @label . I.runInputAction

----------------------------------------
-- Operations

-- | Fetch the value.
input
  :: forall label i es
   . (HasCallStack, Labeled label (Input i) :> es)
  => Eff es i
input = send $ Labeled @label Input

-- | Fetch the result of applying a function to the value.
--
-- @'inputs' f ≡ f '<$>' 'input'@
inputs
  :: forall label i es a
   . (HasCallStack, Labeled label (Input i) :> es)
  => (i -> a) -- ^ The function to apply to the value.
  -> Eff es a
inputs f = f <$> input @label
