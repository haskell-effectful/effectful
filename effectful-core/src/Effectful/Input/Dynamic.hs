-- | Support for access to read only values of a particular type.
--
-- /Note:/ unless you plan to change interpretations at runtime, it's
-- recommended to use one of the statically dispatched variants,
-- i.e. "Effectful.Input.Static.Action" or "Effectful.Input.Static.Value".
module Effectful.Input.Dynamic
  ( -- * Effect
    Input

    -- ** Handlers
  , runInputAction
  , runInputValue

    -- ** Operations
  , input
  , inputs
  ) where

import Effectful
import Effectful.Dispatch.Dynamic

-- | Provide access to read only values of type @i@.
data Input i :: Effect where
  Input :: Input i m i

type instance DispatchOf (Input i) = Dynamic

----------------------------------------
-- Handlers

-- | Run an 'Input' effect with the given action that supplies values.
runInputAction
  :: forall i es a
   . HasCallStack
  => (HasCallStack => Eff es i)
  -- ^ The action for input generation.
  -> Eff (Input i : es) a
  -> Eff es a
runInputAction inputAction = interpret_ $ \case
  Input -> inputAction

-- | Run an 'Input' effect with the given initial value.
runInputValue
  :: HasCallStack
  => i
  -- ^ The input value.
  -> Eff (Input i : es) a
  -> Eff es a
runInputValue inputValue = interpret_ $ \case
  Input -> pure inputValue

----------------------------------------
-- Operations

-- | Fetch the value.
input :: (HasCallStack, Input i :> es) => Eff es i
input = send Input

-- | Fetch the result of applying a function to the value.
--
-- @'inputs' f ≡ f '<$>' 'input'@
inputs
  :: (HasCallStack, Input i :> es)
  => (i -> a) -- ^ The function to apply to the value.
  -> Eff es a
inputs f = f <$> input
