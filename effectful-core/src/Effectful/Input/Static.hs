-- | Support for access to a value of a particular type.
--
-- @since 2.7.0.0
module Effectful.Input.Static
  ( -- * Effect
    Input

    -- ** Handlers
  , runInput

    -- ** Operations
  , input
  , inputs
  ) where

import Data.Kind

import Effectful
import Effectful.Dispatch.Static

-- | Provide access to a value of type @i@.
data Input (i :: Type) :: Effect

type instance DispatchOf (Input i) = Static NoSideEffects
newtype instance StaticRep (Input i) = Input i

-- | Run the 'Input' effect with the given value.
runInput
  :: HasCallStack
  => i
  -- ^ The input value.
  -> Eff (Input i : es) a
  -> Eff es a
runInput = evalStaticRep . Input

-- | Fetch the value.
input :: (HasCallStack, Input i :> es) => Eff es i
input = do
  Input i <- getStaticRep
  pure i

-- | Fetch the result of applying a function to the value.
--
-- @'inputs' f ≡ f '<$>' 'input'@
inputs
  :: (HasCallStack, Input i :> es)
  => (i -> a) -- ^ The function to apply to the value.
  -> Eff es a
inputs f = f <$> input
