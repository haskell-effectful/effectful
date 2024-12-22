-- | Support for access to a read only value of a particular type.
module Effectful.Input.Static.Value
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

-- | Provide access to a read only value of type @i@.
data Input (i :: Type) :: Effect

type instance DispatchOf (Input i) = Static NoSideEffects
newtype instance StaticRep (Input i) = Input i

-- | Run an 'Input' effect with the given initial value.
runInput
  :: HasCallStack
  => i
  -- ^ The input.
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
