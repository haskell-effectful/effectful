-- | Support for accumulation of values in a thread local list.
--
-- @since 2.7.0.0
module Effectful.Output.Static.Local.List
  ( -- * Effect
    Output

    -- ** Handlers
  , runOutput

    -- ** Operations
  , output
  ) where

import Data.Kind

import Effectful
import Effectful.Dispatch.Static

-- | Provide access to accumulation of values of type @o@ in a thread local
-- list.
data Output (o :: Type) :: Effect

type instance DispatchOf (Output o) = Static NoSideEffects
newtype instance StaticRep (Output o) = Output [o]

-- | Run the 'Output' effect and return the final value along with the
-- accumulated list.
runOutput :: HasCallStack => Eff (Output o : es) a -> Eff es (a, [o])
runOutput action = do
  (a, Output acc) <- runStaticRep (Output []) action
  pure (a, reverse acc)

-- | Append the value to the end of the list.
output
  :: (HasCallStack, Output o :> es)
  => o -- ^ The value.
  -> Eff es ()
output !o = stateStaticRep $ \(Output acc) -> ((), Output (o : acc))
