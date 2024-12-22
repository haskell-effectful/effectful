-- | The dynamically dispatched variant of the 'Output' effect.
--
-- /Note:/ unless you plan to change interpretations at runtime, it's
-- recommended to use one of the statically dispatched variants,
-- i.e. "Effectful.Output.Static.Action", "Effectful.Output.Static.Local.Array",
-- "Effectful.Output.Static.Local.List", "Effectful.Output.Static.Shared.Array"
-- or "Effectful.Output.Static.Shared.List".
--
-- @since 2.7.0.0
module Effectful.Output.Dynamic
  ( -- * Effect
    Output(..)

    -- ** Handlers
  , runOutputAction
  , runOutputLocalArray
  , runOutputLocalList
  , runOutputSharedArray
  , runOutputSharedList

    -- ** Operations
  , output
  ) where

import Data.Primitive.Array

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Output.Static.Local.Array qualified as LA
import Effectful.Output.Static.Local.List qualified as LL
import Effectful.Output.Static.Shared.Array qualified as SA
import Effectful.Output.Static.Shared.List qualified as SL

-- | Provide the ability to feed values of type @o@ to a handler.
data Output o :: Effect where
  Output :: o -> Output o m ()

type instance DispatchOf (Output o) = Dynamic

----------------------------------------
-- Handlers

-- | Run the 'Output' effect with the given action for receiving values.
runOutputAction
  :: forall o es a
   . HasCallStack
  => (HasCallStack => o -> Eff es ())
  -- ^ The action for output generation.
  -> Eff (Output o : es) a
  -> Eff es a
runOutputAction outputAction = interpret_ $ \case
  Output o -> outputAction $! o

-- | Run the 'Output' effect and return the final value along with the
-- accumulated array (via "Effectful.Output.Static.Local.Array").
runOutputLocalArray :: HasCallStack => Eff (Output o : es) a -> Eff es (a, Array o)
runOutputLocalArray = reinterpret_ LA.runOutput $ \case
  Output o -> LA.output o

-- | Run the 'Output' effect and return the final value along with the
-- accumulated list (via "Effectful.Output.Static.Local.List").
runOutputLocalList :: HasCallStack => Eff (Output o : es) a -> Eff es (a, [o])
runOutputLocalList = reinterpret_ LL.runOutput $ \case
  Output o -> LL.output o

-- | Run the 'Output' effect and return the final value along with the
-- accumulated array (via "Effectful.Output.Static.Shared.Array").
runOutputSharedArray :: HasCallStack => Eff (Output o : es) a -> Eff es (a, Array o)
runOutputSharedArray = reinterpret_ SA.runOutput $ \case
  Output o -> SA.output o

-- | Run the 'Output' effect and return the final value along with the
-- accumulated list (via "Effectful.Output.Static.Shared.List").
runOutputSharedList :: HasCallStack => Eff (Output o : es) a -> Eff es (a, [o])
runOutputSharedList = reinterpret_ SL.runOutput $ \case
  Output o -> SL.output o

----------------------------------------
-- Operations

-- | Feed the value to the underlying handler.
output
  :: (HasCallStack, Output o :> es)
  => o -- ^ The value.
  -> Eff es ()
output = send . Output
