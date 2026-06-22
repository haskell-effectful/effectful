{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Convenience functions for the 'Labeled' 'Output' effect.
--
-- @since 2.7.0.0
module Effectful.Labeled.Output
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

    -- * Re-exports
  , Labeled(..)
  , Array
  ) where

import Data.Primitive.Array

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Labeled
import Effectful.Output.Dynamic (Output(..))
import Effectful.Output.Dynamic qualified as O

----------------------------------------
-- Handlers

-- | Run the 'Output' effect with the given action for receiving values.
runOutputAction
  :: forall label o es a
   . HasCallStack
  => (HasCallStack => o -> Eff es ())
  -- ^ The action for output generation.
  -> Eff (Labeled label (Output o) : es) a
  -> Eff es a
runOutputAction = runLabeled @label . O.runOutputAction

-- | Run the 'Output' effect and return the final value along with the
-- accumulated array (via "Effectful.Output.Static.Local.Array").
runOutputLocalArray
  :: forall label o es a
   . HasCallStack
  => Eff (Labeled label (Output o) : es) a
  -> Eff es (a, Array o)
runOutputLocalArray = runLabeled @label O.runOutputLocalArray

-- | Run the 'Output' effect and return the final value along with the
-- accumulated list (via "Effectful.Output.Static.Local.List").
runOutputLocalList
  :: forall label o es a
   . HasCallStack
  => Eff (Labeled label (Output o) : es) a
  -> Eff es (a, [o])
runOutputLocalList = runLabeled @label O.runOutputLocalList

-- | Run the 'Output' effect and return the final value along with the
-- accumulated array (via "Effectful.Output.Static.Shared.Array").
runOutputSharedArray
  :: forall label o es a
   . HasCallStack
  => Eff (Labeled label (Output o) : es) a
  -> Eff es (a, Array o)
runOutputSharedArray = runLabeled @label O.runOutputSharedArray

-- | Run the 'Output' effect and return the final value along with the
-- accumulated list (via "Effectful.Output.Static.Shared.List").
runOutputSharedList
  :: forall label o es a
    . HasCallStack
  => Eff (Labeled label (Output o) : es) a
  -> Eff es (a, [o])
runOutputSharedList = runLabeled @label O.runOutputSharedList

----------------------------------------
-- Operations

-- | Feed the value to the underlying handler.
output
  :: forall label o es
   . (HasCallStack, Labeled label (Output o) :> es)
  => o
  -> Eff es ()
output = send . Labeled @label . Output
