{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RequiredTypeArguments #-}
-- | Convenience functions for the 'Labeled' 'State' effect with visible
-- dependent quantification.
--
-- Requires GHC >= 9.10.
--
-- @since 2.4.0.0
module Effectful.Labeled.State.VDQ
  ( -- * Effect
    State(..)

    -- ** Handlers

    -- *** Local
  , runStateLocal
  , evalStateLocal
  , execStateLocal

    -- *** Shared
  , runStateShared
  , evalStateShared
  , execStateShared

    -- ** Operations
  , get
  , gets
  , put
  , state
  , modify
  , stateM
  , modifyM
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Labeled.VDQ
import Effectful.State.Dynamic (State(..))
import Effectful.State.Dynamic qualified as S

----------------------------------------
-- Local

-- | Run the 'State' effect with the given initial state and return the final
-- value along with the final state (via "Effectful.State.Static.Local").
runStateLocal
  :: forall label
  -> s
   -- ^ The initial state.
  -> Eff (Labeled label (State s) : es) a
  -> Eff es (a, s)
runStateLocal label = runLabeled label . S.runStateLocal

-- | Run the 'State' effect with the given initial state and return the final
-- value, discarding the final state (via "Effectful.State.Static.Local").
evalStateLocal
  :: forall label
  -> s
   -- ^ The initial state.
  -> Eff (Labeled label (State s) : es) a
  -> Eff es a
evalStateLocal label = runLabeled label . S.evalStateLocal

-- | Run the 'State' effect with the given initial state and return the final
-- state, discarding the final value (via "Effectful.State.Static.Local").
execStateLocal
  :: forall label
  -> s
   -- ^ The initial state.
  -> Eff (Labeled label (State s) : es) a
  -> Eff es s
execStateLocal label = runLabeled label . S.execStateLocal

----------------------------------------
-- Shared

-- | Run the 'State' effect with the given initial state and return the final
-- value along with the final state (via "Effectful.State.Static.Shared").
runStateShared
  :: forall label
  -> s
   -- ^ The initial state.
  -> Eff (Labeled label (State s) : es) a
  -> Eff es (a, s)
runStateShared label = runLabeled label . S.runStateShared

-- | Run the 'State' effect with the given initial state and return the final
-- value, discarding the final state (via "Effectful.State.Static.Shared").
evalStateShared
  :: forall label
  -> s
   -- ^ The initial state.
  -> Eff (Labeled label (State s) : es) a
  -> Eff es a
evalStateShared label = runLabeled label . S.evalStateShared

-- | Run the 'State' effect with the given initial state and return the final
-- state, discarding the final value (via "Effectful.State.Static.Shared").
execStateShared
  :: forall label
  -> s
   -- ^ The initial state.
  -> Eff (Labeled label (State s) : es) a
  -> Eff es s
execStateShared label = runLabeled label . S.execStateShared

----------------------------------------
-- Operations

-- | Fetch the current value of the state.
get
  :: forall label
  -> (HasCallStack, Labeled label (State s) :> es)
  => Eff es s
get label = send $ Labeled @label Get

-- | Get a function of the current state.
--
-- @'gets' label f ≡ f '<$>' 'get' label@
gets
  :: forall label
  -> (HasCallStack, Labeled label (State s) :> es)
  => (s -> a)
  -- ^ .
  -> Eff es a
gets label f = f <$> get label

-- | Set the current state to the given value.
put
  :: forall label
  -> (HasCallStack, Labeled label (State s) :> es)
  => s
  -- ^ .
  -> Eff es ()
put label = send . Labeled @label . Put

-- | Apply the function to the current state and return a value.
state
  :: forall label
  -> (HasCallStack, Labeled label (State s) :> es)
  => (s -> (a, s))
  -- ^ .
  -> Eff es a
state label = send . Labeled @label . State

-- | Apply the function to the current state.
--
-- @'modify' label f ≡ 'state' label (\\s -> ((), f s))@
modify
  :: forall label
  -> (HasCallStack, Labeled label (State s) :> es)
  => (s -> s)
  -- ^ .
  -> Eff es ()
modify label f = state label (\s -> ((), f s))

-- | Apply the monadic function to the current state and return a value.
stateM
  :: forall label
  -> (HasCallStack, Labeled label (State s) :> es)
  => (s -> Eff es (a, s))
  -- ^ .
  -> Eff es a
stateM label = send . Labeled @label . StateM

-- | Apply the monadic function to the current state.
--
-- @'modifyM' label f ≡ 'stateM' label (\\s -> ((), ) '<$>' f s)@
modifyM
  :: forall label
  -> (HasCallStack, Labeled label (State s) :> es)
  => (s -> Eff es s)
  -- ^ .
  -> Eff es ()
modifyM label f = stateM label (\s -> ((), ) <$> f s)
