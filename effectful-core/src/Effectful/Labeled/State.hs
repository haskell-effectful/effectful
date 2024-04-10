{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
-- | Convenience functions for the 'Labeled' 'State' effect.
--
-- @since 2.4.0.0
module Effectful.Labeled.State
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
import Effectful.Labeled
import Effectful.State.Dynamic (State(..))
import Effectful.State.Dynamic qualified as S

----------------------------------------
-- Local

-- | Run the 'State' effect with the given initial state and return the final
-- value along with the final state (via "Effectful.State.Static.Local").
runStateLocal
  :: forall label s es a
   . s
   -- ^ The initial state.
  -> Eff (Labeled label (State s) : es) a
  -> Eff es (a, s)
runStateLocal = runLabeled @label . S.runStateLocal

-- | Run the 'State' effect with the given initial state and return the final
-- value, discarding the final state (via "Effectful.State.Static.Local").
evalStateLocal
  :: forall label s es a
   . s
   -- ^ The initial state.
  -> Eff (Labeled label (State s) : es) a
  -> Eff es a
evalStateLocal = runLabeled @label . S.evalStateLocal

-- | Run the 'State' effect with the given initial state and return the final
-- state, discarding the final value (via "Effectful.State.Static.Local").
execStateLocal
  :: forall label s es a
   . s
   -- ^ The initial state.
  -> Eff (Labeled label (State s) : es) a
  -> Eff es s
execStateLocal = runLabeled @label . S.execStateLocal

----------------------------------------
-- Shared

-- | Run the 'State' effect with the given initial state and return the final
-- value along with the final state (via "Effectful.State.Static.Shared").
runStateShared
  :: forall label s es a
   . s
   -- ^ The initial state.
  -> Eff (Labeled label (State s) : es) a
  -> Eff es (a, s)
runStateShared = runLabeled @label . S.runStateShared

-- | Run the 'State' effect with the given initial state and return the final
-- value, discarding the final state (via "Effectful.State.Static.Shared").
evalStateShared
  :: forall label s es a
   . s
   -- ^ The initial state.
  -> Eff (Labeled label (State s) : es) a
  -> Eff es a
evalStateShared = runLabeled @label . S.evalStateShared

-- | Run the 'State' effect with the given initial state and return the final
-- state, discarding the final value (via "Effectful.State.Static.Shared").
execStateShared
  :: forall label s es a
   . s
   -- ^ The initial state.
  -> Eff (Labeled label (State s) : es) a
  -> Eff es s
execStateShared = runLabeled @label . S.execStateShared

----------------------------------------
-- Operations

-- | Fetch the current value of the state.
get
  :: forall label s es
   . (HasCallStack, Labeled label (State s) :> es)
  => Eff es s
get = send $ Labeled @label Get

-- | Get a function of the current state.
--
-- @'gets' f ≡ f '<$>' 'get'@
gets
  :: forall label s es a
   . (HasCallStack, Labeled label (State s) :> es)
  => (s -> a)
  -- ^ .
  -> Eff es a
gets f = f <$> get @label

-- | Set the current state to the given value.
put
  :: forall label s es
   . (HasCallStack, Labeled label (State s) :> es)
  => s
  -- ^ .
  -> Eff es ()
put = send . Labeled @label . Put

-- | Apply the function to the current state and return a value.
state
  :: forall label s es a
   . (HasCallStack, Labeled label (State s) :> es)
  => (s -> (a, s))
  -- ^ .
  -> Eff es a
state = send . Labeled @label . State

-- | Apply the function to the current state.
--
-- @'modify' f ≡ 'state' (\\s -> ((), f s))@
modify
  :: forall label s es
   . (HasCallStack, Labeled label (State s) :> es)
  => (s -> s)
  -- ^ .
  -> Eff es ()
modify f = state @label (\s -> ((), f s))

-- | Apply the monadic function to the current state and return a value.
stateM
  :: forall label s es a
   . (HasCallStack, Labeled label (State s) :> es)
  => (s -> Eff es (a, s))
  -- ^ .
  -> Eff es a
stateM = send . Labeled @label . StateM

-- | Apply the monadic function to the current state.
--
-- @'modifyM' f ≡ 'stateM' (\\s -> ((), ) '<$>' f s)@
modifyM
  :: forall label s es
   . (HasCallStack, Labeled label (State s) :> es)
  => (s -> Eff es s)
  -- ^ .
  -> Eff es ()
modifyM f = stateM @label (\s -> ((), ) <$> f s)
