{-# OPTIONS_GHC -Wno-orphans #-}
-- | The dynamically dispatched variant of the 'State' effect.
--
-- /Note:/ unless you plan to change interpretations at runtime or you need the
-- 'MTL.MonadState' instance for compatibility with existing code, it's
-- recommended to use one of the statically dispatched variants,
-- i.e. "Effectful.State.Static.Local" or "Effectful.State.Static.Shared".
module Effectful.State.Dynamic
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

import Control.Monad.State qualified as MTL

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local qualified as L
import Effectful.State.Static.Shared qualified as S

-- | Provide access to a mutable value of type @s@.
data State s :: Effect where
  Get    :: State s m s
  Put    :: s -> State s m ()
  State  :: (s ->   (a, s)) -> State s m a
  StateM :: (s -> m (a, s)) -> State s m a

type instance DispatchOf (State s) = Dynamic

----------------------------------------
-- Local

-- | Run the 'State' effect with the given initial state and return the final
-- value along with the final state (via "Effectful.State.Static.Local").
runStateLocal :: HasCallStack => s -> Eff (State s : es) a -> Eff es (a, s)
runStateLocal s0 = reinterpret (L.runState s0) localState

-- | Run the 'State' effect with the given initial state and return the final
-- value, discarding the final state (via "Effectful.State.Static.Local").
evalStateLocal :: HasCallStack => s -> Eff (State s : es) a -> Eff es a
evalStateLocal s0 = reinterpret (L.evalState s0) localState

-- | Run the 'State' effect with the given initial state and return the final
-- state, discarding the final value (via "Effectful.State.Static.Local").
execStateLocal :: HasCallStack => s -> Eff (State s : es) a -> Eff es s
execStateLocal s0 = reinterpret (L.execState s0) localState

localState :: L.State s :> es => EffectHandler (State s) es
localState env = \case
  Get      -> L.get
  Put s    -> L.put s
  State f  -> L.state f
  StateM f -> localSeqUnlift env $ \unlift -> L.stateM (unlift . f)

----------------------------------------
-- Shared

-- | Run the 'State' effect with the given initial state and return the final
-- value along with the final state (via "Effectful.State.Static.Shared").
runStateShared :: HasCallStack => s -> Eff (State s : es) a -> Eff es (a, s)
runStateShared s0 = reinterpret (S.runState s0) sharedState

-- | Run the 'State' effect with the given initial state and return the final
-- value, discarding the final state (via "Effectful.State.Static.Shared").
evalStateShared :: HasCallStack => s -> Eff (State s : es) a -> Eff es a
evalStateShared s0 = reinterpret (S.evalState s0) sharedState

-- | Run the 'State' effect with the given initial state and return the final
-- state, discarding the final value (via "Effectful.State.Static.Shared").
execStateShared :: HasCallStack => s -> Eff (State s : es) a -> Eff es s
execStateShared s0 = reinterpret (S.execState s0) sharedState

sharedState :: S.State s :> es => EffectHandler (State s) es
sharedState env = \case
  Get      -> S.get
  Put s    -> S.put s
  State f  -> S.state f
  StateM f -> localSeqUnlift env $ \unlift -> S.stateM (unlift . f)

----------------------------------------
-- Operations

-- | Fetch the current value of the state.
get
  :: (HasCallStack, State s :> es)
  => Eff es s
get = send Get

-- | Get a function of the current state.
--
-- @'gets' f ≡ f '<$>' 'get'@
gets
  :: (HasCallStack, State s :> es)
  => (s -> a)
  -> Eff es a
gets f = f <$> get

-- | Set the current state to the given value.
put
  :: (HasCallStack, State s :> es)
  => s
  -> Eff es ()
put = send . Put

-- | Apply the function to the current state and return a value.
state
  :: (HasCallStack, State s :> es)
  => (s -> (a, s))
  -> Eff es a
state = send . State

-- | Apply the function to the current state.
--
-- @'modify' f ≡ 'state' (\\s -> ((), f s))@
modify
  :: (HasCallStack, State s :> es)
  => (s -> s)
  -> Eff es ()
modify f = state (\s -> ((), f s))

-- | Apply the monadic function to the current state and return a value.
stateM
  :: (HasCallStack, State s :> es)
  => (s -> Eff es (a, s))
  -> Eff es a
stateM = send . StateM

-- | Apply the monadic function to the current state.
--
-- @'modifyM' f ≡ 'stateM' (\\s -> ((), ) '<$>' f s)@
modifyM
  :: (HasCallStack, State s :> es)
  => (s -> Eff es s)
  -> Eff es ()
modifyM f = stateM (\s -> ((), ) <$> f s)

----------------------------------------
-- Orphan instance

-- | Instance included for compatibility with existing code.
instance
  ( State s :> es
  , MTL.MonadState s (Eff es)
  ) => MTL.MonadState s (Eff es) where
  get = send Get
  put = send . Put
  state = send . State
