-- | The 'State' effect with dynamic dispatch.
module Effectful.State.Dynamic
  ( State(..)

  -- * Local
  , runLocalState
  , evalLocalState
  , execLocalState

  -- * Shared
  , runSharedState
  , evalSharedState
  , execSharedState

  -- * Operations
  , get
  , gets
  , put
  , state
  , modify
  , stateM
  , modifyM
  ) where

import Effectful.Dispatch.Dynamic
import Effectful.Monad
import qualified Effectful.State.Local as L
import qualified Effectful.State.Shared as S

-- | Provide access to a mutable value of type @s@.
data State s :: Effect where
  Get    :: State s m s
  Put    :: s -> State s m ()
  State  :: (s ->   (a, s)) -> State s m a
  StateM :: (s -> m (a, s)) -> State s m a

----------------------------------------
-- Local

runLocalState :: s -> Eff (State s : es) a -> Eff es (a, s)
runLocalState s0 = reinterpret (L.runState s0) localState

evalLocalState :: s -> Eff (State s : es) a -> Eff es a
evalLocalState s0 = reinterpret (L.evalState s0) localState

execLocalState :: s -> Eff (State s : es) a -> Eff es s
execLocalState s0 = reinterpret (L.execState s0) localState

localState
  :: L.State s :> es
  => LocalEnv localEs es
  -> State s (Eff localEs) a
  -> Eff es a
localState env = \case
  Get      -> L.get
  Put s    -> L.put s
  State f  -> L.state f
  StateM f -> localSeqUnlift env $ \unlift -> L.stateM (unlift . f)

----------------------------------------
-- Shared

runSharedState :: s -> Eff (State s : es) a -> Eff es (a, s)
runSharedState s0 = reinterpret (S.runState s0) sharedState

evalSharedState :: s -> Eff (State s : es) a -> Eff es a
evalSharedState s0 = reinterpret (S.evalState s0) sharedState

execSharedState :: s -> Eff (State s : es) a -> Eff es s
execSharedState s0 = reinterpret (S.execState s0) sharedState

sharedState
  :: S.State s :> es
  => LocalEnv localEs es
  -> State s (Eff localEs) a
  -> Eff es a
sharedState env = \case
  Get      -> S.get
  Put s    -> S.put s
  State f  -> S.state f
  StateM f -> localSeqUnlift env $ \unlift -> S.stateM (unlift . f)

----------------------------------------
-- Operations

get
  :: (HasCallStack, State s :> es)
  => Eff es s
get = send Get

gets
  :: (HasCallStack, State s :> es)
  => (s -> a)
  -> Eff es a
gets f = f <$> get

put
  :: (HasCallStack, State s :> es)
  => s
  -> Eff es ()
put = send . Put

state
  :: (HasCallStack, State s :> es)
  => (s -> (a, s))
  -> Eff es a
state = send . State

modify
  :: (HasCallStack, State s :> es)
  => (s -> s)
  -> Eff es ()
modify f = state (\s -> ((), f s))

stateM
  :: (HasCallStack, State s :> es)
  => (s -> Eff es (a, s))
  -> Eff es a
stateM = send . StateM

modifyM
  :: (HasCallStack, State s :> es)
  => (s -> Eff es s)
  -> Eff es ()
modifyM f = stateM (\s -> ((), ) <$> f s)
