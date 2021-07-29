-- | The 'State' effect with dynamic dispatch.
--
-- It's not clear in which situation it's beneficial to use this instead of
-- "Effectful.State.Local" or "Effectful.State.Shared" as you either:
--
-- - Share state between threads and need the shared version.
--
-- - Don't share state between threads (or want it to be thread local) and are
--   free to use the faster, local version.
--
module Effectful.State.Dynamic
  ( StateE(..)

  -- * Local
  , runLocalStateE
  , evalLocalStateE
  , execLocalStateE

  -- * Shared
  , runSharedStateE
  , evalSharedStateE
  , execSharedStateE

  -- * Operations
  , get
  , gets
  , put
  , state
  , modify
  , stateM
  , modifyM
  ) where

import Effectful.Handler
import Effectful.Monad
import qualified Effectful.State.Local as L
import qualified Effectful.State.Shared as S

-- | Provide access to a mutable state of type @s@.
--
-- Whether the state is local or shared depends on the interpretation.
data StateE s :: Effect where
  Get    :: StateE s m s
  Put    :: s -> StateE s m ()
  State  :: (s ->   (a, s)) -> StateE s m a
  StateM :: (s -> m (a, s)) -> StateE s m a

----------------------------------------
-- Local

runLocalStateE :: s -> Eff (StateE s : es) a -> Eff es (a, s)
runLocalStateE s0 = reinterpret (L.runStateE s0) localState

evalLocalStateE :: s -> Eff (StateE s : es) a -> Eff es a
evalLocalStateE s0 = reinterpret (L.evalStateE s0) localState

execLocalStateE :: s -> Eff (StateE s : es) a -> Eff es s
execLocalStateE s0 = reinterpret (L.execStateE s0) localState

localState
  :: L.StateE s :> es
  => LocalEnv localEs
  -> StateE s (Eff localEs) a
  -> Eff es a
localState env = \case
  Get      -> L.get
  Put s    -> L.put s
  State f  -> L.state f
  StateM f -> localSeqUnlift env $ \unlift -> L.stateM (unlift . f)

----------------------------------------
-- Shared

runSharedStateE :: s -> Eff (StateE s : es) a -> Eff es (a, s)
runSharedStateE s0 = reinterpret (S.runStateE s0) sharedState

evalSharedStateE :: s -> Eff (StateE s : es) a -> Eff es a
evalSharedStateE s0 = reinterpret (S.evalStateE s0) sharedState

execSharedStateE :: s -> Eff (StateE s : es) a -> Eff es s
execSharedStateE s0 = reinterpret (S.execStateE s0) sharedState

sharedState
  :: S.StateE s :> es
  => LocalEnv localEs
  -> StateE s (Eff localEs) a
  -> Eff es a
sharedState env = \case
  Get      -> S.get
  Put s    -> S.put s
  State f  -> S.state f
  StateM f -> localSeqUnlift env $ \unlift -> S.stateM (unlift . f)

----------------------------------------
-- Operations

get
  :: (HasCallStack, StateE s :> es)
  => Eff es s
get = send Get

gets
  :: (HasCallStack, StateE s :> es)
  => (s -> a)
  -> Eff es a
gets f = f <$> get

put
  :: (HasCallStack, StateE s :> es)
  => s
  -> Eff es ()
put = send . Put

state
  :: (HasCallStack, StateE s :> es)
  => (s -> (a, s))
  -> Eff es a
state = send . State

modify
  :: (HasCallStack, StateE s :> es)
  => (s -> s)
  -> Eff es ()
modify f = state (\s -> ((), f s))

stateM
  :: (HasCallStack, StateE s :> es)
  => (s -> Eff es (a, s))
  -> Eff es a
stateM = send . StateM

modifyM
  :: (HasCallStack, StateE s :> es)
  => (s -> Eff es s)
  -> Eff es ()
modifyM f = stateM (\s -> ((), ) <$> f s)
