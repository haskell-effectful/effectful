-- | The 'State' as an effect with dynamic dispatch.
--
-- It's not clear in which situation it's beneficial to use this instead of
-- "Effective.State" or "Effective.State.MVar" as you either:
--
-- - Share state between threads and need the synchonized version.
--
-- - Don't share state between threads and are free to use the faster, pure
--   version.
--
-- However, let's include this for now.
--
module Effectful.State.Dynamic
  ( State

  -- * Pure
  , runState
  , evalState
  , execState

  -- * MVar
  , runStateMVar
  , evalStateMVar
  , execStateMVar

  -- * Operations
  , get
  , put
  , state
  , modify
  , stateM
  , modifyM
  ) where

import Control.Concurrent.MVar

import Effectful.Internal.Has
import Effectful.Internal.Monad

-- | Provide access to a mutable state of type @s@.
--
-- Whether the state is represented as a pure value or an 'MVar' depends on the
-- interpretation.
data State s = forall ref. State
  { _ref     :: ref
  , _get     :: forall es.   ref -> Eff es s
  , _put     :: forall es.   ref -> s -> Eff es ref
  , _state   :: forall es a. ref -> (s -> Eff es (a, s)) -> Eff es (a, ref)
  }

----------------------------------------
-- Pure

runState :: s -> Eff (State s : es) a -> Eff es (a, s)
runState s m = evalEffect (statePure s) $ (,) <$> m <*> get

evalState :: s -> Eff (State s : es) a -> Eff es a
evalState s = evalEffect (statePure s)

execState :: s -> Eff (State s : es) a -> Eff es s
execState s m = evalEffect (statePure s) $ m *> get

statePure :: s -> State s
statePure s0 = State
  { _ref     = s0
  , _get     = pure
  , _put     = \_ -> pure
  , _state   = \s f -> f s
  }

----------------------------------------
-- MVar

runStateMVar :: s -> Eff (State s : es) a -> Eff es (a, s)
runStateMVar s m = do
  v <- impureEff_ $ newMVar s
  evalEffect (stateMVar v) $ (,) <$> m <*> get

evalStateMVar :: s -> Eff (State s : es) a -> Eff es a
evalStateMVar s m = do
  v <- impureEff_ $ newMVar s
  evalEffect (stateMVar v) m

execStateMVar :: s -> Eff (State s : es) a -> Eff es s
execStateMVar s m = do
  v <- impureEff_ $ newMVar s
  evalEffect (stateMVar v) $ m *> get

stateMVar :: MVar s -> State s
stateMVar v0 = State
  { _ref     = v0
  , _get     = impureEff_ . readMVar
  , _put     = \v s -> impureEff_ . modifyMVar v $ \_ ->
      s `seq` pure (s, v)
  , _state   = \v f -> impureEff $ \es -> modifyMVar v $ \s0 -> do
      (a, s) <- unEff (f s0) es
      s `seq` pure (s, (a, v))
  }

----------------------------------------
-- Operations

get :: State s :> es => Eff es s
get = readerEffectM $ \State{..} -> _get _ref

put :: State s :> es => s -> Eff es ()
put s = stateEffectM $ \State{..} -> do
  ref <- _put _ref s
  pure ((), State { _ref = ref, .. })

state :: State s :> es => (s -> (a, s)) -> Eff es a
state f = stateEffectM $ \State{..} -> do
  (a, ref) <- _state _ref $ pure . f
  pure (a, State { _ref = ref, .. })

modify :: State s :> es => (s -> s) -> Eff es ()
modify f = state (\s -> ((), f s))

stateM :: State s :> es => (s -> Eff es (a, s)) -> Eff es a
stateM f = stateEffectM $ \State{..} -> do
  (a, ref) <- _state _ref f
  pure (a, State { _ref = ref, .. })

modifyM :: State s :> es => (s -> Eff es s) -> Eff es ()
modifyM f = stateM (\s -> ((), ) <$> f s)
