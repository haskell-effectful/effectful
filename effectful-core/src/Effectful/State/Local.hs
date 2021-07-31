-- | The 'State' effect.
--
-- Represented as a pure value underneath, therefore:
--
-- - thread local (if you need the state to be shared between threads, have a
--   look at "Effectful.State.Shared"),
--
-- - very fast.
--
module Effectful.State.Local
  ( State
  , runState
  , evalState
  , execState
  , get
  , gets
  , put
  , state
  , modify
  , stateM
  , modifyM
  ) where

import Effectful.Internal.Effect
import Effectful.Internal.Monad

-- | Provide access to a strict (WHNF), thread local, mutable state of type @s@.
newtype State s :: Effect where
  State :: s -> State s m r

-- | Run a 'State' effect with the given initial state and return the final
-- value along with the final state.
runState
  :: s -- ^ An initial state.
  -> Eff (State s : es) a
  -> Eff es (a, s)
runState s0 m = do
  (a, IdE (State s)) <- runEffect (IdE (State s0)) m
  pure (a, s)

-- | Run a 'State' effect with the given initial state and return the final
-- value, discarding the final state.
evalState
  :: s -- ^ An initial state.
  -> Eff (State s : es) a
  -> Eff es a -- ^ A return value.
evalState s = evalEffect (IdE (State s))

-- | Run a 'State' effect with the given initial state and return the final
-- state, discarding the final value.
execState
  :: s -- ^ An initial state.
  -> Eff (State s : es) a
  -> Eff es s
execState s0 m = do
  IdE (State s) <- execEffect (IdE (State s0)) m
  pure s

-- | Fetch the current value of the state.
get :: State s :> es => Eff es s
get = do
  IdE (State s) <- getEffect
  pure s

-- | Get a function of the current state.
--
-- @'gets' f ≡ f '<$>' 'get'@
gets
  :: State s :> es
  => (s -> a) -- ^ The function to apply to the state.
  -> Eff es a
gets f = f <$> get

-- | Set the current state to the given value.
put :: State s :> es => s -> Eff es ()
put s = putEffect (IdE (State s))

-- | Apply the function to the current state and return a value.
state
  :: State s :> es
  => (s -> (a, s)) -- ^ The function to modify the state.
  -> Eff es a
state f = stateEffect $ \(IdE (State s0)) -> let (a, s) = f s0 in (a, IdE (State s))

-- | Apply the function to the current state.
--
-- @'modify' f ≡ 'state' (\\s -> ((), f s))@
modify
  :: State s :> es
  => (s -> s) -- ^ The function to modify the state.
  -> Eff es ()
modify f = state $ \s -> ((), f s)

-- | Apply the monadic function to the current state and return a value.
stateM
  :: State s :> es
  => (s -> Eff es (a, s)) -- ^ The function to modify the state.
  -> Eff es a
stateM f = stateEffectM $ \(IdE (State s0)) -> do
  (a, s) <- f s0
  pure (a, IdE (State s))

-- | Apply the monadic function to the current state.
--
-- @'modifyM' f ≡ 'stateM' (\\s -> ((), ) '<$>' f s)@
modifyM
  :: State s :> es
  => (s -> Eff es s) -- ^ The monadic function to modify the state.
  -> Eff es ()
modifyM f = stateM (\s -> ((), ) <$> f s)
