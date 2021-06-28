-- | The 'State' effect.
--
-- Represented as a pure value underneath, therefore:
--
-- - thread local (if you need sharing, have a look at "Effectful.State.MVar"),
--
-- - very fast.
--
module Effectful.State
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

runState :: s -> Eff (State s : es) a -> Eff es (a, s)
runState s0 m = do
  (a, IdE (State s)) <- runEffect (IdE (State s0)) m
  pure (a, s)

evalState :: s -> Eff (State s : es) a -> Eff es a
evalState s = evalEffect (IdE (State s))

execState :: s -> Eff (State s : es) a -> Eff es s
execState s0 m = do
  IdE (State s) <- execEffect (IdE (State s0)) m
  pure s

get :: State s :> es => Eff es s
get = do
  IdE (State s) <- getEffect
  pure s

gets :: State s :> es => (s -> a) -> Eff es a
gets f = f <$> get

put :: State s :> es => s -> Eff es ()
put s = putEffect (IdE (State s))

state :: State s :> es => (s -> (a, s)) -> Eff es a
state f = stateEffect $ \(IdE (State s0)) -> let (a, s) = f s0 in (a, IdE (State s))

modify :: State s :> es => (s -> s) -> Eff es ()
modify f = state (\s -> ((), f s))

stateM :: State s :> es => (s -> Eff es (a, s)) -> Eff es a
stateM f = stateEffectM $ \(IdE (State s0)) -> do
  (a, s) <- f s0
  pure (a, IdE (State s))

modifyM :: State s :> es => (s -> Eff es s) -> Eff es ()
modifyM f = stateM (\s -> ((), ) <$> f s)
