-- | The 'State' as an effect.
--
-- Represented as a pure value underneath, therefore:
--
-- - very fast
--
-- - not suitable for sharing between multiple threads.
--
-- If you plan to do the latter, have a look at "Effective.State.MVar" or
-- "Effective.State.Dynamic".
--
module Effective.State
  ( State
  , runState
  , evalState
  , execState
  , get
  , put
  , state
  , modify
  , stateM
  , modifyM
  ) where

import Data.Coerce

import Effective.Internal.Has
import Effective.Internal.Monad

-- | Provide access to a pure, mutable state of type @s@.
newtype State s = State { unState :: s }

runState :: s -> Eff (State s : es) a -> Eff es (a, s)
runState s = coerce . runEffect (State s)

evalState :: s -> Eff (State s : es) a -> Eff es a
evalState s = evalEffect (State s)

execState :: s -> Eff (State s : es) a -> Eff es s
execState s = coerce . execEffect (State s)

get :: State s :> es => Eff es s
get = unState <$> getEffect

put :: State s :> es => s -> Eff es ()
put = putEffect . State

state :: State s :> es => (s -> (a, s)) -> Eff es a
state f = stateEffect $ \(State s0) -> let (a, s) = f s0 in (a, State s)

modify :: State s :> es => (s -> s) -> Eff es ()
modify f = state (\s -> ((), f s))

stateM :: State s :> es => (s -> Eff es (a, s)) -> Eff es a
stateM f = stateEffectM $ \(State s0) -> coerce $ f s0

modifyM :: State s :> es => (s -> Eff es s) -> Eff es ()
modifyM f = stateM (\s -> ((), ) <$> f s)
