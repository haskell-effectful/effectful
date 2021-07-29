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
  ( StateE
  , runStateE
  , evalStateE
  , execStateE
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
newtype StateE s :: Effect where
  StateE :: s -> StateE s m r

runStateE :: s -> Eff (StateE s : es) a -> Eff es (a, s)
runStateE s0 m = do
  (a, IdE (StateE s)) <- runEffect (IdE (StateE s0)) m
  pure (a, s)

evalStateE :: s -> Eff (StateE s : es) a -> Eff es a
evalStateE s = evalEffect (IdE (StateE s))

execStateE :: s -> Eff (StateE s : es) a -> Eff es s
execStateE s0 m = do
  IdE (StateE s) <- execEffect (IdE (StateE s0)) m
  pure s

get :: StateE s :> es => Eff es s
get = do
  IdE (StateE s) <- getEffect
  pure s

gets :: StateE s :> es => (s -> a) -> Eff es a
gets f = f <$> get

put :: StateE s :> es => s -> Eff es ()
put s = putEffect (IdE (StateE s))

state :: StateE s :> es => (s -> (a, s)) -> Eff es a
state f = stateEffect $ \(IdE (StateE s0)) -> let (a, s) = f s0 in (a, IdE (StateE s))

modify :: StateE s :> es => (s -> s) -> Eff es ()
modify f = state (\s -> ((), f s))

stateM :: StateE s :> es => (s -> Eff es (a, s)) -> Eff es a
stateM f = stateEffectM $ \(IdE (StateE s0)) -> do
  (a, s) <- f s0
  pure (a, IdE (StateE s))

modifyM :: StateE s :> es => (s -> Eff es s) -> Eff es ()
modifyM f = stateM (\s -> ((), ) <$> f s)
