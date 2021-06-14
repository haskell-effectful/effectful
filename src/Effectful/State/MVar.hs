-- | The 'State' as an effect.
--
-- Represented as an MVar underneath, therefore:
--
-- - slower than "Effectful.State"
--
-- - suitable for sharing between multiple threads.
--
module Effectful.State.MVar
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

import Control.Concurrent.MVar

import Effectful.Internal.Has
import Effectful.Internal.Monad

-- | Provide access to a synchronized, mutable state of type @s@.
newtype State s = State (MVar s)

runState :: s -> Eff (State s : es) a -> Eff es (a, s)
runState s m = do
  v <- impureEff_ $ newMVar s
  evalEffect (State v) $ (,) <$> m <*> get

evalState :: s -> Eff (State s : es) a -> Eff es a
evalState s m = do
  v <- impureEff_ $ newMVar s
  evalEffect (State v) m

execState :: s -> Eff (State s : es) a -> Eff es s
execState s m = do
  v <- impureEff_ $ newMVar s
  evalEffect (State v) $ m *> get

get :: State s :> es => Eff es s
get = do
  State v <- getEffect
  impureEff_ $ readMVar v

put :: State s :> es => s -> Eff es ()
put s = do
  State v <- getEffect
  impureEff_ . modifyMVar_ v $ \_ -> s `seq` pure s

state :: State s :> es => (s -> (a, s)) -> Eff es a
state f = do
  State v <- getEffect
  impureEff_ . modifyMVar v $ \s0 -> let (a, s) = f s0 in s `seq` pure (s, a)

modify :: State s :> es => (s -> s) -> Eff es ()
modify f = state (\s -> ((), f s))

stateM :: State s :> es => (s -> Eff es (a, s)) -> Eff es a
stateM f = do
  State v <- getEffect
  impureEff $ \es -> modifyMVar v $ \s0 -> do
    (a, s) <- unEff (f s0) es
    s `seq` pure (s, a)

modifyM :: State s :> es => (s -> Eff es s) -> Eff es ()
modifyM f = stateM (\s -> ((), ) <$> f s)
