-- | The 'State' effect.
--
-- Represented as an 'MVar' underneath, therefore:
--
-- - shareable between multiple threads,
--
-- - slower than "Effectful.State.Local".
--
module Effectful.State.Shared
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

import Control.Concurrent.MVar

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

-- | Provide access to a strict (WHNF), shareable, mutable state of type @s@.
newtype StateE s :: Effect where
  StateE :: MVar s -> StateE s m r

runStateE :: s -> Eff (StateE s : es) a -> Eff es (a, s)
runStateE s m = do
  v <- unsafeEff_ $ newMVar s
  a <- evalEffect (IdE (StateE v)) m
  (a, ) <$> unsafeEff_ (readMVar v)

evalStateE :: s -> Eff (StateE s : es) a -> Eff es a
evalStateE s m = do
  v <- unsafeEff_ $ newMVar s
  evalEffect (IdE (StateE v)) m

execStateE :: s -> Eff (StateE s : es) a -> Eff es s
execStateE s m = do
  v <- unsafeEff_ $ newMVar s
  _ <- evalEffect (IdE (StateE v)) m
  unsafeEff_ $ readMVar v

get :: StateE s :> es => Eff es s
get = unsafeEff $ \es -> do
  IdE (StateE v) <- getEnv es
  readMVar v

gets :: StateE s :> es => (s -> a) -> Eff es a
gets f = f <$> get

put :: StateE s :> es => s -> Eff es ()
put s = unsafeEff $ \es -> do
  IdE (StateE v) <- getEnv es
  modifyMVar_ v $ \_ -> s `seq` pure s

state :: StateE s :> es => (s -> (a, s)) -> Eff es a
state f = unsafeEff $ \es -> do
  IdE (StateE v) <- getEnv es
  modifyMVar v $ \s0 -> let (a, s) = f s0 in s `seq` pure (s, a)

modify :: StateE s :> es => (s -> s) -> Eff es ()
modify f = state (\s -> ((), f s))

stateM :: StateE s :> es => (s -> Eff es (a, s)) -> Eff es a
stateM f = unsafeEff $ \es -> do
  IdE (StateE v) <- getEnv es
  modifyMVar v $ \s0 -> do
    (a, s) <- unEff (f s0) es
    s `seq` pure (s, a)

modifyM :: StateE s :> es => (s -> Eff es s) -> Eff es ()
modifyM f = stateM (\s -> ((), ) <$> f s)
