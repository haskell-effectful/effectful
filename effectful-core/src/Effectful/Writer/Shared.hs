module Effectful.Writer.Shared
  ( Writer
  , runWriter
  , execWriter
  , tell
  , listen
  , listens
  ) where

import Control.Concurrent.MVar
import Control.Exception

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

-- | Provide access to a strict (WHNF), shareable, write only value of type @w@.
newtype Writer w :: Effect where
  Writer :: MVar w -> Writer w m r

runWriter :: Monoid w => Eff (Writer w : es) a -> Eff es (a, w)
runWriter m = do
  v <- unsafeEff_ $ newMVar mempty
  a <- evalEffect (IdE (Writer v)) m
  (a, ) <$> unsafeEff_ (readMVar v)

execWriter :: Monoid w => Eff (Writer w : es) a -> Eff es w
execWriter m = do
  v <- unsafeEff_ $ newMVar mempty
  _ <- evalEffect (IdE (Writer v)) m
  unsafeEff_ $ readMVar v

tell :: (Writer w :> es, Monoid w) => w -> Eff es ()
tell w1 = unsafeEff $ \es -> do
  IdE (Writer v) <- getEnv es
  modifyMVar_ v $ \w0 -> let w = w0 <> w1 in w `seq` pure w

listen :: (Writer w :> es, Monoid w) => Eff es a -> Eff es (a, w)
listen m = unsafeEff $ \es -> do
  -- The mask is uninterruptible because modifyMVar_ v0 in the merge function
  -- might block and if an async exception is received while waiting, w1 will be
  -- lost.
  uninterruptibleMask $ \restore -> do
    v1 <- newMVar mempty
    -- Replace thread local MVar with a fresh one for isolated listening.
    v0 <- unsafeStateEnv es $ \(IdE (Writer v)) -> (v, IdE (Writer v1))
    a <- restore (unEff m es) `onException` merge es v0 v1
    (a, ) <$> merge es v0 v1
  where
    -- Merge results accumulated in the local MVar with the mainline. If an
    -- exception was received while listening, merge results recorded so far.
    merge es v0 v1 = do
      unsafePutEnv es $ IdE (Writer v0)
      w1 <- readMVar v1
      modifyMVar_ v0 $ \w0 -> let w = w0 <> w1 in w `seq` pure w
      pure w1

listens :: (Writer w :> es, Monoid w) => (w -> b) -> Eff es a -> Eff es (a, b)
listens f m = do
  (a, w) <- listen m
  pure (a, f w)
