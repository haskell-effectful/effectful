module Effectful.Writer.MVar
  ( Writer
  , runWriter
  , execWriter
  , writer
  , tell
  , listen
  , listens
  ) where

import Control.Concurrent.MVar
import Control.Monad.Catch

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

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

writer :: (Writer w :> es, Monoid w) => (a, w) -> Eff es a
writer (a, w1) = do
  IdE (Writer v) <- getEffect
  unsafeEff_ . modifyMVar v $ \w0 -> let w = w0 `mappend` w1 in w `seq` pure (w, a)

tell :: (Writer w :> es, Monoid w) => w -> Eff es ()
tell w1 = do
  IdE (Writer v) <- getEffect
  unsafeEff_ . modifyMVar_ v $ \w0 -> let w = w0 `mappend` w1 in w `seq` pure w

listen
  :: forall w es a. (Writer w :> es, Monoid w)
  => Eff es a
  -> Eff es (a, w)
listen (Eff m) = unsafeEff $ \es -> uninterruptibleMask $ \restore -> do
  v1 <- newMVar mempty
  -- Replace thread local MVar with a fresh one for isolated listening.
  v0 <- unsafeStateEnv (\(IdE (Writer v)) -> (v, IdE (Writer v1))) es
  a <- restore (m es) `onException` merge es v0 v1
  (a, ) <$> merge es v0 v1
  where
    -- Merge results accumulated in the local MVar with the mainline. If an
    -- exception was received while listening, merge results recorded so far.
    merge es v0 v1 = do
      unsafePutEnv @(Writer w) (IdE (Writer v0)) es
      w1 <- readMVar v1
      -- The mask is uninterruptible because modifyMVar_ v0 might block and if
      -- we get an async exception while waiting, w1 will be lost.
      modifyMVar_ v0 $ \w0 -> let w = w0 `mappend` w1 in w `seq` pure w
      pure w1

listens
  :: (Writer w :> es, Monoid w)
  => (w -> b)
  -> Eff es a
  -> Eff es (a, b)
listens f m = do
  (a, w) <- listen m
  pure (a, f w)
