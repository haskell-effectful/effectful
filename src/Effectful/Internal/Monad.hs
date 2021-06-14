{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | The 'Eff' monad.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Effectful.Internal.Monad
  ( -- * Monad
    Eff(..)
  , runEff
  , impureEff
  , impureEff_
  , impureUnliftEff

  -- * IO
  , IOE
  , runIOE

  -- * Helpers
  , runEffect
  , evalEffect
  , execEffect
  , getEffect
  , putEffect
  , stateEffect
  , localEffect
  , listenEffect
  , readerEffectM
  , stateEffectM
  ) where

import Control.Concurrent (myThreadId)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control
import GHC.Magic (oneShot)

import Effectful.Internal.Env
import Effectful.Internal.Has

type role Eff nominal representational

newtype Eff (es :: [Effect]) a = Eff { unEff :: Env es -> IO a }

runEff :: Eff '[] a -> IO a
runEff (Eff m) = m =<< emptyEnv

impureEff :: (Env es -> IO a) -> Eff es a
impureEff m = Eff (oneShot m)

impureEff_ :: IO a -> Eff es a
impureEff_ m = impureEff $ \_ -> m

-- | Run 'Eff' computations in 'IO'.
impureUnliftEff :: ((forall r. Eff es r -> IO r) -> IO a) -> Eff es a
impureUnliftEff f = impureEff $ \es -> do
  tid0 <- myThreadId
  f $ \(Eff m) -> do
    tid <- myThreadId
    -- If the unlifting function is called from another thread, we need to clone
    -- the environment. Otherwise multiple threads will attempt to modify it in
    -- different ways and things will break horribly.
    if tid0 == tid
      then m es
      else m =<< cloneEnv es

----------------------------------------
-- Base

instance Functor (Eff es) where
  fmap f (Eff m) = impureEff $ \es -> f <$> m es

instance Applicative (Eff es) where
  pure = impureEff_ . pure
  Eff mf <*> Eff mx = impureEff $ \es -> mf es <*> mx es

instance Monad (Eff es) where
  Eff m >>= k = impureEff $ \es -> do
    a <- m es
    unEff (k a) es

----------------------------------------
-- Exception

instance MonadThrow (Eff es) where
  throwM = impureEff_ . throwM

instance MonadCatch (Eff es) where
  catch (Eff m) handler = impureEff $ \es -> do
    size <- sizeEnv es
    m es `catch` \e -> do
      checkSizeEnv size es
      unEff (handler e) es

instance MonadMask (Eff es) where
  mask k = impureEff $ \es -> mask $ \restore ->
    unEff (k $ \(Eff m) -> impureEff $ restore . m) es

  uninterruptibleMask k = impureEff $ \es -> uninterruptibleMask $ \restore ->
    unEff (k $ \(Eff m) -> impureEff $ restore . m) es

  generalBracket acquire release use = impureEff $ \es -> mask $ \restore -> do
    size <- sizeEnv es
    resource <- unEff acquire es
    b <- restore (unEff (use resource) es) `catch` \e -> do
      checkSizeEnv size es
      _ <- unEff (release resource $ ExitCaseException e) es
      throwM e
    checkSizeEnv size es
    c <- unEff (release resource $ ExitCaseSuccess b) es
    pure (b, c)

----------------------------------------
-- IO

data IOE = IOE

runIOE :: Eff '[IOE] a -> Eff '[] a
runIOE = evalEffect IOE

instance IOE :> es => MonadIO (Eff es) where
  liftIO = impureEff_

instance IOE :> es => MonadBase IO (Eff es) where
  liftBase = impureEff_

instance IOE :> es => MonadBaseControl IO (Eff es) where
  type StM (Eff es) a = a
  liftBaseWith = impureUnliftEff
  restoreM = pure

instance IOE :> es => MonadUnliftIO (Eff es) where
  withRunInIO = impureUnliftEff

----------------------------------------
-- Helpers

runEffect :: e -> Eff (e : es) a -> Eff es (a, e)
runEffect e0 (Eff m) = impureEff $ \es0 -> do
  size0 <- sizeEnv es0
  bracket (unsafeConsEnv e0 es0)
          (unsafeTailEnv size0)
          (\es -> (,) <$> m es <*> getEnv es)

evalEffect :: e -> Eff (e : es) a -> Eff es a
evalEffect e (Eff m) = impureEff $ \es0 -> do
  size0 <- sizeEnv es0
  bracket (unsafeConsEnv e es0)
          (unsafeTailEnv size0)
          (\es -> m es)

execEffect :: e -> Eff (e : es) a -> Eff es e
execEffect e0 (Eff m) = impureEff $ \es0 -> do
  size0 <- sizeEnv es0
  bracket (unsafeConsEnv e0 es0)
          (unsafeTailEnv size0)
          (\es -> m es *> getEnv es)

getEffect :: e :> es => Eff es e
getEffect = impureEff $ \es -> getEnv es

putEffect :: e :> es => e -> Eff es ()
putEffect e = impureEff $ \es -> unsafePutEnv e es

stateEffect :: e :> es => (e -> (a, e)) -> Eff es a
stateEffect f = impureEff $ \es -> unsafeStateEnv f es

localEffect :: e :> es => (e -> e) -> Eff es a -> Eff es a
localEffect f (Eff m) = impureEff $ \es -> do
  bracket (unsafeStateEnv (\e -> (e, f e)) es)
          (\e -> unsafePutEnv e es)
          (\_ -> m es)

listenEffect :: (e :> es, Monoid e) => Eff es a -> Eff es (a, e)
listenEffect (Eff m) = impureEff $ \es -> mask $ \restore -> do
  e0 <- unsafeStateEnv (\e -> (e, mempty)) es
  -- If an exception is thrown, restore e0 and keep parts of e1.
  a <- restore (m es) `onException` unsafeModifyEnv (\e -> e0 `mappend` e) es
  e1 <- unsafeStateEnv (\e -> (e, e0 `mappend` e)) es
  pure (a, e1)

readerEffectM :: e :> es => (e -> Eff es a) -> Eff es a
readerEffectM f = impureEff $ \es -> getEnv es >>= \e -> unEff (f e) es

stateEffectM :: e :> es => (e -> Eff es (a, e)) -> Eff es a
stateEffectM f = impureEff $ \es -> do
  (a, e) <- (\e -> unEff (f e) es) =<< getEnv es
  unsafePutEnv e es
  pure a
