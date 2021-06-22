{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | The 'Eff' monad.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Effectful.Internal.Monad
  ( -- * Monad
    Eff(..)
  , runEff
  , unsafeEff
  , unsafeEff_
  , unsafeUnliftEff

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
  , readerEffectM
  , stateEffectM
  ) where

import Control.Applicative (liftA2)
import Control.Concurrent (myThreadId)
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control
import GHC.Magic (oneShot)

import Effectful.Internal.Effect
import Effectful.Internal.Env

type role Eff nominal representational

newtype Eff (es :: [Effect]) a = Eff { unEff :: Env es -> IO a }

runEff :: Eff '[] a -> IO a
runEff (Eff m) = m =<< emptyEnv

unsafeEff :: (Env es -> IO a) -> Eff es a
unsafeEff m = Eff (oneShot m)

unsafeEff_ :: IO a -> Eff es a
unsafeEff_ m = unsafeEff $ \_ -> m

-- | Lower 'Eff' computations into 'IO'.
unsafeUnliftEff :: ((forall r. Eff es r -> IO r) -> IO a) -> Eff es a
unsafeUnliftEff f = unsafeEff $ \es -> do
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
  fmap f (Eff m) = unsafeEff $ \es -> f <$> m es
  a <$ Eff fb = unsafeEff $ \es -> a <$ fb es

instance Applicative (Eff es) where
  pure = unsafeEff_ . pure
  Eff mf <*> Eff mx = unsafeEff $ \es -> mf es <*> mx es
  Eff ma  *> Eff mb = unsafeEff $ \es -> ma es  *> mb es
  Eff ma <*  Eff mb = unsafeEff $ \es -> ma es <*  mb es
  liftA2 f (Eff ma) (Eff mb) = unsafeEff $ \es -> liftA2 f (ma es) (mb es)

instance Monad (Eff es) where
  return = unsafeEff_ . pure
  Eff m >>= k = unsafeEff $ \es -> m es >>= \a -> unEff (k a) es
  -- https://gitlab.haskell.org/ghc/ghc/-/issues/20008
  Eff ma >> Eff mb = unsafeEff $ \es -> ma es >> mb es

#if __GLASGOW_HASKELL__ < 808
  fail = unsafeEff_ . fail
#endif

----------------------------------------
-- Exception

instance MonadThrow (Eff es) where
  throwM = unsafeEff_ . throwM

instance MonadCatch (Eff es) where
  catch (Eff m) handler = unsafeEff $ \es -> do
    size <- sizeEnv es
    m es `catch` \e -> do
      checkSizeEnv size es
      unEff (handler e) es

instance MonadMask (Eff es) where
  mask k = unsafeEff $ \es -> mask $ \restore ->
    unEff (k $ \(Eff m) -> unsafeEff $ restore . m) es

  uninterruptibleMask k = unsafeEff $ \es -> uninterruptibleMask $ \restore ->
    unEff (k $ \(Eff m) -> unsafeEff $ restore . m) es

  generalBracket acquire release use = unsafeEff $ \es -> mask $ \restore -> do
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

data IOE :: Effect where
  IOE :: IOE m r

runIOE :: Eff '[IOE] a -> IO a
runIOE = runEff . evalEffect (IdE IOE)

instance IOE :> es => MonadIO (Eff es) where
  liftIO = unsafeEff_

instance IOE :> es => MonadBase IO (Eff es) where
  liftBase = unsafeEff_

instance IOE :> es => MonadBaseControl IO (Eff es) where
  type StM (Eff es) a = a
  liftBaseWith = unsafeUnliftEff
  restoreM = pure

instance IOE :> es => MonadUnliftIO (Eff es) where
  withRunInIO = unsafeUnliftEff

----------------------------------------
-- Helpers

runEffect :: i e -> Eff (e : es) a -> Eff es (a, i e)
runEffect e0 (Eff m) = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  bracket (unsafeConsEnv e0 noRelinker es0)
          (unsafeTailEnv size0)
          (\es -> (,) <$> m es <*> getEnv es)

evalEffect :: i e -> Eff (e : es) a -> Eff es a
evalEffect e (Eff m) = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  bracket (unsafeConsEnv e noRelinker es0)
          (unsafeTailEnv size0)
          (\es -> m es)

execEffect :: i e -> Eff (e : es) a -> Eff es (i e)
execEffect e0 (Eff m) = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  bracket (unsafeConsEnv e0 noRelinker es0)
          (unsafeTailEnv size0)
          (\es -> m es *> getEnv es)

getEffect :: e :> es => Eff es (i e)
getEffect = unsafeEff $ \es -> getEnv es
{-# INLINE getEffect #-}

putEffect :: e :> es => i e -> Eff es ()
putEffect e = unsafeEff $ \es -> unsafePutEnv e es
{-# INLINE putEffect #-}

stateEffect :: e :> es => (i e -> (a, i e)) -> Eff es a
stateEffect f = unsafeEff $ \es -> unsafeStateEnv f es
{-# INLINE stateEffect #-}

localEffect :: e :> es => (i e -> i e) -> Eff es a -> Eff es a
localEffect f (Eff m) = unsafeEff $ \es -> do
  bracket (unsafeStateEnv (\e -> (e, f e)) es)
          (\e -> unsafePutEnv e es)
          (\_ -> m es)
{-# INLINE localEffect #-}

readerEffectM :: e :> es => (i e -> Eff es a) -> Eff es a
readerEffectM f = unsafeEff $ \es -> getEnv es >>= \e -> unEff (f e) es
{-# INLINE readerEffectM #-}

stateEffectM :: e :> es => (i e -> Eff es (a, i e)) -> Eff es a
stateEffectM f = unsafeEff $ \es -> do
  (a, e) <- (\e -> unEff (f e) es) =<< getEnv es
  unsafePutEnv e es
  pure a
{-# INLINE stateEffectM #-}
