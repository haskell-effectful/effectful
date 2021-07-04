{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | The 'Eff' monad.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Effectful.Internal.Monad
  ( -- * Monad
    Eff
  , runEff

  -- ** Access to the internal representation
  , RunIn
  , unEff
  , unsafeEff
  , unsafeEff_
  , unsafeUnliftEff

  -- * Fail
  , Fail
  , runFail

  -- * IO
  , IOE
  , runIOE

  -- * Low-level helpers

  -- ** Running
  , runEffect
  , evalEffect
  , execEffect

  -- ** Modification
  , getEffect
  , putEffect
  , stateEffect
  , localEffect
  , readerEffectM
  , stateEffectM
  ) where

import Control.Applicative (liftA2)
import Control.Concurrent (myThreadId)
import Control.Exception
import Control.Monad.Base
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as M
import Data.Unique
import GHC.Magic (oneShot)
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Control.Monad.Catch as E

import Effectful.Internal.Effect
import Effectful.Internal.Env

type role Eff nominal representational

-- | The 'Eff' monad provides the implementation of an operation that performs
-- an arbitrary set of effects. In @'Eff' es a@, @es@ is a type-level list that
-- contains all the effects that the operation may perform. For example, an
-- operation that produces an 'Integer' by consuming a 'String' from the global
-- environment and acting upon a single mutable cell containing a 'Bool' would
-- have the following type:
--
-- @
-- 'Eff' '['Effectful.Reader.Reader' 'String', 'Effectful.State.State' 'Bool'] 'Integer'
-- @
--
-- Normally, a concrete list of effects is not used to parameterize 'Eff'.
-- Instead, the '(:>)' class is used to express constraints on the list of
-- effects without coupling an operation to a concrete list of effects. For
-- example, the above example would more commonly be expressed with the
-- following type:
--
-- @
-- ('Effectful.Reader.Reader' 'String' ':>' es, 'Effectful.State.State' 'Bool' ':>' es) => 'Eff' es 'Integer'
-- @
--
-- This abstraction allows the operation to be used in functions that may
-- perform other effects, and it also allows the effects to be handled in any
-- order.
newtype Eff (es :: [Effect]) a = Eff (Env es -> IO a)

-- | Run a pure 'Eff' operation.
--
-- For the impure version see 'runIOE'.
runEff :: Eff '[] a -> a
runEff (Eff m) =
  -- unsafePerformIO is safe here since IOE was not on the stack, so no IO with
  -- side effects was performed (unless someone sneakily introduced side effects
  -- with unsafeEff, but then all bets are off).
  --
  -- Moreover, internals don't allocate any resources that require explicit
  -- cleanup actions to run, so the "Dupable" part should also be just fine.
  unsafeDupablePerformIO $ m =<< emptyEnv

----------------------------------------
-- Access to the internal representation

-- | Peel off the constructor of 'Eff'.
unEff :: Eff es a -> Env es -> IO a
unEff (Eff m) = m

-- | Access underlying 'IO' monad along with the environment.
unsafeEff :: (Env es -> IO a) -> Eff es a
unsafeEff m = Eff (oneShot m)

-- | Access underlying 'IO' monad.
unsafeEff_ :: IO a -> Eff es a
unsafeEff_ m = unsafeEff $ \_ -> m

-- | A function that runs 'Eff' operations in @m@ (usually a local 'Eff'
-- environment or 'IO').
type RunIn es m = forall r. Eff es r -> m r

-- | Lower 'Eff' operations into 'IO'.
--
-- /Note:/ the 'RunIn' argument has to be called from the same thread as
-- 'unsafeUnliftEff' ('liftBaseWith' / 'withRunInIO'). Attempting otherwise
-- results in a runtime error, because it cannot be done safely in general
-- (*). If you need to spawn threads, have a look at "Effectful.Async" or create
-- a new 'Eff' environment.
--
-- (*) Consider the following:
--
-- @
-- runState 'x' $ do
--   liftBaseWith $ \run -> forkIO $ do
--     threadDelay 1000000
--     run $ ... (1)
--   ... (2)
-- ...
-- @
--
-- If (1) runs after (2) completed, the State Char effect is no longer in scope,
-- but (1) assumes otherwise and things break horribly.
unsafeUnliftEff :: (RunIn es IO -> IO a) -> Eff es a
unsafeUnliftEff f = unsafeEff $ \es0 -> do
  tid0 <- myThreadId
  envsRef <- newIORef M.empty
  es <- cloneEnv es0
  f $ \m -> do
    tid <- myThreadId
    if tid == tid0
      then unEff m es0
      else do
        mes <- M.lookup tid <$> readIORef envsRef
        case mes of
          Nothing -> do
            es' <- cloneEnv es
            atomicModifyIORef' envsRef $ \envs ->
              (M.insert tid es' envs, ())
            unEff m es'
          Just es' -> unEff m es'

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

instance MonadFix (Eff es) where
  mfix f = unsafeEff $ \es -> mfix $ \a -> unEff (f a) es

----------------------------------------
-- Exception

instance E.MonadThrow (Eff es) where
  throwM = unsafeEff_ . throwIO

instance E.MonadCatch (Eff es) where
  catch m handler = unsafeEff $ \es -> do
    size <- sizeEnv es
    unEff m es `catch` \e -> do
      checkSizeEnv size es
      unEff (handler e) es
  {-# INLINABLE catch #-}

instance E.MonadMask (Eff es) where
  mask k = unsafeEff $ \es -> mask $ \restore ->
    unEff (k $ \m -> unsafeEff $ restore . unEff m) es

  uninterruptibleMask k = unsafeEff $ \es -> uninterruptibleMask $ \restore ->
    unEff (k $ \m -> unsafeEff $ restore . unEff m) es

  generalBracket acquire release use = unsafeEff $ \es -> mask $ \restore -> do
    size <- sizeEnv es
    resource <- unEff acquire es
    b <- restore (unEff (use resource) es) `catch` \e -> do
      checkSizeEnv size es
      _ <- unEff (release resource $ E.ExitCaseException e) es
      throwIO e
    checkSizeEnv size es
    c <- unEff (release resource $ E.ExitCaseSuccess b) es
    pure (b, c)
  {-# INLINABLE generalBracket #-}

----------------------------------------
-- Fail

data Fail :: Effect where
  Fail :: Unique -> Fail m r

runFail :: Eff (Fail : es) a -> Eff es (Either String a)
runFail m = unsafeEff $ \es0 -> mask $ \release -> do
  -- A unique tag is picked so that different runFail handlers don't catch each
  -- other's exceptions.
  tag <- newUnique
  size0 <- sizeEnv es0
  es <- unsafeConsEnv (IdE (Fail tag)) noRelinker es0
  r <- tryFailIO tag (release $ unEff m es) `onException` unsafeTailEnv size0 es
  _ <- unsafeTailEnv size0 es
  pure r

instance Fail :> es => MonadFail (Eff es) where
  fail msg = readerEffectM $ \(IdE (Fail tag)) -> unsafeEff_ $ do
    throwIO $ FailEx tag msg

--------------------

data FailEx = FailEx Unique String
instance Show FailEx where
  showsPrec p (FailEx _ msg)
    = ("Effectful.Internal.Monad.FailEx " ++)
    . showsPrec p msg
instance Exception FailEx

tryFailIO :: Unique -> IO a -> IO (Either String a)
tryFailIO tag m =
  (Right <$> m) `catch` \err@(FailEx etag msg) -> do
    if tag == etag
      then pure $ Left msg
      else throwIO err

----------------------------------------
-- IO

-- | Run arbitrary 'IO' operations via 'MonadIO', 'MonadBaseControl' 'IO' or
-- 'MonadUnliftIO'.
data IOE :: Effect where
  IOE :: IOE m r

-- | Run an 'Eff' operation with side effects.
--
-- For the pure version see 'runEff'.
runIOE :: Eff '[IOE] a -> IO a
runIOE m = unEff (evalEffect (IdE IOE) m) =<< emptyEnv

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
runEffect e0 m = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  bracket (unsafeConsEnv e0 noRelinker es0)
          (unsafeTailEnv size0)
          (\es -> (,) <$> unEff m es <*> getEnv es)

evalEffect :: i e -> Eff (e : es) a -> Eff es a
evalEffect e m = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  bracket (unsafeConsEnv e noRelinker es0)
          (unsafeTailEnv size0)
          (\es -> unEff m es)

execEffect :: i e -> Eff (e : es) a -> Eff es (i e)
execEffect e0 m = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  bracket (unsafeConsEnv e0 noRelinker es0)
          (unsafeTailEnv size0)
          (\es -> unEff m es *> getEnv es)

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
localEffect f m = unsafeEff $ \es -> do
  bracket (unsafeStateEnv (\e -> (e, f e)) es)
          (\e -> unsafePutEnv e es)
          (\_ -> unEff m es)
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
