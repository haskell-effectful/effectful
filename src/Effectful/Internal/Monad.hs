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
  , runPureEff

  -- ** Access to the internal representation
  , unEff
  , unsafeEff
  , unsafeEff_

  -- * Fail
  , Fail
  , runFail

  -- * IO
  , IOE
  , runEff

  -- * Primitive
  , PrimE
  , runPrimE

  -- ** Unlift strategies
  , UnliftStrategy(..)
  , Persistence(..)
  , Limit(..)
  , unliftStrategy
  , withUnliftStrategy

  --- *** Low-level helpers
  , unsafeWithLiftSeqOp
  , unsafeSeqUnliftEff
  , unsafeConcUnliftEff

  -- * Primitive effects

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
import Control.Monad.Primitive
import Control.Monad.Trans.Control
import Data.Unique
import GHC.Exts (oneShot)
import GHC.IO (IO(..))
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Control.Monad.Catch as E

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Unlift
import Effectful.Internal.Utils

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
-- Instead, the '(:>)' type class is used to express constraints on the list of
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
  deriving (Monoid, Semigroup)

-- | Run a pure 'Eff' operation.
--
-- For running operations with side effects see 'runEff'.
runPureEff :: Eff '[] a -> a
runPureEff (Eff m) =
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

-- | Access the underlying 'IO' monad along with the environment.
unsafeEff :: (Env es -> IO a) -> Eff es a
unsafeEff m = Eff (oneShot m)

-- | Access the underlying 'IO' monad.
unsafeEff_ :: IO a -> Eff es a
unsafeEff_ m = unsafeEff $ \_ -> m

-- | Utility for lifting 'IO' operations of type
--
-- @'IO' a -> 'IO' b@
--
-- to
--
-- @forall localEs. 'Eff' localEs a -> 'Eff' localEs b@
--
-- /Note:/ the 'IO' operation must not run its argument in a separate thread,
-- attempting to do so will result in a runtime error.
unsafeWithLiftSeqOp
  :: ((forall a b localEs. (IO a -> IO b) -> Eff localEs a -> Eff localEs b) -> Eff es r)
  -> Eff es r
unsafeWithLiftSeqOp k = k $ \mapIO m -> unsafeEff $ \es -> do
  unsafeSeqUnliftEff es $ \unlift -> mapIO $ unlift m

-- | Lower 'Eff' operations into 'IO' ('SeqUnlift').
unsafeSeqUnliftEff :: Env es -> ((forall r. Eff es r -> IO r) -> IO a) -> IO a
unsafeSeqUnliftEff es k = do
  tid0 <- myThreadId
  k $ \m -> do
    tid <- myThreadId
    if tid `eqThreadId` tid0
      then unEff m es
      else error
         $ "If you want to use the unlifting function to run Eff operations "
        ++ "in multiple threads, have a look at UnliftStrategy (ConcUnlift)."

-- | Lower 'Eff' operations into 'IO' ('ConcUnlift').
unsafeConcUnliftEff
  :: Env es
  -> Persistence
  -> Limit
  -> ((forall r. Eff es r -> IO r) -> IO a)
  -> IO a
unsafeConcUnliftEff es Ephemeral (Limited uses) k =
  unsafeEphemeralConcUnliftIO uses k es unEff
unsafeConcUnliftEff es Ephemeral Unlimited k =
  unsafeEphemeralConcUnliftIO maxBound k es unEff
unsafeConcUnliftEff es Persistent (Limited threads) k =
  unsafePersistentConcUnliftIO False threads k es unEff
unsafeConcUnliftEff es Persistent Unlimited k =
  unsafePersistentConcUnliftIO True maxBound k es unEff

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

-- | Run arbitrary 'IO' operations via 'MonadIO', 'MonadUnliftIO' or
-- 'MonadBaseControl' 'IO'.
newtype IOE :: Effect where
  IOE :: UnliftStrategy -> IOE m r

-- | Run an 'Eff' operation with side effects.
--
-- For running pure operations see 'runPureEff'.
runEff :: Eff '[IOE] a -> IO a
runEff m = unEff (evalEffect (IdE (IOE SeqUnlift)) m) =<< emptyEnv

instance IOE :> es => MonadIO (Eff es) where
  liftIO = unsafeEff_

instance IOE :> es => MonadUnliftIO (Eff es) where
  withRunInIO = unliftEff

-- | Instance included for compatibility with existing code, usage of 'liftIO'
-- is preferrable.
instance IOE :> es => MonadBase IO (Eff es) where
  liftBase = unsafeEff_

-- | Instance included for compatibility with existing code, usage of
-- 'withRunInIO' is preferrable.
instance IOE :> es => MonadBaseControl IO (Eff es) where
  type StM (Eff es) a = a
  liftBaseWith = unliftEff
  restoreM = pure

----------------------------------------
-- Primitive

-- | An effect to perform primitive state-transformer actions.
data PrimE :: Effect where
  PrimE :: PrimE m r

-- | Run an 'Eff' operation with primitive state-transformer actions.
runPrimE :: IOE :> es => Eff (PrimE : es) a -> Eff es a
runPrimE = evalEffect (IdE PrimE)

instance PrimE :> es => PrimMonad (Eff es) where
  type PrimState (Eff es) = PrimState IO
  primitive = unsafeEff_ . IO

----------------------------------------
-- Unlift strategies

-- | The strategy to use when unlifting 'Eff' operations via 'withRunInIO',
-- 'liftBaseWith' or the 'Effectful.Interpreter.localUnlift' family.
data UnliftStrategy
  = SeqUnlift
  -- ^ The fastest strategy and a default setting for 'IOE'. An attempt to call
  -- the unlifting function in threads distinct from its creator will result in
  -- a runtime error.
  | ConcUnlift Persistence Limit
  -- ^ A strategy that makes it possible for the unlifting function to be called
  -- in threads distinct from its creator. See 'Persistence' and 'Limit'
  -- settings for more information.
  deriving (Eq, Ord, Show)

-- | Persistence setting for the 'ConcUnlift' strategy.
--
-- Different functions require different persistence strategies. Examples:
--
-- - Lifting 'pooledMapConcurrentlyN' from the @unliftio@ library requires the
--   'Ephemeral' strategy as we don't want jobs to share environment changes
--   made by previous jobs run in the same worker thread.
--
-- - Lifting 'Control.Concurrent.forkIOWithUnmask' requires the 'Persistent'
--   strategy, otherwise the unmasking function would start with a fresh
--   environment each time it's called.
data Persistence
  = Ephemeral
  -- ^ Don't persist the environment between calls to the unlifting function in
  -- threads distinct from its creator.
  | Persistent
  -- ^ Persist the environment between calls to the unlifting function within a
  -- particular thread.
  deriving (Eq, Ord, Show)

-- | Limit setting for the 'ConcUnlift' strategy.
data Limit
  = Limited Int
  -- ^ Behavior dependent on the 'Persistence' setting.
  --
  -- For 'Ephemeral', it limits the amount of uses of the unlifting function in
  -- threads distinct from its creator to @N@. The unlifting function will
  -- create @N@ copies of the environment when called @N@ times and @K+1@ copies
  -- when called @K < N@ times.
  --
  -- For 'Persistent', it limits the amount of threads, distinct from the
  -- creator of the unlifting function, it can be called in to @N@. The amount
  -- of calls to the unlifting function within a particular threads is
  -- unlimited. The unlifting function will create @N@ copies of the environment
  -- when called in @N@ threads and @K+1@ copies when called in @K < N@ threads.
  | Unlimited
  -- ^ Unlimited use of the unlifting function.
  deriving (Eq, Ord, Show)

-- | Get the current 'UnliftStrategy'.
unliftStrategy :: IOE :> es => Eff es UnliftStrategy
unliftStrategy = readerEffectM $ \(IdE (IOE unlift)) -> pure unlift

-- | Locally override the 'UnliftStrategy' with a given value.
--
-- /Note:/ the strategy is always reset to 'SeqUnlift' for new threads.
withUnliftStrategy :: IOE :> es => UnliftStrategy -> Eff es a -> Eff es a
withUnliftStrategy unlift = localEffect $ \_ -> IdE (IOE unlift)

-- | Helper for 'MonadBaseControl' and 'MonadUnliftIO' instances.
unliftEff :: IOE :> es => ((forall r. Eff es r -> IO r) -> IO a) -> Eff es a
unliftEff f = unliftStrategy >>= \case
  SeqUnlift -> unsafeEff $ \es -> unsafeSeqUnliftEff es f
  ConcUnlift p b -> withUnliftStrategy SeqUnlift $ do
    unsafeEff $ \es -> unsafeConcUnliftEff es p b f

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

putEffect :: e :> es => i e -> Eff es ()
putEffect e = unsafeEff $ \es -> unsafePutEnv e es

stateEffect :: e :> es => (i e -> (a, i e)) -> Eff es a
stateEffect f = unsafeEff $ \es -> unsafeStateEnv f es

localEffect :: e :> es => (i e -> i e) -> Eff es a -> Eff es a
localEffect f m = unsafeEff $ \es -> do
  bracket (unsafeStateEnv (\e -> (e, f e)) es)
          (\e -> unsafePutEnv e es)
          (\_ -> unEff m es)

readerEffectM :: e :> es => (i e -> Eff es a) -> Eff es a
readerEffectM f = unsafeEff $ \es -> getEnv es >>= \e -> unEff (f e) es

stateEffectM :: e :> es => (i e -> Eff es (a, i e)) -> Eff es a
stateEffectM f = unsafeEff $ \es -> mask $ \release -> do
  (a, e) <- (\e -> release $ unEff (f e) es) =<< getEnv es
  unsafePutEnv e es
  pure a
