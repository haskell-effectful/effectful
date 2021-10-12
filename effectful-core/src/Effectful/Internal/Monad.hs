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
  , Fail(..)

  -- * IO
  , IOE
  , runEff

  -- * Primitive
  , Prim
  , runPrim

  -- ** Unlift strategies
  , UnliftStrategy(..)
  , Persistence(..)
  , Limit(..)
  , unliftStrategy
  , withUnliftStrategy
  , withEffToIO

  --- *** Low-level helpers
  , unsafeLiftMapIO
  , seqUnliftEff
  , concUnliftEff

  -- * Effects

  -- ** Handler
  , EffectHandler
  , LocalEnv(..)
  , Handler(..)
  , runHandler

  -- *** Sending operations
  , send

  -- ** Primitive

  -- *** Running
  , runEffect
  , evalEffect
  , execEffect

  -- *** Modification
  , getEffect
  , putEffect
  , stateEffect
  , stateEffectM
  , localEffect
  ) where

import Control.Applicative (liftA2)
import Control.Concurrent (myThreadId)
import Control.Monad.Base
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Primitive
import Control.Monad.Trans.Control
import GHC.Exts (oneShot)
import GHC.IO (IO(..))
import GHC.Stack (HasCallStack)
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Control.Exception as E
import qualified Control.Monad.Catch as C

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
--
-- This function is __unsafe__ because it can be used to introduce arbitrary
-- 'IO' actions into pure 'Eff' operations.
unsafeEff :: (Env es -> IO a) -> Eff es a
unsafeEff m = Eff (oneShot m)

-- | Access the underlying 'IO' monad.
--
-- This function is __unsafe__ because it can be used to introduce arbitrary
-- 'IO' actions into pure 'Eff' operations.
unsafeEff_ :: IO a -> Eff es a
unsafeEff_ m = unsafeEff $ \_ -> m

-- | Utility for lifting 'IO' operations of type
--
-- @'IO' a -> 'IO' b@
--
-- to
--
-- @'Eff' es a -> 'Eff' es b@
--
-- This function is __unsafe__ because:
--
-- - It can be used to introduce arbitrary 'IO' actions into pure 'Eff'
--   operations.
--
-- - The 'IO' operation must not run its argument in a separate thread, but it's
--   not checked anywhere.
unsafeLiftMapIO :: (IO a -> IO b) -> Eff es a -> Eff es b
unsafeLiftMapIO f m = unsafeEff $ \es -> f (unEff m es)

-- | Lower 'Eff' operations into 'IO' ('SeqUnlift').
seqUnliftEff
  :: HasCallStack
  => Env es
  -> ((forall r. Eff es r -> IO r) -> IO a)
  -> IO a
seqUnliftEff es k = do
  tid0 <- myThreadId
  k $ \m -> do
    tid <- myThreadId
    if tid `eqThreadId` tid0
      then unEff m es
      else error
         $ "If you want to use the unlifting function to run Eff operations "
        ++ "in multiple threads, have a look at UnliftStrategy (ConcUnlift)."

-- | Lower 'Eff' operations into 'IO' ('ConcUnlift').
concUnliftEff
  :: HasCallStack
  => Env es
  -> Persistence
  -> Limit
  -> ((forall r. Eff es r -> IO r) -> IO a)
  -> IO a
concUnliftEff es Ephemeral (Limited uses) k =
  ephemeralConcUnliftIO uses k es unEff
concUnliftEff es Ephemeral Unlimited k =
  ephemeralConcUnliftIO maxBound k es unEff
concUnliftEff es Persistent (Limited threads) k =
  persistentConcUnliftIO False threads k es unEff
concUnliftEff es Persistent Unlimited k =
  persistentConcUnliftIO True maxBound k es unEff

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

instance C.MonadThrow (Eff es) where
  throwM = unsafeEff_ . E.throwIO

instance C.MonadCatch (Eff es) where
  catch m handler = unsafeEff $ \es -> do
    size <- sizeEnv es
    unEff m es `E.catch` \e -> do
      checkSizeEnv size es
      unEff (handler e) es

instance C.MonadMask (Eff es) where
  mask k = unsafeEff $ \es -> E.mask $ \restore ->
    unEff (k $ \m -> unsafeEff $ restore . unEff m) es

  uninterruptibleMask k = unsafeEff $ \es -> E.uninterruptibleMask $ \restore ->
    unEff (k $ \m -> unsafeEff $ restore . unEff m) es

  generalBracket acquire release use = unsafeEff $ \es -> E.mask $ \restore -> do
    size <- sizeEnv es
    resource <- unEff acquire es
    b <- restore (unEff (use resource) es) `E.catch` \e -> do
      checkSizeEnv size es
      _ <- unEff (release resource $ C.ExitCaseException e) es
      E.throwIO e
    checkSizeEnv size es
    c <- unEff (release resource $ C.ExitCaseSuccess b) es
    pure (b, c)

----------------------------------------
-- Fail

-- | Provide the ability to use the 'MonadFail' instance of 'Eff'.
data Fail :: Effect where
  Fail :: String -> Fail m a

instance Fail :> es => MonadFail (Eff es) where
  fail = send . Fail

----------------------------------------
-- IO

-- | Run arbitrary 'IO' operations via 'MonadIO' or 'MonadUnliftIO'.
newtype IOE :: Effect where
  IOE :: UnliftStrategy -> IOE m r

-- | Run an 'Eff' operation with side effects.
--
-- For running pure operations see 'runPureEff'.
runEff :: Eff '[IOE] a -> IO a
runEff m = unEff (evalEffect (IdE (IOE SeqUnlift)) m) =<< emptyEnv

instance IOE :> es => MonadIO (Eff es) where
  liftIO = unsafeEff_

-- | Use 'withEffToIO' if you want accurate stack traces on errors.
instance IOE :> es => MonadUnliftIO (Eff es) where
  withRunInIO = withEffToIO

-- | Instance included for compatibility with existing code, usage of 'liftIO'
-- is preferrable.
instance IOE :> es => MonadBase IO (Eff es) where
  liftBase = unsafeEff_

-- | Instance included for compatibility with existing code, usage of
-- 'withRunInIO' is preferrable.
instance IOE :> es => MonadBaseControl IO (Eff es) where
  type StM (Eff es) a = a
  liftBaseWith = withEffToIO
  restoreM = pure

----------------------------------------
-- Primitive

-- | An effect to perform primitive state-transformer actions.
data Prim :: Effect where
  Prim :: Prim m r

-- | Run an 'Eff' operation with primitive state-transformer actions.
runPrim :: IOE :> es => Eff (Prim : es) a -> Eff es a
runPrim = evalEffect (IdE Prim)

instance Prim :> es => PrimMonad (Eff es) where
  type PrimState (Eff es) = RealWorld
  primitive = unsafeEff_ . IO

----------------------------------------
-- Unlift strategies

-- | The strategy to use when unlifting 'Eff' operations via 'withRunInIO' or
-- the 'Effectful.Interpreter.localUnlift' family.
data UnliftStrategy
  = SeqUnlift
  -- ^ The fastest strategy and a default setting for 'IOE'. An attempt to call
  -- the unlifting function in threads distinct from its creator will result in
  -- a runtime error.
  | ConcUnlift !Persistence !Limit
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
  = Limited !Int
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
unliftStrategy = do
  IdE (IOE unlift) <- getEffect
  pure unlift

-- | Locally override the 'UnliftStrategy' with the given value.
--
-- /Note:/ the strategy is reset to 'SeqUnlift' for new threads.
withUnliftStrategy :: IOE :> es => UnliftStrategy -> Eff es a -> Eff es a
withUnliftStrategy unlift = localEffect $ \_ -> IdE (IOE unlift)

-- | Create an unlifting function with the current 'UnliftStrategy'.
--
-- This function is equivalent to 'withRunInIO', but has a 'HasCallStack'
-- constraint for accurate stack traces in case an insufficiently powerful
-- 'UnliftStrategy' is used and the unlifting function fails.
withEffToIO
  :: (HasCallStack, IOE :> es)
  => ((forall r. Eff es r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
withEffToIO f = unliftStrategy >>= \case
  SeqUnlift -> unsafeEff $ \es -> seqUnliftEff es f
  ConcUnlift p b -> withUnliftStrategy SeqUnlift $ do
    unsafeEff $ \es -> concUnliftEff es p b f

----------------------------------------
-- Handler

-- | Opaque representation of the 'Eff' environment at the point of calling the
-- 'send' function, i.e. right before the control is passed to the effect
-- handler.
--
-- The second type variable represents effects of a handler and is needed for
-- technical reasons to guarantee soundness.
newtype LocalEnv (localEs :: [Effect]) (handlerEs :: [Effect]) = LocalEnv (Env localEs)

-- | Type signature of the effect handler.
type EffectHandler e es
  = forall a localEs. HasCallStack
  => LocalEnv localEs es
  -- ^ Capture of the local environment for handling local 'Eff' operations when
  -- @e@ is a higher order effect.
  -> e (Eff localEs) a
  -- ^ The effect performed in the local environment.
  -> Eff es a

-- | An effect handler bundled along with its environment.
data Handler :: Effect -> Type where
  Handler :: !(Env es) -> !(EffectHandler e es) -> Handler e

-- | Run the effect with the given handler.
runHandler :: Handler e -> Eff (e : es) a -> Eff es a
runHandler e m = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  E.bracket (unsafeConsEnv e relinker es0)
            (unsafeTailEnv size0)
            (\es -> unEff m es)
  where
    relinker :: Relinker Handler e
    relinker = Relinker $ \relink (Handler env handle) -> do
      newEnv <- relink env
      pure $ Handler newEnv handle

-- | Send an operation of the given effect to its handler for execution.
send :: (HasCallStack, e :> es) => e (Eff es) a -> Eff es a
send op = unsafeEff $ \es -> do
  Handler env handle <- getEnv es
  unEff (handle (LocalEnv es) op) env

----------------------------------------
-- Helpers

runEffect :: handler e -> Eff (e : es) a -> Eff es (a, handler e)
runEffect e0 m = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  E.bracket (unsafeConsEnv e0 noRelinker es0)
            (unsafeTailEnv size0)
            (\es -> (,) <$> unEff m es <*> getEnv es)

evalEffect :: handler e -> Eff (e : es) a -> Eff es a
evalEffect e m = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  E.bracket (unsafeConsEnv e noRelinker es0)
            (unsafeTailEnv size0)
            (\es -> unEff m es)

execEffect :: handler e -> Eff (e : es) a -> Eff es (handler e)
execEffect e0 m = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  E.bracket (unsafeConsEnv e0 noRelinker es0)
            (unsafeTailEnv size0)
            (\es -> unEff m es *> getEnv es)

getEffect :: e :> es => Eff es (handler e)
getEffect = unsafeEff $ \es -> getEnv es

putEffect :: e :> es => handler e -> Eff es ()
putEffect e = unsafeEff $ \es -> putEnv es e

stateEffect :: e :> es => (handler e -> (a, handler e)) -> Eff es a
stateEffect f = unsafeEff $ \es -> stateEnv es f

stateEffectM :: e :> es => (handler e -> Eff es (a, handler e)) -> Eff es a
stateEffectM f = unsafeEff $ \es -> E.mask $ \release -> do
  (a, e) <- (\e -> release $ unEff (f e) es) =<< getEnv es
  putEnv es e
  pure a

localEffect :: e :> es => (handler e -> handler e) -> Eff es a -> Eff es a
localEffect f m = unsafeEff $ \es -> do
  E.bracket (stateEnv es $ \e -> (e, f e))
            (\e -> putEnv es e)
            (\_ -> unEff m es)
