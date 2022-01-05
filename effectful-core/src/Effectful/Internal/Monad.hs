{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | The 'Eff' monad.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Effectful.Internal.Monad
  ( -- * The 'Eff' monad
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

  -- * Prim
  , Prim
  , runPrim

  -- * Unlift strategies
  , UnliftStrategy(..)
  , Persistence(..)
  , Limit(..)
  , unliftStrategy
  , withUnliftStrategy
  , withEffToIO

  -- ** Low-level unlifts
  , seqUnliftIO
  , concUnliftIO

  -- * Effects

  , EffectStyle

  -- ** Dynamic dispatch
  , EffectHandler
  , LocalEnv(..)
  , DynamicEffect(..)
  , runHandler
  , send

  -- ** Static dispatch
  , StaticEffect(..)
  , runStatic
  , evalStatic
  , execStatic
  , getStatic
  , putStatic
  , stateStatic
  , stateStaticM
  , localStatic
  ) where

import Control.Applicative (liftA2)
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

type role Eff nominal representational

-- | The 'Eff' monad provides the implementation of a computation that performs
-- an arbitrary set of effects. In @'Eff' es a@, @es@ is a type-level list that
-- contains all the effects that the computation may perform. For example, a
-- computation that produces an 'Integer' by consuming a 'String' from the
-- global environment and acting upon a single mutable value of type 'Bool'
-- would have the following type:
--
-- @
-- 'Eff' '['Effectful.Reader.Reader' 'String', 'Effectful.State.Local.State' 'Bool'] 'Integer'
-- @
--
-- Normally, a concrete list of effects is not used to parameterize 'Eff'.
-- Instead, the '(:>)' type class is used to express constraints on the list of
-- effects without coupling a computation to a concrete list of effects. For
-- example, the above example would be expressed with the following type:
--
-- @
-- ('Effectful.Reader.Reader' 'String' ':>' es, 'Effectful.State.Local.State' 'Bool' ':>' es) => 'Eff' es 'Integer'
-- @
--
-- This abstraction allows the computation to be used in functions that may
-- perform other effects, and it also allows the effects to be handled in any
-- order.
newtype Eff (es :: [Effect]) a = Eff (Env es -> IO a)
  deriving (Monoid, Semigroup)

-- | Run a pure 'Eff' computation.
--
-- For running computations with side effects see 'runEff'.
runPureEff :: Eff '[] a -> a
runPureEff (Eff m) =
  -- unsafeDupablePerformIO is safe here since IOE was not on the stack, so no
  -- IO with side effects was performed (unless someone sneakily introduced side
  -- effects with unsafeEff, but then all bets are off).
  --
  -- Moreover, internals don't allocate any resources that require explicit
  -- cleanup actions to run.
  unsafeDupablePerformIO $ m =<< emptyEnv

----------------------------------------
-- Access to the internal representation

-- | Peel off the constructor of 'Eff'.
unEff :: Eff es a -> Env es -> IO a
unEff (Eff m) = m

-- | Access the underlying 'IO' monad along with the environment.
--
-- This function is __unsafe__ because it can be used to introduce arbitrary
-- 'IO' actions into pure 'Eff' computations.
unsafeEff :: (Env es -> IO a) -> Eff es a
unsafeEff m = Eff (oneShot m)

-- | Access the underlying 'IO' monad.
--
-- This function is __unsafe__ because it can be used to introduce arbitrary
-- 'IO' actions into pure 'Eff' computations.
unsafeEff_ :: IO a -> Eff es a
unsafeEff_ m = unsafeEff $ \_ -> m

----------------------------------------
-- Unlifting IO

-- | Get the current 'UnliftStrategy'.
unliftStrategy :: IOE :> es => Eff es UnliftStrategy
unliftStrategy = do
  StaticEffect (IOE unlift) <- getStatic
  pure unlift

-- | Locally override the 'UnliftStrategy' with the given value.
withUnliftStrategy :: IOE :> es => UnliftStrategy -> Eff es a -> Eff es a
withUnliftStrategy unlift = localStatic $ \_ -> StaticEffect (IOE unlift)

-- | Create an unlifting function with the current 'UnliftStrategy'.
--
-- This function is equivalent to 'withRunInIO', but has a 'HasCallStack'
-- constraint for accurate stack traces in case an insufficiently powerful
-- 'UnliftStrategy' is used and the unlifting function fails.
--
-- /Note:/ the strategy is reset to 'SeqUnlift' inside the continuation.
withEffToIO
  :: (HasCallStack, IOE :> es)
  => ((forall r. Eff es r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
withEffToIO f = unliftStrategy >>= \case
  SeqUnlift -> unsafeEff $ \es -> seqUnliftIO es f
  ConcUnlift p b -> withUnliftStrategy SeqUnlift $ do
    unsafeEff $ \es -> concUnliftIO es p b f

-- | Create an unlifting function with the 'SeqUnlift' strategy.
seqUnliftIO
  :: HasCallStack
  => Env es
  -- ^ The environment.
  -> ((forall r. Eff es r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> IO a
seqUnliftIO es k = seqUnlift k es unEff

-- | Create an unlifting function with the 'ConcUnlift' strategy.
concUnliftIO
  :: HasCallStack
  => Env es
  -- ^ The environment.
  -> Persistence
  -> Limit
  -> ((forall r. Eff es r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> IO a
concUnliftIO es persistence limit k = concUnlift persistence limit k es unEff

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

type instance EffectStyle Fail = DynamicEffect

instance Fail :> es => MonadFail (Eff es) where
  fail = send . Fail

----------------------------------------
-- IO

-- | Run arbitrary 'IO' computations via 'MonadIO' or 'MonadUnliftIO'.
newtype IOE :: Effect where
  IOE :: UnliftStrategy -> IOE m r

type instance EffectStyle IOE = StaticEffect

-- | Run an 'Eff' computation with side effects.
--
-- For running pure computations see 'runPureEff'.
runEff :: Eff '[IOE] a -> IO a
runEff m = unEff (evalStatic (IOE SeqUnlift) m) =<< emptyEnv

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

-- | Provide the ability to perform primitive state-transformer actions.
data Prim :: Effect where
  Prim :: Prim m r

type instance EffectStyle Prim = StaticEffect

-- | Run an 'Eff' computation with primitive state-transformer actions.
runPrim :: IOE :> es => Eff (Prim : es) a -> Eff es a
runPrim = evalStatic Prim

instance Prim :> es => PrimMonad (Eff es) where
  type PrimState (Eff es) = RealWorld
  primitive = unsafeEff_ . IO

----------------------------------------
-- Dynamic dispatch

type role LocalEnv nominal nominal

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
  -- ^ Capture of the local environment for handling local 'Eff' computations
  -- when @e@ is a higher order effect.
  -> e (Eff localEs) a
  -- ^ The effect performed in the local environment.
  -> Eff es a

-- | An effect implementation for dynamically dispatched effects.
--
-- Represents the effect handler bundled with its environment.
data DynamicEffect :: Effect -> Type where
  DynamicEffect :: !(Env es) -> !(EffectHandler e es) -> DynamicEffect e

-- | Run a dynamically dispatched effect with the given handler.
runHandler
  :: forall e es a. (EffectStyle e ~ DynamicEffect)
  => DynamicEffect e -> Eff (e : es) a -> Eff es a
runHandler e m = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  E.bracket (unsafeConsEnv e relinker es0)
            (unsafeTailEnv size0)
            (\es -> unEff m es)
  where
    relinker :: Relinker e
    relinker = Relinker $ \relink (DynamicEffect handlerEs handle) -> do
      newHandlerEs <- relink handlerEs
      pure $ DynamicEffect newHandlerEs handle

-- | Send an operation of the given effect to its handler for execution.
send
  :: forall e es a. (HasCallStack, e :> es, EffectStyle e ~ DynamicEffect)
  => e (Eff es) a -> Eff es a
send op = unsafeEff $ \es -> do
  DynamicEffect handlerEs handle <- getEnv @e es
  unEff (handle (LocalEnv es) op) handlerEs

----------------------------------------
-- Static dispatch

-- | An effect implementation for statically dispatched effects.
--
-- Represents an arbitrary data type with the appropriate number of phantom type
-- parameters.
newtype StaticEffect :: Effect -> Type where
  StaticEffect :: (forall m r. e m r) -> StaticEffect e

unStaticEffect :: StaticEffect a -> a m r
unStaticEffect (StaticEffect e) = e

-- | Run a statically dispatched effect with the given initial state and return
-- the final value along with the final state.
runStatic
  :: forall e es a m r. (EffectStyle e ~ StaticEffect)
  => (forall m' r'. e m' r') -> Eff (e : es) a -> Eff es (a, e m r)
runStatic e0 m = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  E.bracket (unsafeConsEnv (StaticEffect e0) noRelinker es0)
            (unsafeTailEnv size0)
            (\es -> (,) <$> unEff m es <*> (unStaticEffect <$> getEnv @e es))

-- | Run a statically dispatched effect with the given initial state and return
-- the final value, discarding the final state.
evalStatic
  :: EffectStyle e ~ StaticEffect
  => (forall m r. e m r) -> Eff (e : es) a -> Eff es a
evalStatic e m = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  E.bracket (unsafeConsEnv (StaticEffect e) noRelinker es0)
            (unsafeTailEnv size0)
            (\es -> unEff m es)

-- | Run a statically dispatched effect with the given initial state and return
-- the final state, discarding the final value.
execStatic
  :: forall e es a m r. (EffectStyle e ~ StaticEffect)
  => (forall m' r'. e m' r') -> Eff (e : es) a -> Eff es (e m r)
execStatic e0 m = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  E.bracket (unsafeConsEnv (StaticEffect e0) noRelinker es0)
            (unsafeTailEnv size0)
            (\es -> unEff m es *> (unStaticEffect <$> getEnv @e es))

-- | Fetch the current state of the effect.
getStatic
  :: forall e es. (e :> es, EffectStyle e ~ StaticEffect)
  => Eff es (StaticEffect e)
getStatic = unsafeEff $ \es -> getEnv @e es

-- | Set the current state of the effect to the given value.
putStatic
  :: forall e es. (e :> es, EffectStyle e ~ StaticEffect)
  => StaticEffect e -> Eff es ()
putStatic e = unsafeEff $ \es -> putEnv @e es e

-- | Apply the function to the current state of the effect and return a value.
stateStatic
  :: forall e es a. (e :> es, EffectStyle e ~ StaticEffect)
  => (StaticEffect e -> (a, StaticEffect e)) -> Eff es a
stateStatic f = unsafeEff $ \es -> stateEnv @e es f

-- | Apply the monadic function to the current state of the effect and return a
-- value.
stateStaticM
  :: forall e es a. (e :> es, EffectStyle e ~ StaticEffect)
  => (StaticEffect e -> Eff es (a, StaticEffect e)) -> Eff es a
stateStaticM f = unsafeEff $ \es -> E.mask $ \release -> do
  (a, e) <- (\e -> release $ unEff (f e) es) =<< getEnv @e es
  putEnv @e es e
  pure a

-- | Execute a computation with a temporarily modified state of the effect.
localStatic
  :: forall e es a. (e :> es, EffectStyle e ~ StaticEffect)
  => (StaticEffect e -> StaticEffect e) -> Eff es a -> Eff es a
localStatic f m = unsafeEff $ \es -> do
  E.bracket (stateEnv @e es $ \e -> (e, f e))
            (\e -> putEnv @e es e)
            (\_ -> unEff m es)
