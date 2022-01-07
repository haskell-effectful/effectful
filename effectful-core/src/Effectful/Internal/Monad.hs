{-# LANGUAGE TypeFamilyDependencies #-}
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

  -- * Dispatch
  , Dispatch(..)
  , DispatchOf
  , EffectRep

  -- ** Dynamic dispatch
  , EffectHandler
  , LocalEnv(..)
  , Handler(..)
  , runHandler
  , send

  -- ** Static dispatch
  , StaticRep
  , runStaticRep
  , evalStaticRep
  , execStaticRep
  , getStaticRep
  , putStaticRep
  , stateStaticRep
  , stateStaticRepM
  , localStaticRep

  -- *** Primitive operations
  , unsafeConsEnv
  , getEnv
  , putEnv
  , stateEnv
  , modifyEnv
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
  IOE unlift <- getStaticRep
  pure unlift

-- | Locally override the 'UnliftStrategy' with the given value.
withUnliftStrategy :: IOE :> es => UnliftStrategy -> Eff es a -> Eff es a
withUnliftStrategy unlift = localStaticRep $ \_ -> IOE unlift

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

type instance DispatchOf Fail = 'Dynamic

instance Fail :> es => MonadFail (Eff es) where
  fail = send . Fail

----------------------------------------
-- IO

-- | Run arbitrary 'IO' computations via 'MonadIO' or 'MonadUnliftIO'.
data IOE :: Effect

type instance DispatchOf IOE = 'Static
newtype instance StaticRep IOE = IOE UnliftStrategy

-- | Run an 'Eff' computation with side effects.
--
-- For running pure computations see 'runPureEff'.
runEff :: Eff '[IOE] a -> IO a
runEff m = unEff (evalStaticRep (IOE SeqUnlift) m) =<< emptyEnv

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
data Prim :: Effect

type instance DispatchOf Prim = 'Static
data instance StaticRep Prim = Prim

-- | Run an 'Eff' computation with primitive state-transformer actions.
runPrim :: IOE :> es => Eff (Prim : es) a -> Eff es a
runPrim = evalStaticRep Prim

instance Prim :> es => PrimMonad (Eff es) where
  type PrimState (Eff es) = RealWorld
  primitive = unsafeEff_ . IO

----------------------------------------
-- Dispatch

-- | A type of dispatch. For more information consult the documentation in
-- "Effectful.Dispatch.Dynamic" and "Effectful.Dispatch.Static".
data Dispatch = Dynamic | Static

-- | Dispatch types of effects.
type family DispatchOf (e :: Effect) :: Dispatch

-- | Internal representations of effects.
type family EffectRep (d :: Dispatch) = (r :: Effect -> Type) | r -> d where
  EffectRep 'Dynamic = Handler
  EffectRep 'Static  = StaticRep

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

-- | An internal representation of dynamically dispatched effects, i.e. the
-- effect handler bundled with its environment.
data Handler :: Effect -> Type where
  Handler :: !(Env es) -> !(EffectHandler e es) -> Handler e

-- | Run a dynamically dispatched effect with the given handler.
runHandler :: DispatchOf e ~ 'Dynamic => Handler e -> Eff (e : es) a -> Eff es a
runHandler e m = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  E.bracket (unsafeConsEnv e relinker es0)
            (unsafeTailEnv size0)
            (\es -> unEff m es)
  where
    relinker :: Relinker Handler e
    relinker = Relinker $ \relink (Handler handlerEs handle) -> do
      newHandlerEs <- relink handlerEs
      pure $ Handler newHandlerEs handle

-- | Send an operation of the given effect to its handler for execution.
send :: (HasCallStack, DispatchOf e ~ 'Dynamic, e :> es) => e (Eff es) a -> Eff es a
send op = unsafeEff $ \es -> do
  Handler handlerEs handle <- getEnv es
  unEff (handle (LocalEnv es) op) handlerEs

----------------------------------------
-- Static dispatch

-- | Internal representations of statically dispatched effects.
data family StaticRep (e :: Effect) :: Type

-- | Run a statically dispatched effect with the given initial representation
-- and return the final value along with the final representation.
runStaticRep
  :: DispatchOf e ~ 'Static
  => StaticRep e -- ^ The initial representation.
  -> Eff (e : es) a
  -> Eff es (a, StaticRep e)
runStaticRep e0 m = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  E.bracket (unsafeConsEnv e0 noRelinker es0)
            (unsafeTailEnv size0)
            (\es -> (,) <$> unEff m es <*> getEnv es)

-- | Run a statically dispatched effect with the given initial representation
-- and return the final value, discarding the final representation.
evalStaticRep
  :: DispatchOf e ~ 'Static
  => StaticRep e -- ^ The initial representation.
  -> Eff (e : es) a
  -> Eff es a
evalStaticRep e m = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  E.bracket (unsafeConsEnv e noRelinker es0)
            (unsafeTailEnv size0)
            (\es -> unEff m es)

-- | Run a statically dispatched effect with the given initial representation
-- and return the final representation, discarding the final value.
execStaticRep
  :: DispatchOf e ~ 'Static
  => StaticRep e -- ^ The initial representation.
  -> Eff (e : es) a
  -> Eff es (StaticRep e)
execStaticRep e0 m = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  E.bracket (unsafeConsEnv e0 noRelinker es0)
            (unsafeTailEnv size0)
            (\es -> unEff m es *> getEnv es)

-- | Fetch the current representation of the effect.
getStaticRep :: (DispatchOf e ~ 'Static, e :> es) => Eff es (StaticRep e)
getStaticRep = unsafeEff $ \es -> getEnv es

-- | Set the current representation of the effect to the given value.
putStaticRep :: (DispatchOf e ~ 'Static, e :> es) => StaticRep e -> Eff es ()
putStaticRep s = unsafeEff $ \es -> putEnv es s

-- | Apply the function to the current representation of the effect and return a
-- value.
stateStaticRep
  :: (DispatchOf e ~ 'Static, e :> es)
  => (StaticRep e -> (a, StaticRep e))
  -- ^ The function to modify the representation.
  -> Eff es a
stateStaticRep f = unsafeEff $ \es -> stateEnv es f

-- | Apply the monadic function to the current representation of the effect and
-- return a value.
stateStaticRepM
  :: (DispatchOf e ~ 'Static, e :> es)
  => (StaticRep e -> Eff es (a, StaticRep e))
  -- ^ The function to modify the representation.
  -> Eff es a
stateStaticRepM f = unsafeEff $ \es -> E.mask $ \release -> do
  (a, s) <- (\s0 -> release $ unEff (f s0) es) =<< getEnv es
  putEnv es s
  pure a

-- | Execute a computation with a temporarily modified representation of the
-- effect.
localStaticRep
  :: (DispatchOf e ~ 'Static, e :> es)
  => (StaticRep e -> StaticRep e)
  -- ^ The function to temporarily modify the representation.
  -> Eff es a
  -> Eff es a
localStaticRep f m = unsafeEff $ \es -> do
  E.bracket (stateEnv es $ \s -> (s, f s))
            (\s -> putEnv es s)
            (\_ -> unEff m es)

----------------------------------------
-- Safer interface for Env

-- | Extend the environment with a new effect (in place).
--
-- This function is __highly unsafe__ because it renders the input 'Env'
-- unusable until the corresponding 'unsafeTailEnv' call is made, but it's not
-- checked anywhere.
unsafeConsEnv
  :: EffectRep (DispatchOf e) e
  -- ^ The representation of the effect.
  -> Relinker (EffectRep (DispatchOf e)) e
  -> Env es
  -> IO (Env (e : es))
unsafeConsEnv = veryUnsafeConsEnv

-- | Extract a specific representation of the effect from the environment.
getEnv :: e :> es => Env es -> IO (EffectRep (DispatchOf e) e)
getEnv = unsafeGetEnv

-- | Replace the representation of the effect in the environment with a new
-- value (in place).
putEnv :: e :> es => Env es -> EffectRep (DispatchOf e) e -> IO ()
putEnv = unsafePutEnv

-- | Modify the representation of the effect in the environment (in place) and
-- return a value.
stateEnv
  :: e :> es
  => Env es
  -> (EffectRep (DispatchOf e) e -> (a, EffectRep (DispatchOf e) e))
  -- ^ The function to modify the representation.
  -> IO a
stateEnv = unsafeStateEnv

-- | Modify the representation of the effect in the environment (in place).
modifyEnv
  :: e :> es
  => Env es
  -> (EffectRep (DispatchOf e) e -> EffectRep (DispatchOf e) e)
  -- ^ The function to modify the representation.
  -> IO ()
modifyEnv = unsafeModifyEnv
