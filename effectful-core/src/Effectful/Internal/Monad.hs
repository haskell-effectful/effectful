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

  -- * Lifting
  , raise
  , subsume
  , inject

  -- * Unlifting
  , UnliftStrategy(..)
  , Persistence(..)
  , Limit(..)
  , unliftStrategy
  , withUnliftStrategy
  , withEffToIO

  -- ** Low-level unlifts
  , seqUnliftIO
  , concUnliftIO

  -- * Dispatch

  -- ** Dynamic dispatch
  , EffectHandler
  , LocalEnv(..)
  , Handler(..)
  , relinkHandler
  , runHandler
  , send

  -- ** Static dispatch
  , StaticRep
  , MaybeIOE
  , runStaticRep
  , evalStaticRep
  , execStaticRep
  , getStaticRep
  , putStaticRep
  , stateStaticRep
  , stateStaticRepM
  , localStaticRep

  -- *** Primitive operations
  , consEnv
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
import Data.Kind (Constraint)
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
-- ('Effectful.Reader.Static.Reader' 'String' ':>' es, 'Effectful.State.Static.Local.State' 'Bool' ':>' es) => 'Eff' es 'Integer'
-- @
--
-- Abstracting over the list of effects with '(:>)':
--
-- - Allows the computation to be used in functions that may perform other
-- effects.
--
-- - Allows the effects to be handled in any order.
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
unEff = \(Eff m) -> m

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
-- This function is equivalent to 'Effectful.withRunInIO', but has a
-- 'HasCallStack' constraint for accurate stack traces in case an insufficiently
-- powerful 'UnliftStrategy' is used and the unlifting function fails.
withEffToIO
  :: (HasCallStack, IOE :> es)
  => ((forall r. Eff es r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  --
  -- /Note:/ the strategy is reset to 'SeqUnlift' inside the continuation.
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
    unEff m es `E.catch` \e -> do
      checkSizeEnv es
      unEff (handler e) es

instance C.MonadMask (Eff es) where
  mask k = unsafeEff $ \es -> E.mask $ \unmask ->
    unEff (k $ \m -> unsafeEff $ unmask . unEff m) es

  uninterruptibleMask k = unsafeEff $ \es -> E.uninterruptibleMask $ \unmask ->
    unEff (k $ \m -> unsafeEff $ unmask . unEff m) es

  generalBracket acquire release use = unsafeEff $ \es -> E.mask $ \unmask -> do
    resource <- unEff acquire es
    b <- unmask (unEff (use resource) es) `E.catch` \e -> do
      checkSizeEnv es
      _ <- unEff (release resource $ C.ExitCaseException e) es
      E.throwIO e
    c <- unEff (release resource $ C.ExitCaseSuccess b) es
    pure (b, c)

----------------------------------------
-- Fail

-- | Provide the ability to use the 'MonadFail' instance of 'Eff'.
data Fail :: Effect where
  Fail :: String -> Fail m a

type instance DispatchOf Fail = Dynamic

instance Fail :> es => MonadFail (Eff es) where
  fail = send . Fail

----------------------------------------
-- IO

-- | Run arbitrary 'IO' computations via 'MonadIO' or 'MonadUnliftIO'.
--
-- /Note:/ it is not recommended to use this effect in application code as it is
-- too liberal. Ideally, this is only used in handlers of more fine-grained
-- effects.
data IOE :: Effect

type instance DispatchOf IOE = Static WithSideEffects
newtype instance StaticRep IOE = IOE UnliftStrategy

-- | Run an 'Eff' computation with side effects.
--
-- For running pure computations see 'runPureEff'.
runEff :: Eff '[IOE] a -> IO a
runEff m = unEff m =<< consEnv (IOE SeqUnlift) dummyRelinker =<< emptyEnv

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
-- 'Effectful.withRunInIO' is preferrable.
instance IOE :> es => MonadBaseControl IO (Eff es) where
  type StM (Eff es) a = a
  liftBaseWith = withEffToIO
  restoreM = pure

----------------------------------------
-- Primitive

-- | Provide the ability to perform primitive state-transformer actions.
data Prim :: Effect

type instance DispatchOf Prim = Static WithSideEffects
data instance StaticRep Prim = Prim

-- | Run an 'Eff' computation with primitive state-transformer actions.
runPrim :: IOE :> es => Eff (Prim : es) a -> Eff es a
runPrim = evalStaticRep Prim

instance Prim :> es => PrimMonad (Eff es) where
  type PrimState (Eff es) = RealWorld
  primitive = unsafeEff_ . IO

----------------------------------------
-- Lifting

-- | Lift an 'Eff' computation into an effect stack with one more effect.
raise :: Eff es a -> Eff (e : es) a
raise m = unsafeEff $ \es -> unEff m =<< tailEnv es

-- | Eliminate a duplicate effect from the top of the effect stack.
subsume :: e :> es => Eff (e : es) a -> Eff es a
subsume m = unsafeEff $ \es0 -> do
  E.bracket (subsumeEnv es0)
            unsubsumeEnv
            (\es -> unEff m es)

-- | Allow for running an effect stack @xs@ within @es@ as long as @xs@ is a
-- permutation (with possible duplicates) of a subset of @es@.
--
-- Generalizes 'raise' and 'subsume'.
--
-- /Note:/ this function should be needed rarely, usually when you have to cross
-- API boundaries and monomorphic effect stacks are involved. Using monomorphic
-- stacks is discouraged (see 'Eff'), but sometimes might be necessary due to
-- external constraints.
inject :: Subset xs es => Eff xs a -> Eff es a
inject m = unsafeEff $ \es -> unEff m =<< injectEnv es

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
type instance EffectRep Dynamic = Handler

relinkHandler :: Relinker Handler e
relinkHandler = Relinker $ \relink (Handler handlerEs handle) -> do
  newHandlerEs <- relink handlerEs
  pure $ Handler newHandlerEs handle

-- | Run a dynamically dispatched effect with the given handler.
runHandler :: DispatchOf e ~ Dynamic => Handler e -> Eff (e : es) a -> Eff es a
runHandler e m = unsafeEff $ \es0 -> do
  E.bracket (consEnv e relinkHandler es0)
            unconsEnv
            (\es -> unEff m es)

-- | Send an operation of the given effect to its handler for execution.
send
  :: (HasCallStack, DispatchOf e ~ Dynamic, e :> es)
  => e (Eff es) a
  -- ^ The effect.
  -> Eff es a
send op = unsafeEff $ \es -> do
  Handler handlerEs handle <- getEnv es
  unEff (handle (LocalEnv es) op) handlerEs
{-# NOINLINE send #-}

----------------------------------------
-- Static dispatch

-- | Require the 'IOE' effect for running statically dispatched effects whose
-- operations perform side effects.
type family MaybeIOE (sideEffects :: SideEffects) (es :: [Effect]) :: Constraint where
  MaybeIOE NoSideEffects   _  = ()
  MaybeIOE WithSideEffects es = IOE :> es

-- | Internal representations of statically dispatched effects.
data family StaticRep (e :: Effect) :: Type
type instance EffectRep (Static sideEffects) = StaticRep

-- | Run a statically dispatched effect with the given initial representation
-- and return the final value along with the final representation.
runStaticRep
  :: (DispatchOf e ~ Static sideEffects, MaybeIOE sideEffects es)
  => StaticRep e -- ^ The initial representation.
  -> Eff (e : es) a
  -> Eff es (a, StaticRep e)
runStaticRep e0 m = unsafeEff $ \es0 -> do
  E.bracket (consEnv e0 dummyRelinker es0)
            unconsEnv
            (\es -> (,) <$> unEff m es <*> getEnv es)

-- | Run a statically dispatched effect with the given initial representation
-- and return the final value, discarding the final representation.
evalStaticRep
  :: (DispatchOf e ~ Static sideEffects, MaybeIOE sideEffects es)
  => StaticRep e -- ^ The initial representation.
  -> Eff (e : es) a
  -> Eff es a
evalStaticRep e m = unsafeEff $ \es0 -> do
  E.bracket (consEnv e dummyRelinker es0)
            unconsEnv
            (\es -> unEff m es)

-- | Run a statically dispatched effect with the given initial representation
-- and return the final representation, discarding the final value.
execStaticRep
  :: (DispatchOf e ~ Static sideEffects, MaybeIOE sideEffects es)
  => StaticRep e -- ^ The initial representation.
  -> Eff (e : es) a
  -> Eff es (StaticRep e)
execStaticRep e0 m = unsafeEff $ \es0 -> do
  E.bracket (consEnv e0 dummyRelinker es0)
            unconsEnv
            (\es -> unEff m es *> getEnv es)

-- | Fetch the current representation of the effect.
getStaticRep :: (DispatchOf e ~ Static sideEffects, e :> es) => Eff es (StaticRep e)
getStaticRep = unsafeEff $ \es -> getEnv es

-- | Set the current representation of the effect to the given value.
putStaticRep :: (DispatchOf e ~ Static sideEffects, e :> es) => StaticRep e -> Eff es ()
putStaticRep s = unsafeEff $ \es -> putEnv es s

-- | Apply the function to the current representation of the effect and return a
-- value.
stateStaticRep
  :: (DispatchOf e ~ Static sideEffects, e :> es)
  => (StaticRep e -> (a, StaticRep e))
  -- ^ The function to modify the representation.
  -> Eff es a
stateStaticRep f = unsafeEff $ \es -> stateEnv es f

-- | Apply the monadic function to the current representation of the effect and
-- return a value.
stateStaticRepM
  :: (DispatchOf e ~ Static sideEffects, e :> es)
  => (StaticRep e -> Eff es (a, StaticRep e))
  -- ^ The function to modify the representation.
  -> Eff es a
stateStaticRepM f = unsafeEff $ \es -> E.mask $ \unmask -> do
  (a, s) <- (\s0 -> unmask $ unEff (f s0) es) =<< getEnv es
  putEnv es s
  pure a

-- | Execute a computation with a temporarily modified representation of the
-- effect.
localStaticRep
  :: (DispatchOf e ~ Static sideEffects, e :> es)
  => (StaticRep e -> StaticRep e)
  -- ^ The function to temporarily modify the representation.
  -> Eff es a
  -> Eff es a
localStaticRep f m = unsafeEff $ \es -> do
  E.bracket (stateEnv es $ \s -> (s, f s))
            (\s -> putEnv es s)
            (\_ -> unEff m es)
