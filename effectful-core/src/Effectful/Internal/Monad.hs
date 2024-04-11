{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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

  -- * NonDet
  , NonDet(..)

  -- * Fail
  , Fail(..)

  -- * IO
  , IOE
  , runEff

  -- * Prim
  , Prim
  , PrimStateEff
  , runPrim

  -- * Lifting
  , raise
  , raiseWith
  , subsume
  , inject
  , Subset

  -- * Unlifting
  , UnliftStrategy(..)
  , Persistence(..)
  , Limit(..)
  , unliftStrategy
  , withUnliftStrategy
  , withSeqEffToIO
  , withEffToIO
  , withConcEffToIO

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

import Control.Applicative
import Control.Exception qualified as E
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch qualified as C
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Primitive
import Control.Monad.Trans.Control
import Data.Kind (Constraint)
import GHC.Exts (oneShot)
import GHC.IO (IO(..))
import GHC.Stack
import System.IO.Unsafe (unsafeDupablePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Unlift
import Effectful.Internal.Utils

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
--
-- /Note:/ this strategy is implicitly used by the 'MonadUnliftIO' and
-- 'MonadBaseControl' instance for 'Eff'.
unliftStrategy :: IOE :> es => Eff es UnliftStrategy
unliftStrategy = do
  IOE unlift <- getStaticRep
  pure unlift

-- | Locally override the current 'UnliftStrategy' with the given value.
withUnliftStrategy :: IOE :> es => UnliftStrategy -> Eff es a -> Eff es a
withUnliftStrategy unlift = localStaticRep $ \_ -> IOE unlift

-- | Create an unlifting function with the 'SeqUnlift' strategy. For the general
-- version see 'withEffToIO'.
--
-- /Note:/ usage of this function is preferrable to 'Effectful.withRunInIO'
-- because of explicit unlifting strategy and better error reporting.
--
-- @since 2.2.2.0
withSeqEffToIO
  :: (HasCallStack, IOE :> es)
  => ((forall r. Eff es r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
withSeqEffToIO f = unsafeEff $ \es -> seqUnliftIO es f

-- | Create an unlifting function with the given strategy.
--
-- /Note:/ usage of this function is preferrable to 'Effectful.withRunInIO'
-- because of explicit unlifting strategy and better error reporting.
withEffToIO
  :: (HasCallStack, IOE :> es)
  => UnliftStrategy
  -> ((forall r. Eff es r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
withEffToIO strategy k = case strategy of
  SeqUnlift      -> unsafeEff $ \es -> seqUnliftIO es k
  ConcUnlift p b -> unsafeEff $ \es -> concUnliftIO es p b k
{-# INLINE withEffToIO #-}

-- | Create an unlifting function with the 'ConcUnlift' strategy.
--
-- @since 2.2.2.0
withConcEffToIO
  :: (HasCallStack, IOE :> es)
  => Persistence
  -> Limit
  -> ((forall r. Eff es r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
withConcEffToIO persistence limit f = unsafeEff $ \es ->
  concUnliftIO es persistence limit f
{-# DEPRECATED withConcEffToIO "Use withEffToIO with the appropriate strategy." #-}

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
-- NonDet

-- | Provide the ability to use the 'Alternative' and 'MonadPlus' instance for
-- 'Eff'.
--
-- @since 2.2.0.0
data NonDet :: Effect where
  Empty   :: NonDet m a
  (:<|>:) :: m a -> m a -> NonDet m a

type instance DispatchOf NonDet = Dynamic

-- | @since 2.2.0.0
instance NonDet :> es => Alternative (Eff es) where
  empty   = withFrozenCallStack (send Empty)
  a <|> b = send (a :<|>: b)

-- | @since 2.2.0.0
instance NonDet :> es => MonadPlus (Eff es)

----------------------------------------
-- Exception

instance C.MonadThrow (Eff es) where
  throwM = unsafeEff_ . E.throwIO

instance C.MonadCatch (Eff es) where
  catch m handler = unsafeEff $ \es -> do
    unEff m es `E.catch` \e -> do
      unEff (handler e) es

instance C.MonadMask (Eff es) where
  mask k = unsafeEff $ \es -> E.mask $ \unmask ->
    unEff (k $ \m -> unsafeEff $ unmask . unEff m) es

  uninterruptibleMask k = unsafeEff $ \es -> E.uninterruptibleMask $ \unmask ->
    unEff (k $ \m -> unsafeEff $ unmask . unEff m) es

  generalBracket acquire release use = unsafeEff $ \es -> E.mask $ \unmask -> do
    resource <- unEff acquire es
    b <- unmask (unEff (use resource) es) `E.catch` \e -> do
      _ <- unEff (release resource $ C.ExitCaseException e) es
      E.throwIO e
    c <- unEff (release resource $ C.ExitCaseSuccess b) es
    pure (b, c)

----------------------------------------
-- Fail

-- | Provide the ability to use the 'MonadFail' instance for 'Eff'.
data Fail :: Effect where
  Fail :: String -> Fail m a

type instance DispatchOf Fail = Dynamic

instance Fail :> es => MonadFail (Eff es) where
  fail msg = withFrozenCallStack $ send (Fail msg)

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

-- | Instance included for compatibility with existing code.
--
-- Usage of 'withEffToIO' is preferrable as it allows specifying the
-- 'UnliftStrategy' on a case-by-case basis and has better error reporting.
--
-- /Note:/ the unlifting strategy for 'withRunInIO' is taken from the 'IOE'
-- context (see 'unliftStrategy').
instance IOE :> es => MonadUnliftIO (Eff es) where
  withRunInIO k = unliftStrategy >>= (`withEffToIO` k)

-- | Instance included for compatibility with existing code.
--
-- Usage of 'liftIO' is preferrable as it's a standard.
instance IOE :> es => MonadBase IO (Eff es) where
  liftBase = unsafeEff_

-- | Instance included for compatibility with existing code.
--
-- Usage of 'withEffToIO' is preferrable as it allows specifying the
-- 'UnliftStrategy' on a case-by-case basis and has better error reporting.
--
-- /Note:/ the unlifting strategy for 'liftBaseWith' is taken from the 'IOE'
-- context (see 'unliftStrategy').
instance IOE :> es => MonadBaseControl IO (Eff es) where
  type StM (Eff es) a = a
  liftBaseWith k = unliftStrategy >>= (`withEffToIO` k)
  restoreM = pure

----------------------------------------
-- Primitive

-- | Provide the ability to perform primitive state-transformer actions.
data Prim :: Effect

type instance DispatchOf Prim = Static WithSideEffects
data instance StaticRep Prim = Prim

-- | 'PrimState' token for 'Eff'. Used instead of 'RealWorld' to prevent the
-- 'Prim' effect from executing arbitrary 'IO' actions via 'ioToPrim'.
data PrimStateEff

-- | Run an 'Eff' computation with primitive state-transformer actions.
runPrim :: IOE :> es => Eff (Prim : es) a -> Eff es a
runPrim = evalStaticRep Prim

instance Prim :> es => PrimMonad (Eff es) where
  type PrimState (Eff es) = PrimStateEff
  primitive = unsafeEff_ . IO . unsafeCoerce

----------------------------------------
-- Lifting

-- | Lift an 'Eff' computation into an effect stack with one more effect.
raise :: Eff es a -> Eff (e : es) a
raise m = unsafeEff $ \es -> unEff m =<< tailEnv es

-- | Lift an 'Eff' computation into an effect stack with one more effect and
-- create an unlifting function with the given strategy.
--
-- @since 1.2.0.0
raiseWith
  :: HasCallStack
  => UnliftStrategy
  -> ((forall r. Eff (e : es) r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff (e : es) a
raiseWith strategy k = case strategy of
  SeqUnlift -> unsafeEff $ \ees -> do
    es <- tailEnv ees
    seqUnliftIO ees $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift
  ConcUnlift p l -> unsafeEff $ \ees -> do
    es <- tailEnv ees
    concUnliftIO ees p l $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift
{-# INLINE raiseWith #-}

-- | Eliminate a duplicate effect from the top of the effect stack.
subsume :: e :> es => Eff (e : es) a -> Eff es a
subsume m = unsafeEff $ \es -> unEff m =<< subsumeEnv es

-- | Allow for running an effect stack @xs@ within @es@ as long as @xs@ is a
-- permutation (with possible duplicates) of a subset of @es@.
--
-- Generalizes 'raise' and 'subsume'.
--
-- >>> data E1 :: Effect
-- >>> data E2 :: Effect
-- >>> data E3 :: Effect
--
-- It makes it possible to rearrange the effect stack however you like:
--
-- >>> :{
--   shuffle :: Eff (E3 : E1 : E2 : es) a -> Eff (E1 : E2 : E3 : es) a
--   shuffle = inject
-- :}
--
-- It can also turn a monomorphic effect stack into a polymorphic one:
--
-- >>> :{
--   toPoly :: (E1 :> es, E2 :> es, E3 :> es) => Eff [E1, E2, E3] a -> Eff es a
--   toPoly = inject
-- :}
--
-- Moreover, it allows for hiding specific effects from downstream:
--
-- >>> :{
--   onlyE1 :: Eff (E1 : es) a -> Eff (E1 : E2 : E3 : es) a
--   onlyE1 = inject
-- :}
--
-- >>> :{
--   onlyE2 :: Eff (E2 : es) a -> Eff (E1 : E2 : E3 : es) a
--   onlyE2 = inject
-- :}
--
-- >>> :{
--   onlyE3 :: Eff (E3 : es) a -> Eff (E1 : E2 : E3 : es) a
--   onlyE3 = inject
-- :}
--
-- However, it's not possible to inject a computation into an incompatible
-- effect stack:
--
-- >>> :{
--   coerceEs :: Eff es1 a -> Eff es2 a
--   coerceEs = inject
-- :}
-- ...
-- ...Couldn't match type ‘es1’ with ‘es2’
-- ...
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
-- technical reasons to guarantee soundness (see
-- t'Effectful.Dispatch.Dynamic.SharedSuffix' for more information).
newtype LocalEnv (localEs :: [Effect]) (handlerEs :: [Effect]) = LocalEnv (Env localEs)

-- | Type signature of the effect handler.
type EffectHandler e es
  = forall a localEs. (HasCallStack, e :> localEs)
  => LocalEnv localEs es
  -- ^ Capture of the local environment for handling local 'Eff' computations
  -- when @e@ is a higher order effect.
  -> e (Eff localEs) a
  -- ^ The effect performed in the local environment.
  -> Eff es a

-- | An internal representation of dynamically dispatched effects, i.e. the
-- effect handler bundled with its environment.
data Handler :: Effect -> Type where
  Handler :: !(Env handlerEs) -> !(EffectHandler e handlerEs) -> Handler e
type instance EffectRep Dynamic = Handler

relinkHandler :: Relinker Handler e
relinkHandler = Relinker $ \relink (Handler handlerEs handler) -> do
  newHandlerEs <- relink handlerEs
  pure $ Handler newHandlerEs handler

-- | Run a dynamically dispatched effect with the given handler.
runHandler :: DispatchOf e ~ Dynamic => Handler e -> Eff (e : es) a -> Eff es a
runHandler e m = unsafeEff $ \es0 -> do
  inlineBracket
    (consEnv e relinkHandler es0)
    unconsEnv
    (\es -> unEff m es)

-- | Send an operation of the given effect to its handler for execution.
send
  :: (HasCallStack, DispatchOf e ~ Dynamic, e :> es)
  => e (Eff es) a
  -- ^ The operation.
  -> Eff es a
send op = unsafeEff $ \es -> do
  Handler handlerEs handler <- getEnv es
  -- Prevent internal functions that rebind the effect handler from polluting
  -- its call stack by freezing it. Note that functions 'interpret',
  -- 'reinterpret', 'interpose' and 'impose' need to thaw it so that useful
  -- stack frames from inside the effect handler continue to be added.
  unEff (withFrozenCallStack handler (LocalEnv es) op) handlerEs
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
  inlineBracket
    (consEnv e0 dummyRelinker es0)
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
  inlineBracket
    (consEnv e dummyRelinker es0)
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
  inlineBracket
    (consEnv e0 dummyRelinker es0)
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
stateStaticRep f = unsafeEff $ \es -> stateEnv es (pure . f)

-- | Apply the monadic function to the current representation of the effect and
-- return a value.
stateStaticRepM
  :: (DispatchOf e ~ Static sideEffects, e :> es)
  => (StaticRep e -> Eff es (a, StaticRep e))
  -- ^ The function to modify the representation.
  -> Eff es a
stateStaticRepM f = unsafeEff $ \es -> E.mask $ \unmask -> do
  stateEnv es $ unmask . (`unEff` es) . f

-- | Execute a computation with a temporarily modified representation of the
-- effect.
localStaticRep
  :: (DispatchOf e ~ Static sideEffects, e :> es)
  => (StaticRep e -> StaticRep e)
  -- ^ The function to temporarily modify the representation.
  -> Eff es a
  -> Eff es a
localStaticRep f m = unsafeEff $ \es -> do
  inlineBracket
    (stateEnv es $ \s -> pure (s, f s))
    (\s -> putEnv es s)
    (\_ -> unEff m es)
