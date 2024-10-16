{-# LANGUAGE CPP #-}
-- | Support for runtime exceptions.
--
-- This module provides thin wrappers over functions from "Control.Exception" as
-- well as several utility functions for convenience.
--
-- /Note:/ the 'Eff' monad provides instances for 'C.MonadThrow', 'C.MonadCatch'
-- and 'C.MonadMask', so any existing code that uses them remains compatible.
module Effectful.Exception
  ( -- * Throwing
    throwIO

    -- * Catching (with recovery)
    -- $catchAll
  , catch
  , catchDeep
  , catchJust
  , catchIf
  , catchIO
  , catchSync
  , catchSyncDeep

  , handle
  , handleDeep
  , handleJust
  , handleIf
  , handleIO
  , handleSync
  , handleSyncDeep

  , try
  , tryDeep
  , tryJust
  , tryIf
  , tryIO
  , trySync
  , trySyncDeep

  , C.Handler(..)
  , catches
  , catchesDeep

    -- | #cleanup#

    -- * Cleanup (no recovery)
  , bracket
  , bracket_
  , bracketOnError
  , generalBracket
  , C.ExitCase(..)
  , finally
  , onException

    -- * Utils

    -- ** Evaluation
  , evaluate
  , evaluateDeep

#if MIN_VERSION_base(4,20,0)
    -- ** Annotations
  , annotateIO
#endif

    -- ** Check exception type
    -- $syncVsAsync
  , isSyncException
  , isAsyncException

    -- * Low-level API
  , mask
  , mask_
  , uninterruptibleMask
  , uninterruptibleMask_
  , E.MaskingState(..)
  , getMaskingState
  , interruptible
  , allowInterrupt

    -- * Re-exports from "Control.Exception"

    -- ** The 'SomeException' type
  , E.SomeException(..)

    -- ** The 'Exception' class
  , E.Exception(..)
  , E.mapException

#if MIN_VERSION_base(4,20,0)
    -- ** Exception context and annotation
  , E.addExceptionContext
  , E.someExceptionContext
  , E.ExceptionWithContext(..)
  , E.ExceptionContext(..)
  , E.emptyExceptionContext
  , E.addExceptionAnnotation
  , E.getExceptionAnnotations
  , E.getAllExceptionAnnotations
  , E.displayExceptionContext
  , E.SomeExceptionAnnotation(..)
  , E.ExceptionAnnotation(..)
#endif

    -- ** Concrete exception types
  , E.IOException
  , E.ArithException(..)
  , E.ArrayException(..)
  , E.AssertionFailed(..)
  , E.NoMethodError(..)
  , E.PatternMatchFail(..)
  , E.RecConError(..)
  , E.RecSelError(..)
  , E.RecUpdError(..)
  , E.ErrorCall(..)
  , E.TypeError(..)

    -- ** Asynchronous exceptions
  , E.SomeAsyncException(..)
  , E.AsyncException(..)
  , E.asyncExceptionToException
  , E.asyncExceptionFromException
  , E.NonTermination(..)
  , E.NestedAtomically(..)
  , E.BlockedIndefinitelyOnMVar(..)
  , E.BlockedIndefinitelyOnSTM(..)
  , E.AllocationLimitExceeded(..)
  , E.CompactionFailed(..)
  , E.Deadlock(..)

    -- ** Assertions
  , E.assert
  ) where

#if MIN_VERSION_base(4,20,0)
import Control.Exception.Annotation qualified as E
import Control.Exception.Context qualified as E
#endif

import Control.DeepSeq
import Control.Exception qualified as E
import Control.Monad.Catch qualified as C
import GHC.Stack (withFrozenCallStack)

import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Unsafe

----------------------------------------
-- Throwing

-- | Lifted 'E.throwIO'.
throwIO
  :: (HasCallStack, E.Exception e)
  => e
  -- ^ The error.
  -> Eff es a
throwIO = unsafeEff_ . withFrozenCallStack E.throwIO

----------------------------------------
-- Catching

-- $catchAll
--
-- /Note:/ __do not use 'catch', 'handle' or 'try' to catch 'E.SomeException'__
-- unless you're really sure you want to catch __all__ exceptions (including
-- asynchronous ones). Instead:
--
-- - If you want to catch all exceptions, run a cleanup action and rethrow, use
--   one of the functions from the [cleanup](#cleanup) section.
--
-- - If you want to catch all synchronous exceptions, use 'catchSync',
--   'handleSync' or 'trySync'.

-- | Lifted 'E.catch'.
catch
  :: E.Exception e
  => Eff es a
  -> (e -> Eff es a)
  -- ^ The exception handler.
  -> Eff es a
catch action handler = reallyUnsafeUnliftIO $ \unlift -> do
  E.catch (unlift action) (unlift . handler)

-- | A variant of 'catch' that fully forces evaluation of the result value to
-- find all impure exceptions.
catchDeep
  :: (E.Exception e, NFData a)
  => Eff es a
  -> (e -> Eff es a)
  -- ^ The exception handler.
  -> Eff es a
catchDeep action = catch (evaluateDeep =<< action)

-- | Lifted 'E.catchJust'.
catchJust
  :: E.Exception e
  => (e -> Maybe b)
  -- ^ The predicate.
  -> Eff es a
  -> (b -> Eff es a)
  -- ^ The exception handler.
  -> Eff es a
catchJust f action handler = reallyUnsafeUnliftIO $ \unlift -> do
  E.catchJust f (unlift action) (unlift . handler)

-- | Catch an exception only if it passes a specific predicate.
catchIf
  :: E.Exception e
  => (e -> Bool)
  -- ^ The predicate.
  -> Eff es a
  -> (e -> Eff es a)
  -- ^ The exception handler.
  -> Eff es a
catchIf p = catchJust (\e -> if p e then Just e else Nothing)

-- | 'catch' specialized to catch 'IOException'.
catchIO
  :: Eff es a
  -> (E.IOException -> Eff es a)
  -- ^ The exception handler.
  -> Eff es a
catchIO = catch

-- | 'catch' specialized to catch all exceptions considered to be synchronous.
--
-- @'catchSync' ≡ 'catchIf' \@'E.SomeException' 'isSyncException'@
--
-- See 'isSyncException' for more information.
catchSync
  :: Eff es a
  -> (E.SomeException -> Eff es a)
  -- ^ The exception handler.
  -> Eff es a
catchSync = catchIf @E.SomeException isSyncException

-- | A variant of 'catchSync' that fully forces evaluation of the result value
-- to find all impure exceptions.
catchSyncDeep
  :: NFData a
  => Eff es a
  -> (E.SomeException -> Eff es a)
  -- ^ The exception handler.
  -> Eff es a
catchSyncDeep action = catchSync (evaluateDeep =<< action)

-- | Flipped version of 'catch'.
handle
  :: E.Exception e
  => (e -> Eff es a)
  -- ^ The exception handler.
  -> Eff es a
  -> Eff es a
handle = flip catch

-- | Flipped version of 'catchDeep'.
handleDeep
  :: (E.Exception e, NFData a)
  => (e -> Eff es a)
  -- ^ The exception handler.
  -> Eff es a
  -> Eff es a
handleDeep = flip catchDeep

-- | Flipped version of 'catchJust'.
handleJust
  :: (HasCallStack, E.Exception e)
  => (e -> Maybe b)
  -- ^ The predicate.
  -> (b -> Eff es a)
  -- ^ The exception handler.
  -> Eff es a
  -> Eff es a
handleJust f = flip (catchJust f)

-- | Flipped version of 'catchIf'.
handleIf
  :: E.Exception e
  => (e -> Bool)
  -- ^ The predicate.
  -> (e -> Eff es a)
  -- ^ The exception handler.
  -> Eff es a
  -> Eff es a
handleIf p = flip (catchIf p)

-- | Flipped version of 'catchIO'.
handleIO
  :: (E.IOException -> Eff es a)
  -- ^ The exception handler.
  -> Eff es a
  -> Eff es a
handleIO = flip catchIO

-- | Flipped version of 'catchSync'.
handleSync
  :: (E.SomeException -> Eff es a)
  -- ^ The exception handler.
  -> Eff es a
  -> Eff es a
handleSync = flip catchSync

-- | Flipped version of 'catchSyncDeep'.
handleSyncDeep
  :: NFData a
  => (E.SomeException -> Eff es a)
  -- ^ The exception handler.
  -> Eff es a
  -> Eff es a
handleSyncDeep = flip catchSyncDeep

-- | Lifted 'E.try'.
try
  :: E.Exception e
  => Eff es a
  -- ^ The action.
  -> Eff es (Either e a)
try action = reallyUnsafeUnliftIO $ \unlift -> do
  E.try (unlift action)

-- | A variant of 'try' that fully forces evaluation of the result value to find
-- all impure exceptions.
tryDeep
  :: (E.Exception e, NFData a)
  => Eff es a
  -- ^ The action.
  -> Eff es (Either e a)
tryDeep action = try (evaluateDeep =<< action)

-- | Lifted 'E.tryJust'.
tryJust
  :: E.Exception e
  => (e -> Maybe b)
  -- ^ The predicate.
  -> Eff es a
  -> Eff es (Either b a)
tryJust f action = reallyUnsafeUnliftIO $ \unlift -> do
  E.tryJust f (unlift action)

-- | Catch an exception only if it passes a specific predicate.
tryIf
  :: E.Exception e
  => (e -> Bool)
  -- ^ The predicate.
  -> Eff es a
  -> Eff es (Either e a)
tryIf p = tryJust (\e -> if p e then Just e else Nothing)

-- | 'try' specialized to catch 'IOException'.
tryIO
  :: Eff es a
  -- ^ The action.
  -> Eff es (Either E.IOException a)
tryIO = try

-- | 'try' specialized to catch all exceptions considered to be synchronous.
--
-- @'trySync' ≡ 'tryIf' \@'E.SomeException' 'isSyncException'@
--
-- See 'isSyncException' for more information.
trySync
  :: Eff es a
  -- ^ The action.
  -> Eff es (Either E.SomeException a)
trySync = tryIf @E.SomeException isSyncException

-- | A variant of 'trySync' that fully forces evaluation of the result value to
-- find all impure exceptions.
trySyncDeep
  :: NFData a
  => Eff es a
  -- ^ The action.
  -> Eff es (Either E.SomeException a)
trySyncDeep action = trySync (evaluateDeep =<< action)

-- | Lifted 'E.catches'.
catches
  :: Eff es a
  -> [C.Handler (Eff es) a]
  -- ^ The exception handlers.
  -> Eff es a
catches action handlers = reallyUnsafeUnliftIO $ \unlift -> do
  let unliftHandler (C.Handler handler) = E.Handler (unlift . handler)
  E.catches (unlift action) (map unliftHandler handlers)

-- | A variant of 'catches' that fully forces evaluation of the result value to
-- find all impure exceptions.
catchesDeep
  :: NFData a
  => Eff es a
  -> [C.Handler (Eff es) a]
  -- ^ The exception handlers.
  -> Eff es a
catchesDeep action = catches (evaluateDeep =<< action)

----------------------------------------
-- Cleanup

-- | Lifted 'E.bracket'.
bracket
  :: Eff es a
  -- ^ Computation to run first.
  -> (a -> Eff es b)
  -- ^ Computation to run last.
  -> (a -> Eff es c)
  -- ^ Computation to run in-between.
  -> Eff es c
bracket before after action = reallyUnsafeUnliftIO $ \unlift -> do
  E.bracket (unlift before) (unlift . after) (unlift . action)

-- | Lifted 'E.bracket_'.
bracket_
  :: Eff es a
  -- ^ Computation to run first.
  -> Eff es b
  -- ^ Computation to run last.
  -> Eff es c
  -- ^ Computation to run in-between.
  -> Eff es c
bracket_ before after action = reallyUnsafeUnliftIO $ \unlift -> do
  E.bracket_ (unlift before) (unlift after) (unlift action)

-- | Lifted 'E.bracketOnError'.
bracketOnError
  :: Eff es a
  -- ^ Computation to run first.
  -> (a -> Eff es b)
  -- ^ Computation to run last when an exception or
  -- t'Effectful.Error.Static.Error' was thrown.
  -> (a -> Eff es c)
  -- ^ Computation to run in-between.
  -> Eff es c
bracketOnError before after action = reallyUnsafeUnliftIO $ \unlift -> do
  E.bracketOnError (unlift before) (unlift . after) (unlift . action)

-- | Generalization of 'bracket'.
--
-- See 'C.generalBracket' for more information.
generalBracket
  :: Eff es a
  -- ^ Computation to run first.
  -> (a -> C.ExitCase c -> Eff es b)
  -- ^ Computation to run last.
  -> (a -> Eff es c)
  -- ^ Computation to run in-between.
  -> Eff es (c, b)
generalBracket = C.generalBracket

-- | Lifted 'E.finally'.
finally
  :: Eff es a
  -> Eff es b
  -- ^ Computation to run last.
  -> Eff es a
finally action handler = reallyUnsafeUnliftIO $ \unlift -> do
  E.finally (unlift action) (unlift handler)

-- | Lifted 'E.onException'.
onException
  :: Eff es a
  -> Eff es b
  -- ^ Computation to run last when an exception or
  -- t'Effectful.Error.Static.Error' was thrown.
  -> Eff es a
onException action handler = reallyUnsafeUnliftIO $ \unlift -> do
  E.onException (unlift action) (unlift handler)

----------------------------------------
-- Utils

-- | Lifted 'E.evaluate'.
evaluate :: a -> Eff es a
evaluate = unsafeEff_ . E.evaluate

-- | Deeply evaluate a value using 'evaluate' and 'NFData'.
evaluateDeep :: NFData a => a -> Eff es a
evaluateDeep = unsafeEff_ . E.evaluate . force

#if MIN_VERSION_base(4,20,0)
-- | Lifted 'E.annotateIO'.
annotateIO :: E.ExceptionAnnotation e => e -> Eff es a -> Eff es a
annotateIO e action = reallyUnsafeUnliftIO $ \unlift -> do
  E.annotateIO e (unlift action)
#endif

----------------------------------------
-- Check exception type

-- $syncVsAsync
--
-- /Note:/ there's no way to determine whether an exception was thrown
-- synchronously or asynchronously, so these functions rely on a
-- heuristic. Namely, an exception type is determined by its 'E.Exception'
-- instance.
--
-- Exception types with the default 'E.Exception' instance are considered
-- synchronous:
--
-- >>> import Control.Exception qualified as E
--
-- >>> data SyncEx = SyncEx deriving (Show)
-- >>> instance E.Exception SyncEx
--
-- >>> isSyncException SyncEx
-- True
--
-- >>> isAsyncException SyncEx
-- False
--
-- Whereas for asynchronous exceptions you need to define their 'E.Exception'
-- instance as follows:
--
-- >>> data AsyncEx = AsyncEx deriving (Show)
-- >>> :{
--   instance E.Exception AsyncEx where
--     toException = E.asyncExceptionToException
--     fromException = E.asyncExceptionFromException
-- :}
--
-- >>> isSyncException AsyncEx
-- False
--
-- >>> isAsyncException AsyncEx
-- True

-- | Check if the given exception is considered synchronous.
isSyncException :: E.Exception e => e -> Bool
isSyncException e = case E.fromException (E.toException e) of
  Just E.SomeAsyncException{} -> False
  Nothing -> True

-- | Check if the given exception is considered asynchronous.
isAsyncException :: E.Exception e => e -> Bool
isAsyncException e = case E.fromException (E.toException e) of
  Just E.SomeAsyncException{} -> True
  Nothing -> False

----------------------------------------
-- Low-level API

-- | Lifted 'E.mask'.
mask :: ((forall r. Eff es r -> Eff es r) -> Eff es a) -> Eff es a
mask k = reallyUnsafeUnliftIO $ \unlift -> do
  E.mask $ \release -> unlift $ k (reallyUnsafeLiftMapIO release)

-- | Lifted 'E.mask_'.
mask_ :: Eff es a -> Eff es a
mask_ action = reallyUnsafeUnliftIO $ \unlift -> do
  E.mask_ (unlift action)

-- | Lifted 'E.uninterruptibleMask'.
uninterruptibleMask :: ((forall r. Eff es r -> Eff es r) -> Eff es a) -> Eff es a
uninterruptibleMask k = reallyUnsafeUnliftIO $ \unlift -> do
  E.uninterruptibleMask $ \release -> unlift $ k (reallyUnsafeLiftMapIO release)

-- | Lifted 'E.uninterruptibleMask_'.
uninterruptibleMask_ :: Eff es a -> Eff es a
uninterruptibleMask_ action = reallyUnsafeUnliftIO $ \unlift -> do
  E.uninterruptibleMask_ (unlift action)

-- | Lifted 'E.getMaskingState'.
getMaskingState :: Eff es E.MaskingState
getMaskingState = unsafeEff_ E.getMaskingState

-- | Lifted 'E.interruptible'.
interruptible :: Eff es a -> Eff es a
interruptible action = reallyUnsafeUnliftIO $ \unlift -> do
  E.interruptible (unlift action)

-- | Lifted 'E.allowInterrupt'.
allowInterrupt :: Eff es ()
allowInterrupt = unsafeEff_ E.allowInterrupt
