-- | The 'Eff' monad comes with instances of 'MonadThrow', 'MonadCatch' and
-- 'MonadMask' from the
-- [@exceptions@](https://hackage.haskell.org/package/exceptions) library, so
-- this module simply re-exports the interface of the
-- [@safe-exceptions@](https://hackage.haskell.org/package/safe-exceptions)
-- library.
--
-- Why @safe-exceptions@ and not @exceptions@? Because the former makes it much
-- easier to correctly deal with asynchronous exceptions (for more information
-- see its [README](https://github.com/fpco/safe-exceptions#readme)) and
-- provides more convenience functions.
module Effectful.Exception
  ( -- * Throwing
    C.MonadThrow(..)
  , Safe.throwString
  , Safe.StringException(..)

    -- * Catching (with recovery)
  , C.MonadCatch(..)
  , Safe.catchIO
  , Safe.catchIOError
  , Safe.catchAny
  , Safe.catchDeep
  , Safe.catchAnyDeep
  , Safe.catchAsync
  , Safe.catchJust

  , Safe.handle
  , Safe.handleIO
  , Safe.handleIOError
  , Safe.handleAny
  , Safe.handleDeep
  , Safe.handleAnyDeep
  , Safe.handleAsync
  , Safe.handleJust

  , Safe.try
  , Safe.tryIO
  , Safe.tryAny
  , Safe.tryDeep
  , Safe.tryAnyDeep
  , Safe.tryAsync
  , Safe.tryJust

  , Safe.Handler(..)
  , Safe.catches
  , Safe.catchesDeep
  , Safe.catchesAsync

    -- * Cleanup (no recovery)
  , C.MonadMask(..)
  , C.ExitCase(..)
  , Safe.onException
  , Safe.bracket
  , Safe.bracket_
  , Safe.finally
  , Safe.withException
  , Safe.bracketOnError
  , Safe.bracketOnError_
  , Safe.bracketWithError

    -- * Utilities

    -- ** Coercion to sync and async
  , Safe.SyncExceptionWrapper(..)
  , Safe.toSyncException
  , Safe.AsyncExceptionWrapper(..)
  , Safe.toAsyncException

    -- ** Check exception type
  , Safe.isSyncException
  , Safe.isAsyncException

    -- ** Evaluation
  , evaluate
  , evaluateDeep

    -- * Re-exports from "Control.Exception"

    -- ** The 'SomeException' type
  , E.SomeException(..)

    -- ** The 'Exception' class
  , E.Exception(..)

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

import Control.DeepSeq
import Control.Exception qualified as E
import Control.Exception.Safe qualified as Safe
import Control.Monad.Catch qualified as C

import Effectful
import Effectful.Dispatch.Static

-- | Lifted version of 'E.evaluate'.
evaluate :: a -> Eff es a
evaluate = unsafeEff_ . E.evaluate

-- | Deeply evaluate a value using 'evaluate' and 'NFData'.
evaluateDeep :: NFData a => a -> Eff es a
evaluateDeep = unsafeEff_ . E.evaluate . force
