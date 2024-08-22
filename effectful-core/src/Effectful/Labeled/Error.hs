{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Convenience functions for the 'Labeled' 'Error' effect.
--
-- @since 2.4.0.0
module Effectful.Labeled.Error
  ( -- * Effect
    Error(..)

    -- ** Handlers
  , runError
  , runErrorWith
  , runErrorNoCallStack
  , runErrorNoCallStackWith

    -- ** Operations
  , throwError
  , catchError
  , handleError
  , tryError

    -- * Re-exports
  , E.HasCallStack
  , E.CallStack
  , E.getCallStack
  , E.prettyCallStack
  ) where

import GHC.Stack (withFrozenCallStack)

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Labeled
import Effectful.Error.Dynamic (Error(..))
import Effectful.Error.Dynamic qualified as E

-- | Handle errors of type @e@ (via "Effectful.Error.Static").
runError
  :: forall label e es a
   . Eff (Labeled label (Error e) : es) a
  -> Eff es (Either (E.CallStack, e) a)
runError = runLabeled @label E.runError

-- | Handle errors of type @e@ (via "Effectful.Error.Static") with a specific
-- error handler.
runErrorWith
  :: forall label e es a
  . (E.CallStack -> e -> Eff es a)
  -- ^ The error handler.
  -> Eff (Labeled label (Error e) : es) a
  -> Eff es a
runErrorWith = runLabeled @label . E.runErrorWith

-- | Handle errors of type @e@ (via "Effectful.Error.Static"). In case of an
-- error discard the 'E.CallStack'.
runErrorNoCallStack
  :: forall label e es a
   . Eff (Labeled label (Error e) : es) a
  -> Eff es (Either e a)
runErrorNoCallStack = runLabeled @label E.runErrorNoCallStack

-- | Handle errors of type @e@ (via "Effectful.Error.Static") with a specific
-- error handler. In case of an error discard the 'CallStack'.
runErrorNoCallStackWith
  :: forall label e es a
   . (e -> Eff es a)
  -- ^ The error handler.
  -> Eff (Labeled label (Error e) : es) a
  -> Eff es a
runErrorNoCallStackWith = runLabeled @label . E.runErrorNoCallStackWith

-- | Throw an error of type @e@.
throwError
  :: forall label e es a
   . (HasCallStack, Labeled label (Error e) :> es)
  => e
  -- ^ The error.
  -> Eff es a
throwError e = withFrozenCallStack $ send (Labeled @label $ ThrowError e)

-- | Handle an error of type @e@.
catchError
  :: forall label e es a
   . (HasCallStack, Labeled label (Error e) :> es)
  => Eff es a
  -- ^ The inner computation.
  -> (E.CallStack -> e -> Eff es a)
  -- ^ A handler for errors in the inner computation.
  -> Eff es a
catchError m = send . Labeled @label . CatchError m

-- | The same as @'flip' 'catchError'@, which is useful in situations where the
-- code for the handler is shorter.
handleError
  :: forall label e es a
   . (HasCallStack, Labeled label (Error e) :> es)
  => (E.CallStack -> e -> Eff es a)
  -- ^ A handler for errors in the inner computation.
  -> Eff es a
  -- ^ The inner computation.
  -> Eff es a
handleError = flip (catchError @label)

-- | Similar to 'catchError', but returns an 'Either' result which is a 'Right'
-- if no error was thrown and a 'Left' otherwise.
tryError
  :: forall label e es a
   . (HasCallStack, Labeled label (Error e) :> es)
  => Eff es a
  -- ^ The inner computation.
  -> Eff es (Either (E.CallStack, e) a)
tryError m = catchError @label (Right <$> m) (\es e -> pure $ Left (es, e))
