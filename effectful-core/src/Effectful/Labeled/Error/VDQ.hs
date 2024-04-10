{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RequiredTypeArguments #-}
-- | Convenience functions for the 'Labeled' 'Error' effect with visible
-- dependent quantification.
--
-- Requires GHC >= 9.10.
--
-- @since 2.4.0.0
module Effectful.Labeled.Error.VDQ
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
import Effectful.Labeled.VDQ
import Effectful.Error.Dynamic (Error(..))
import Effectful.Error.Dynamic qualified as E

-- | Handle errors of type @e@ (via "Effectful.Error.Static").
runError
  :: forall label
  -> Eff (Labeled label (Error e) : es) a
  -> Eff es (Either (E.CallStack, e) a)
runError label = runLabeled label E.runError

-- | Handle errors of type @e@ (via "Effectful.Error.Static") with a specific
-- error handler.
runErrorWith
  :: forall label
  -> (E.CallStack -> e -> Eff es a)
  -- ^ The error handler.
  -> Eff (Labeled label (Error e) : es) a
  -> Eff es a
runErrorWith label = runLabeled label . E.runErrorWith

-- | Handle errors of type @e@ (via "Effectful.Error.Static"). In case of an
-- error discard the 'E.CallStack'.
runErrorNoCallStack
  :: forall label
  -> Eff (Labeled label (Error e) : es) a
  -> Eff es (Either e a)
runErrorNoCallStack label = runLabeled label E.runErrorNoCallStack

-- | Handle errors of type @e@ (via "Effectful.Error.Static") with a specific
-- error handler. In case of an error discard the 'CallStack'.
runErrorNoCallStackWith
  :: forall label
  -> (e -> Eff es a)
  -- ^ The error handler.
  -> Eff (Labeled label (Error e) : es) a
  -> Eff es a
runErrorNoCallStackWith label = runLabeled label . E.runErrorNoCallStackWith

-- | Throw an error of type @e@.
throwError
  :: forall label
  -> (HasCallStack, Labeled label (Error e) :> es)
  => e
  -- ^ The error.
  -> Eff es a
throwError label e = withFrozenCallStack $ send (Labeled @label $ ThrowError e)

-- | Handle an error of type @e@.
catchError
  :: forall label
  -> (HasCallStack, Labeled label (Error e) :> es)
  => Eff es a
  -- ^ The inner computation.
  -> (E.CallStack -> e -> Eff es a)
  -- ^ A handler for errors in the inner computation.
  -> Eff es a
catchError label m = send . Labeled @label . CatchError m

-- | The same as @'flip' 'catchError'@, which is useful in situations where the
-- code for the handler is shorter.
handleError
  :: forall label
  -> (HasCallStack, Labeled label (Error e) :> es)
  => (E.CallStack -> e -> Eff es a)
  -- ^ A handler for errors in the inner computation.
  -> Eff es a
  -- ^ The inner computation.
  -> Eff es a
handleError label = flip (catchError label)

-- | Similar to 'catchError', but returns an 'Either' result which is a 'Right'
-- if no error was thrown and a 'Left' otherwise.
tryError
  :: forall label
  -> (HasCallStack, Labeled label (Error e) :> es)
  => Eff es a
  -- ^ The inner computation.
  -> Eff es (Either (E.CallStack, e) a)
tryError label m = catchError label (Right <$> m) (\es e -> pure $ Left (es, e))
