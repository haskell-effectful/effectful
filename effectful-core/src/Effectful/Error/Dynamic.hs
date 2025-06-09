{-# OPTIONS_GHC -Wno-orphans #-}
-- | The dynamically dispatched variant of the 'Error' effect.
--
-- /Note:/ unless you plan to change interpretations at runtime or you need the
-- 'MTL.MonadError' instance for compatibility with existing code, it's
-- recommended to use the statically dispatched variant,
-- i.e. "Effectful.Error.Static".
module Effectful.Error.Dynamic
  ( -- * Effect
    Error(..)

    -- ** Handlers
  , runError
  , runErrorWith
  , runErrorNoCallStack
  , runErrorNoCallStackWith

    -- ** Operations
  , throwErrorWith
  , throwError
  , throwError_
  , catchError
  , handleError
  , tryError

    -- * Re-exports
  , E.HasCallStack
  , E.CallStack
  , E.getCallStack
  , E.prettyCallStack
  ) where

import Control.Monad.Except qualified as MTL
import GHC.Stack (withFrozenCallStack)

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static qualified as E

-- | Provide the ability to handle errors of type @e@.
data Error e :: Effect where
  -- | @since 2.4.0.0
  ThrowErrorWith :: (e -> String) -> e -> Error e m a
  CatchError :: m a -> (E.CallStack -> e -> m a) -> Error e m a

type instance DispatchOf (Error e) = Dynamic

-- | Handle errors of type @e@ (via "Effectful.Error.Static").
runError
  :: HasCallStack
  => Eff (Error e : es) a
  -> Eff es (Either (E.CallStack, e) a)
runError = reinterpret E.runError $ \env -> \case
  ThrowErrorWith display e -> E.throwErrorWith display e
  CatchError m h -> localSeqUnlift env $ \unlift -> do
    E.catchError (unlift m) (\cs -> unlift . h cs)

-- | Handle errors of type @e@ (via "Effectful.Error.Static") with a specific
-- error handler.
--
-- @since 2.3.0.0
runErrorWith
  :: HasCallStack
  => (E.CallStack -> e -> Eff es a)
  -- ^ The error handler.
  -> Eff (Error e : es) a
  -> Eff es a
runErrorWith handler m = runError m >>= \case
  Left (cs, e) -> handler cs e
  Right a -> pure a

-- | Handle errors of type @e@ (via "Effectful.Error.Static"). In case of an
-- error discard the 'E.CallStack'.
--
-- @since 2.3.0.0
runErrorNoCallStack
  :: HasCallStack
  => Eff (Error e : es) a
  -> Eff es (Either e a)
runErrorNoCallStack = fmap (either (Left . snd) Right) . runError

-- | Handle errors of type @e@ (via "Effectful.Error.Static") with a specific
-- error handler. In case of an error discard the 'CallStack'.
runErrorNoCallStackWith
  :: HasCallStack
  => (e -> Eff es a)
  -- ^ The error handler.
  -> Eff (Error e : es) a
  -> Eff es a
runErrorNoCallStackWith handler m = runErrorNoCallStack m >>= \case
  Left e -> handler e
  Right a -> pure a

-- | Throw an error of type @e@ and specify a display function in case a
-- third-party code catches the internal exception and 'show's it.
--
-- @since 2.4.0.0
throwErrorWith
  :: (HasCallStack, Error e :> es)
  => (e -> String)
  -- ^ The display function.
  -> e
  -- ^ The error.
  -> Eff es a
throwErrorWith display = withFrozenCallStack send . ThrowErrorWith display

-- | Throw an error of type @e@ with 'show' as a display function.
throwError
  :: (HasCallStack, Error e :> es, Show e)
  => e
  -- ^ The error.
  -> Eff es a
throwError = withFrozenCallStack throwErrorWith show

-- | Throw an error of type @e@ with no display function.
--
-- @since 2.4.0.0
throwError_
  :: (HasCallStack, Error e :> es)
  => e
  -- ^ The error.
  -> Eff es a
throwError_ = withFrozenCallStack throwErrorWith (const "<opaque>")

-- | Handle an error of type @e@.
catchError
  :: (HasCallStack, Error e :> es)
  => Eff es a
  -- ^ The inner computation.
  -> (E.CallStack -> e -> Eff es a)
  -- ^ A handler for errors in the inner computation.
  -> Eff es a
catchError m = send . CatchError m

-- | The same as @'flip' 'catchError'@, which is useful in situations where the
-- code for the handler is shorter.
handleError
  :: (HasCallStack, Error e :> es)
  => (E.CallStack -> e -> Eff es a)
  -- ^ A handler for errors in the inner computation.
  -> Eff es a
  -- ^ The inner computation.
  -> Eff es a
handleError = flip catchError

-- | Similar to 'catchError', but returns an 'Either' result which is a 'Right'
-- if no error was thrown and a 'Left' otherwise.
tryError
  :: (HasCallStack, Error e :> es)
  => Eff es a
  -- ^ The inner computation.
  -> Eff es (Either (E.CallStack, e) a)
tryError m = (Right <$> m) `catchError` \es e -> pure $ Left (es, e)

----------------------------------------
-- Orphan instance

-- | Instance included for compatibility with existing code.
instance
  ( Show e
  , Error e :> es
  , MTL.MonadError e (Eff es)
  ) => MTL.MonadError e (Eff es) where
  throwError = send . ThrowErrorWith show
  catchError action = send . CatchError action . const
