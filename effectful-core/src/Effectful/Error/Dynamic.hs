-- | The dynamically dispatched variant of the 'Error' effect.
--
-- /Note:/ unless you plan to change interpretations at runtime, it's
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
import Effectful.Error.Static qualified as E

-- | Provide the ability to handle errors of type @e@.
data Error e :: Effect where
  ThrowError :: e -> Error e m a
  CatchError :: m a -> (E.CallStack -> e -> m a) -> Error e m a

type instance DispatchOf (Error e) = Dynamic

-- | Handle errors of type @e@ (via "Effectful.Error.Static").
runError
  :: Eff (Error e : es) a
  -> Eff es (Either (E.CallStack, e) a)
runError = reinterpret E.runError $ \env -> \case
  ThrowError e   -> E.throwError e
  CatchError m h -> localSeqUnlift env $ \unlift -> do
    E.catchError (unlift m) (\cs -> unlift . h cs)

-- | Handle errors of type @e@ (via "Effectful.Error.Static") with a specific
-- error handler.
--
-- @since 2.3.0.0
runErrorWith
  :: (E.CallStack -> e -> Eff es a)
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
  :: Eff (Error e : es) a
  -> Eff es (Either e a)
runErrorNoCallStack = fmap (either (Left . snd) Right) . runError

-- | Handle errors of type @e@ (via "Effectful.Error.Static") with a specific
-- error handler. In case of an error discard the 'CallStack'.
runErrorNoCallStackWith
  :: (e -> Eff es a)
  -- ^ The error handler.
  -> Eff (Error e : es) a
  -> Eff es a
runErrorNoCallStackWith handler m = runErrorNoCallStack m >>= \case
  Left e -> handler e
  Right a -> pure a

-- | Throw an error of type @e@.
throwError
  :: (HasCallStack, Error e :> es)
  => e
  -- ^ The error.
  -> Eff es a
throwError e = withFrozenCallStack $ send (ThrowError e)

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
  :: Error e :> es
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
