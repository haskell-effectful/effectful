-- | Provider of the t'Control.Applicative.Alternative' and
-- t'Control.Monad.MonadPlus' instance for 'Eff'.
module Effectful.NonDet
  ( -- * Effect
    NonDet(..)
  , OnEmptyPolicy(..)

    -- ** Handlers
  , runNonDet

  -- * Operations
  , emptyEff
  , plusEff
  , sumEff

    -- * Re-exports
  , Alternative(..)
  , HasCallStack
  , CallStack
  , getCallStack
  , prettyCallStack
  ) where

import Control.Applicative
import Data.Coerce
import GHC.Generics
import GHC.Stack

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import Effectful.Error.Static
import Effectful.Internal.Monad (LocalEnv(..), NonDet(..))

-- | Policy of dealing with modifications to __thread local__ state in the
-- environment in branches that end up calling the 'Empty' operation.
--
-- /Note:/ 'OnEmptyKeep' is significantly faster as there is no need to back up
-- the environment on each call to ':<|>:'.
--
-- @since 2.2.0.0
data OnEmptyPolicy
  = OnEmptyKeep
  -- ^ Keep modifications on 'Empty'.
  | OnEmptyRollback
  -- ^ Rollback modifications on 'Empty'.
  --
  -- /Note:/ state modifications are rolled back on 'Empty' only. In particular,
  -- they are __not__ rolled back on exceptions.
  deriving stock (Eq, Generic, Ord, Show)

-- | Run the 'NonDet' effect with a given 'OnEmptyPolicy'.
--
-- /Note:/ ':<|>:' executes the second computation if (and only if) the first
-- computation calls 'Empty'.
--
-- @since 2.2.0.0
runNonDet
  :: HasCallStack
  => OnEmptyPolicy
  -> Eff (NonDet : es) a
  -> Eff es (Either CallStack a)
runNonDet = \case
  OnEmptyKeep     -> runNonDetKeep
  OnEmptyRollback -> runNonDetRollback

runNonDetKeep
  :: HasCallStack
  => Eff (NonDet : es) a
  -> Eff es (Either CallStack a)
runNonDetKeep = reinterpret (fmap noError . runError @ErrorEmpty) $ \env -> \case
  Empty       -> throwError ErrorEmpty
  m1 :<|>: m2 -> localSeqUnlift env $ \unlift -> do
    mr <- (Just <$> unlift m1) `catchError` \_ ErrorEmpty -> pure Nothing
    case mr of
      Just r  -> pure r
      Nothing -> unlift m2

runNonDetRollback
  :: HasCallStack
  => Eff (NonDet : es) a
  -> Eff es (Either CallStack a)
runNonDetRollback = reinterpret setup $ \env -> \case
  Empty       -> throwError ErrorEmpty
  m1 :<|>: m2 -> do
    backupEnv <- cloneLocalEnv env
    localSeqUnlift env $ \unlift -> do
      mr <- (Just <$> unlift m1) `catchError` \_ ErrorEmpty -> do
        -- If m1 failed, roll back the environment.
        restoreLocalEnv env backupEnv
        pure Nothing
      case mr of
        Just r  -> pure r
        Nothing -> unlift m2
  where
    setup action = do
      backupEs <- unsafeEff cloneEnv
      runError @ErrorEmpty action >>= \case
        Right r -> pure $ Right r
        Left (cs, _) -> do
          -- If the whole action failed, roll back the environment.
          unsafeEff $ \es -> restoreEnv es backupEs
          pure $ Left cs

----------------------------------------

-- | Specialized version of 'empty' with the 'HasCallStack' constraint for
-- tracking purposes.
--
-- @since 2.2.0.0
emptyEff :: (HasCallStack, NonDet :> es) => Eff es a
emptyEff = send Empty

-- | Specialized version of '<|>' with the `HasCallStack` constraint for
-- tracking purposes.
--
-- @since 2.5.0.0
plusEff :: (HasCallStack, NonDet :> es) => Eff es a -> Eff es a -> Eff es a
plusEff m1 m2 = send (m1 :<|>: m2)
infixl 3 `plusEff` -- same as <|>

-- | Specialized version of 'asum' with the 'HasCallStack' constraint for
-- tracking purposes.
--
-- @since 2.2.0.0
sumEff :: (HasCallStack, Foldable t, NonDet :> es) => t (Eff es a) -> Eff es a
sumEff = foldr plusEff emptyEff

----------------------------------------
-- Internal helpers

-- | Internal error type for the Empty action. Better than '()' in case it
-- escapes the scope of 'runNonDet' and shows up in error messages.
data ErrorEmpty = ErrorEmpty
instance Show ErrorEmpty where
  show ErrorEmpty = "Effectful.NonDet.ErrorEmpty"

noError :: Either (cs, e) a -> Either cs a
noError = either (Left . fst) Right

cloneLocalEnv
  :: HasCallStack
  => LocalEnv localEs handlerEs
  -> Eff es (LocalEnv localEs handlerEs)
cloneLocalEnv = coerce . unsafeEff_ . cloneEnv . coerce

restoreLocalEnv
  :: HasCallStack
  => LocalEnv localEs handlerEs
  -> LocalEnv localEs handlerEs
  -> Eff es ()
restoreLocalEnv dest src = unsafeEff_ $ restoreEnv (coerce dest) (coerce src)
