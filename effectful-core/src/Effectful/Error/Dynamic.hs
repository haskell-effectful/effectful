module Effectful.Error.Dynamic
  ( Error(..)
  , runError
  , throwError
  , catchError
  , tryError
  ) where

import Data.Typeable

import Effectful.Dispatch.Dynamic
import Effectful.Monad
import qualified Effectful.Error as E

data Error e :: Effect where
  ThrowError :: e -> Error e m a
  CatchError :: m a -> (E.CallStack -> e -> m a) -> Error e m a

type instance EffectStyle (Error e) = DynamicEffect

runError
  :: Typeable e
  => Eff (Error e : es) a
  -> Eff es (Either (E.CallStack, e) a)
runError = reinterpret E.runError $ \env -> \case
  ThrowError e   -> E.throwError e
  CatchError m h -> localSeqUnlift env $ \unlift -> do
    E.catchError (unlift m) (\cs -> unlift . h cs)

throwError
  :: (HasCallStack, Error e :> es)
  => e
  -> Eff es a
throwError = send . ThrowError

catchError
  :: (HasCallStack, Error e :> es)
  => Eff es a
  -> (E.CallStack -> e -> Eff es a)
  -> Eff es a
catchError m = send . CatchError m

tryError
  :: (HasCallStack, Error e :> es)
  => Eff es a
  -> Eff es (Either (E.CallStack, e) a)
tryError m = (Right <$> m) `catchError` \es e -> pure $ Left (es, e)
