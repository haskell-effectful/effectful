module Effectful.Error.Dynamic
  ( ErrorE(..)
  , runErrorE
  , throwError
  , catchError
  , tryError
  ) where

import Data.Typeable

import Effectful.Handler
import Effectful.Monad
import qualified Effectful.Error as E

data ErrorE e :: Effect where
  ThrowError :: e -> ErrorE e m a
  CatchError :: m a -> (E.CallStack -> e -> m a) -> ErrorE e m a

runErrorE
  :: Typeable e
  => Eff (ErrorE e : es) a
  -> Eff es (Either (E.CallStack, e) a)
runErrorE = reinterpret E.runErrorE $ \env -> \case
  ThrowError e   -> E.throwError e
  CatchError m h -> localSeqUnlift env $ \unlift -> do
    E.catchError (unlift m) (\cs -> unlift . h cs)

throwError
  :: (HasCallStack, ErrorE e :> es)
  => e
  -> Eff es a
throwError = send . ThrowError

catchError
  :: (HasCallStack, ErrorE e :> es)
  => Eff es a
  -> (E.CallStack -> e -> Eff es a)
  -> Eff es a
catchError m = send . CatchError m

tryError
  :: (HasCallStack, ErrorE e :> es)
  => Eff es a
  -> Eff es (Either (E.CallStack, e) a)
tryError m = (Right <$> m) `catchError` \es e -> pure $ Left (es, e)
