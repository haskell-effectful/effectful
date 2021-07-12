module Effectful.Error.Dynamic
  ( Error(..)
  , runError
  , throwError
  , catchError
  , tryError
  ) where

import Data.Typeable
import GHC.Stack

import Effectful.Handler
import Effectful.Monad
import qualified Effectful.Error as E

data Error e :: Effect where
  ThrowError :: e -> Error e m a
  CatchError :: m a -> (CallStack -> e -> m a) -> Error e m a

runError
  :: Typeable e
  => Eff (Error e : es) a
  -> Eff es (Either (CallStack, e) a)
runError = reinterpretM E.runError $ \env -> \case
  ThrowError e   -> E.throwError e
  CatchError m h -> localSeqUnlift env $ \run -> do
    E.catchError (run m) (\cs -> run . h cs)

throwError
  :: (HasCallStack, Error e :> es)
  => e
  -> Eff es a
throwError = send . ThrowError

catchError
  :: Error e :> es
  => Eff es a
  -> (CallStack -> e -> Eff es a)
  -> Eff es a
catchError m = send . CatchError m

tryError
  :: Error e :> es
  => Eff es a
  -> Eff es (Either (CallStack, e) a)
tryError m = (Right <$> m) `catchError` \es e -> pure $ Left (es, e)
