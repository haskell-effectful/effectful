module Effectful.Reader.Dynamic
  ( ReaderE(..)
  , runReaderE
  , ask
  , asks
  , local
  ) where

import Effectful.Handler
import Effectful.Monad
import qualified Effectful.Reader as R

data ReaderE r :: Effect where
  Ask   :: ReaderE r m r
  Local :: (r -> r) -> m a -> ReaderE r m a

runReaderE :: r -> Eff (ReaderE r : es) a -> Eff es a
runReaderE r = reinterpret (R.runReaderE r) $ \env -> \case
  Ask       -> R.ask
  Local f m -> localSeqUnlift env $ \unlift -> R.local f (unlift m)

----------------------------------------
-- Operations

ask :: (HasCallStack, ReaderE r :> es) => Eff es r
ask = send Ask

asks :: (HasCallStack, ReaderE r :> es) => (r -> a) -> Eff es a
asks f = f <$> ask

local :: (HasCallStack, ReaderE r :> es ) => (r -> r) -> Eff es a -> Eff es a
local f = send . Local f
