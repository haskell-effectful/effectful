{-# LANGUAGE UndecidableInstances #-}
module Effectful.Class.Reader
  ( MonadReader(..)
  , asks
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Control

import Effectful.Internal.Has
import Effectful.Internal.Monad
import qualified Effectful.Reader as R

-- | Compatiblity layer for a transition period from MTL-style effect handling
-- to 'Effective.Eff'.
class Monad m => MonadReader r m where
  {-# MINIMAL (ask | reader), local #-}
  ask :: m r
  ask = reader id

  local :: (r -> r) -> m a -> m a

  reader :: (r -> a) -> m a
  reader f = ask >>= pure . f

-- | Generic, overlappable instance.
instance {-# OVERLAPPABLE #-}
  ( MonadReader r m
  , MonadTransControl t
  , Monad (t m)
  ) => MonadReader r (t m) where
  ask       = lift ask
  local f m = liftWith (\run -> local f (run m)) >>= restoreT . pure
  reader    = lift . reader

instance R.Reader r :> es => MonadReader r (Eff es) where
  ask    = R.ask
  local  = R.local
  reader = R.reader

asks :: MonadReader r m => (r -> a) -> m a
asks = reader
