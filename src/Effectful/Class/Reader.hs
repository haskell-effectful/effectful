{-# LANGUAGE UndecidableInstances #-}
module Effectful.Class.Reader
  ( MonadReader(..)
  , asks
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Control

import Effectful.Internal.Effect
import Effectful.Internal.Monad
import qualified Effectful.Reader as R

-- | Compatiblity layer for a transition period from MTL-style effect handling
-- to 'Effectful.Eff'.
class Monad m => MonadReader r m where
  ask :: m r
  local :: (r -> r) -> m a -> m a

-- | Generic, overlappable instance.
instance {-# OVERLAPPABLE #-}
  ( MonadReader r m
  , MonadTransControl t
  , Monad (t m)
  ) => MonadReader r (t m) where
  ask       = lift ask
  local f m = liftWith (\run -> local f (run m)) >>= restoreT . pure

instance R.Reader r :> es => MonadReader r (Eff es) where
  ask    = R.ask
  local  = R.local

asks :: MonadReader r m => (r -> a) -> m a
asks f = f <$> ask
