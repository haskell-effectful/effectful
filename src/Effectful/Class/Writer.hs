{-# LANGUAGE UndecidableInstances #-}
module Effectful.Class.Writer where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Control

import Effectful.Internal.Effect
import Effectful.Internal.Monad
import qualified Effectful.Writer.Dynamic as W

-- | Compatiblity layer for a transition period from MTL-style effect handling
-- to 'Effectful.Eff'.
class Monad m => MonadWriter w m where
  {-# MINIMAL (writer | tell), listen #-}

  writer :: (a, w) -> m a
  writer (a, w) = do
    tell w
    pure a

  tell :: w -> m ()
  tell w = writer ((), w)

  listen :: m a -> m (a, w)

-- | Generic, overlappable instance.
instance {-# OVERLAPPABLE #-}
  ( MonadWriter w m
  , MonadTransControl t
  , Monad (t m)
  ) => MonadWriter w (t m) where
  writer   = lift . writer
  tell     = lift . tell
  listen m = do
    (stT, w) <- liftWith $ \run -> listen (run m)
    (, w) <$> restoreT (pure stT)

instance (W.Writer w :> es, Monoid w) => MonadWriter w (Eff es) where
  writer = W.writer
  tell   = W.tell
  listen = W.listen

listens :: MonadWriter w m => (w -> b) -> m a -> m (a, b)
listens f m = do
  (a, w) <- listen m
  pure (a, f w)
