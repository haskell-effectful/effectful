{-# LANGUAGE UndecidableInstances #-}
module Effectful.Class.State where

import Control.Monad.Trans.Class

import Effectful.Internal.Effect
import Effectful.Internal.Monad
import qualified Effectful.State as S

-- | Compatiblity layer for a transition period from MTL-style effect handling
-- to 'Effectful.Eff'.
class Monad m => MonadState s m where
  {-# MINIMAL state | get, put #-}

  get :: m s
  get = state (\s -> (s, s))

  put :: s -> m ()
  put s = state (\_ -> ((), s))

  state :: (s -> (a, s)) -> m a
  state f = do
    (a, s) <- f <$> get
    s `seq` put s
    pure a

-- | Generic, overlappable instance.
instance {-# OVERLAPPABLE #-}
  ( MonadState s m
  , MonadTrans t
  , Monad (t m)
  ) => MonadState s (t m) where
  get   = lift get
  put   = lift . put
  state = lift . state

instance S.State s :> es => MonadState s (Eff es) where
  get   = S.get
  put   = S.put
  state = S.state

gets :: MonadState s m => (s -> a) -> m a
gets f = f <$> get

modify :: MonadState s m => (s -> s) -> m ()
modify f = state (\s -> ((), f s))
