{-# OPTIONS_GHC -Wno-orphans #-}
-- | Definitions and instances for MTL compatibility.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Effectful.Internal.MTL where

import Control.Monad.Except qualified as MTL
import Control.Monad.Reader qualified as MTL
import Control.Monad.State qualified as MTL
import Control.Monad.Writer qualified as MTL
import GHC.Stack (CallStack)

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

-- | Provide the ability to handle errors of type @e@.
data Error e :: Effect where
  -- | @since 2.4.0.0
  ThrowErrorWith :: (e -> String) -> e -> Error e m a
  CatchError :: m a -> (CallStack -> e -> m a) -> Error e m a

type instance DispatchOf (Error e) = Dynamic

-- | Instance included for compatibility with existing code.
instance
  ( Show e
  , Error e :> es
  , MTL.MonadError e (Eff es)
  ) => MTL.MonadError e (Eff es) where
  throwError = send . ThrowErrorWith show
  catchError action = send . CatchError action . const

----------------------------------------

data Reader r :: Effect where
  Ask   :: Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a

type instance DispatchOf (Reader r) = Dynamic

-- | Instance included for compatibility with existing code.
instance
  ( Reader r :> es
  , MTL.MonadReader r (Eff es)
  ) => MTL.MonadReader r (Eff es) where
  ask = send Ask
  local f = send . Local f
  reader f = f <$> send Ask

----------------------------------------

-- | Provide access to a mutable value of type @s@.
data State s :: Effect where
  Get    :: State s m s
  Put    :: s -> State s m ()
  State  :: (s ->   (a, s)) -> State s m a
  StateM :: (s -> m (a, s)) -> State s m a

type instance DispatchOf (State s) = Dynamic

-- | Instance included for compatibility with existing code.
instance
  ( State s :> es
  , MTL.MonadState s (Eff es)
  ) => MTL.MonadState s (Eff es) where
  get = send Get
  put = send . Put
  state = send . State

----------------------------------------

-- | Provide access to a write only value of type @w@.
data Writer w :: Effect where
  Tell   :: w   -> Writer w m ()
  Listen :: m a -> Writer w m (a, w)

type instance DispatchOf (Writer w) = Dynamic

-- | Instance included for compatibility with existing code.
instance
  ( Monoid w
  , Writer w :> es
  , MTL.MonadWriter w (Eff es)
  ) => MTL.MonadWriter w (Eff es) where
  writer (a, w) = a <$ send (Tell w)
  tell = send . Tell
  listen = send . Listen
  pass = error "pass is not implemented due to ambiguous semantics in presence of runtime exceptions"
