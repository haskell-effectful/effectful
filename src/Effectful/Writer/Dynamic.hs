module Effectful.Writer.Dynamic
  ( Writer(..)

  -- * Pure
  , runWriter
  , execWriter

  -- * MVar
  , runWriterMVar
  , execWriterMVar

  -- * Operations
  , tell
  , listen
  , listens
  ) where

import Effectful.Internal.Effect
import Effectful.Internal.Monad
import Effectful.Interpreter

import qualified Effectful.Writer as WP
import qualified Effectful.Writer.MVar as WM

data Writer w :: Effect where
  Tell   :: ~w  -> Writer w m ()
  Listen :: m a -> Writer w m (a, w)

----------------------------------------
-- Pure

runWriter :: Monoid w => Eff (Writer w : es) a -> Eff es (a, w)
runWriter = reinterpretM WP.runWriter writerPure

execWriter :: Monoid w => Eff (Writer w : es) a -> Eff es w
execWriter = reinterpretM WP.execWriter writerPure

writerPure
  :: (WP.Writer w :> localEs, Monoid w)
  => RunIn es (Eff localEs)
  -> Writer w (Eff es) a
  -> Eff localEs a
writerPure run = \case
  Tell w    -> WP.tell w
  Listen m  -> WP.listen (run m)

----------------------------------------
-- MVar

runWriterMVar :: Monoid w => Eff (Writer w : es) a -> Eff es (a, w)
runWriterMVar = reinterpretM WM.runWriter writerMVar

execWriterMVar :: Monoid w => Eff (Writer w : es) a -> Eff es w
execWriterMVar = reinterpretM WM.execWriter writerMVar

writerMVar
  :: (WM.Writer w :> localEs, Monoid w)
  => RunIn es (Eff localEs)
  -> Writer w (Eff es) a
  -> Eff localEs a
writerMVar run = \case
  Tell w    -> WM.tell w
  Listen m  -> WM.listen (run m)

----------------------------------------
-- Operations

tell :: (Writer w :> es, Monoid w) => w -> Eff es ()
tell = send . Tell

listen :: (Writer w :> es, Monoid w) => Eff es a -> Eff es (a, w)
listen = send . Listen

listens :: (Writer w :> es, Monoid w) => (w -> b) -> Eff es a -> Eff es (a, b)
listens f m = do
  (a, w) <- listen m
  pure (a, f w)
