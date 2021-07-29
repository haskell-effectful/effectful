module Effectful.Writer.Dynamic
  ( WriterE(..)

  -- * Local
  , runLocalWriter
  , execLocalWriter

  -- * Shared
  , runSharedWriter
  , execSharedWriter

  -- * Operations
  , tell
  , listen
  , listens
  ) where

import Effectful.Handler
import Effectful.Monad
import qualified Effectful.Writer.Local as L
import qualified Effectful.Writer.Shared as S

data WriterE w :: Effect where
  Tell   :: w   -> WriterE w m ()
  Listen :: m a -> WriterE w m (a, w)

----------------------------------------
-- Local

runLocalWriter :: Monoid w => Eff (WriterE w : es) a -> Eff es (a, w)
runLocalWriter = reinterpret L.runWriterE localWriter

execLocalWriter :: Monoid w => Eff (WriterE w : es) a -> Eff es w
execLocalWriter = reinterpret L.execWriterE localWriter

localWriter
  :: (L.WriterE w :> es, Monoid w)
  => LocalEnv localEs
  -> WriterE w (Eff localEs) a
  -> Eff es a
localWriter env = \case
  Tell w   -> L.tell w
  Listen m -> localSeqUnlift env $ \unlift -> L.listen (unlift m)

----------------------------------------
-- Shared

runSharedWriter :: Monoid w => Eff (WriterE w : es) a -> Eff es (a, w)
runSharedWriter = reinterpret S.runWriterE sharedWriter

execSharedWriter :: Monoid w => Eff (WriterE w : es) a -> Eff es w
execSharedWriter = reinterpret S.execWriterE sharedWriter

sharedWriter
  :: (S.WriterE w :> es, Monoid w)
  => LocalEnv localEs
  -> WriterE w (Eff localEs) a
  -> Eff es a
sharedWriter env = \case
  Tell w    -> S.tell w
  Listen m  -> localSeqUnlift env $ \unlift -> S.listen (unlift m)

----------------------------------------
-- Operations

tell
  :: (HasCallStack, WriterE w :> es, Monoid w)
  => w
  -> Eff es ()
tell = send . Tell

listen
  :: (HasCallStack, WriterE w :> es, Monoid w)
  => Eff es a
  -> Eff es (a, w)
listen = send . Listen

listens
  :: (HasCallStack, WriterE w :> es, Monoid w)
  => (w -> b)
  -> Eff es a
  -> Eff es (a, b)
listens f m = do
  (a, w) <- listen m
  pure (a, f w)
