module Effectful.Writer.Dynamic
  ( Writer(..)

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

data Writer w :: Effect where
  Tell   :: ~w  -> Writer w m ()
  Listen :: m a -> Writer w m (a, w)

----------------------------------------
-- Local

runLocalWriter :: Monoid w => Eff (Writer w : es) a -> Eff es (a, w)
runLocalWriter = reinterpretM L.runWriter localWriter

execLocalWriter :: Monoid w => Eff (Writer w : es) a -> Eff es w
execLocalWriter = reinterpretM L.execWriter localWriter

localWriter
  :: (L.Writer w :> es, Monoid w)
  => LocalEnv localEs
  -> Writer w (Eff localEs) a
  -> Eff es a
localWriter env = \case
  Tell w   -> L.tell w
  Listen m -> localSeqUnlift env $ \run -> L.listen (run m)

----------------------------------------
-- Shared

runSharedWriter :: Monoid w => Eff (Writer w : es) a -> Eff es (a, w)
runSharedWriter = reinterpretM S.runWriter sharedWriter

execSharedWriter :: Monoid w => Eff (Writer w : es) a -> Eff es w
execSharedWriter = reinterpretM S.execWriter sharedWriter

sharedWriter
  :: (S.Writer w :> es, Monoid w)
  => LocalEnv localEs
  -> Writer w (Eff localEs) a
  -> Eff es a
sharedWriter env = \case
  Tell w    -> S.tell w
  Listen m  -> localSeqUnlift env $ \run -> S.listen (run m)

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
