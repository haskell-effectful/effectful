{-# OPTIONS_GHC -Wno-orphans #-}
-- | The dynamically dispatched variant of the 'Writer' effect.
--
-- /Note:/ unless you plan to change interpretations at runtime or you need the
-- 'MTL.MonadWriter' instance for compatibility with existing code, it's
-- recommended to use one of the statically dispatched variants,
-- i.e. "Effectful.Writer.Static.Local" or "Effectful.Writer.Static.Shared".
module Effectful.Writer.Dynamic
  ( -- * Effect
    Writer(..)

    -- ** Handlers

    -- *** Local
  , runWriterLocal
  , execWriterLocal

    -- *** Shared
  , runWriterShared
  , execWriterShared

    -- * Operations
  , tell
  , listen
  , listens
  ) where

import Control.Monad.Writer qualified as MTL

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Writer.Static.Local qualified as L
import Effectful.Writer.Static.Shared qualified as S

-- | Provide access to a write only value of type @w@.
data Writer w :: Effect where
  Tell   :: w   -> Writer w m ()
  Listen :: m a -> Writer w m (a, w)

type instance DispatchOf (Writer w) = Dynamic

----------------------------------------
-- Local

-- | Run the 'Writer' effect and return the final value along with the final
-- output (via "Effectful.Writer.Static.Local").
runWriterLocal :: (HasCallStack, Monoid w) => Eff (Writer w : es) a -> Eff es (a, w)
runWriterLocal = reinterpret L.runWriter localWriter

-- | Run a 'Writer' effect and return the final output, discarding the final
-- value (via "Effectful.Writer.Static.Local").
execWriterLocal :: (HasCallStack, Monoid w) => Eff (Writer w : es) a -> Eff es w
execWriterLocal = reinterpret L.execWriter localWriter

localWriter :: (L.Writer w :> es, Monoid w) => EffectHandler (Writer w) es
localWriter env = \case
  Tell w   -> L.tell w
  Listen m -> localSeqUnlift env $ \unlift -> L.listen (unlift m)

----------------------------------------
-- Shared

-- | Run the 'Writer' effect and return the final value along with the final
-- output (via "Effectful.Writer.Static.Shared").
runWriterShared :: (HasCallStack, Monoid w) => Eff (Writer w : es) a -> Eff es (a, w)
runWriterShared = reinterpret S.runWriter sharedWriter

-- | Run the 'Writer' effect and return the final output, discarding the final
-- value (via "Effectful.Writer.Static.Shared").
execWriterShared :: (HasCallStack, Monoid w) => Eff (Writer w : es) a -> Eff es w
execWriterShared = reinterpret S.execWriter sharedWriter

sharedWriter :: (S.Writer w :> es, Monoid w) => EffectHandler (Writer w) es
sharedWriter env = \case
  Tell w    -> S.tell w
  Listen m  -> localSeqUnlift env $ \unlift -> S.listen (unlift m)

----------------------------------------
-- Operations

-- | Append the given output to the overall output of the 'Writer'.
tell
  :: (HasCallStack, Writer w :> es)
  => w
  -> Eff es ()
tell = send . Tell

-- | Execute an action and append its output to the overall output of the
-- 'Writer'.
listen
  :: (HasCallStack, Writer w :> es)
  => Eff es a
  -> Eff es (a, w)
listen = send . Listen

-- | Execute an action and append its output to the overall output of the
-- 'Writer', then return the final value along with a function of the recorded
-- output.
--
-- @'listens' f m ≡ 'Data.Bifunctor.second' f '<$>' 'listen' m@
listens
  :: (HasCallStack, Writer w :> es)
  => (w -> b)
  -> Eff es a
  -> Eff es (a, b)
listens f m = do
  (a, w) <- listen m
  pure (a, f w)

----------------------------------------
-- Orphan instance

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
