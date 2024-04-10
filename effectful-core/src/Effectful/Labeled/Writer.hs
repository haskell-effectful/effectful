{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
-- | Convenience functions for the 'Labeled' 'Writer' effect.
--
-- @since 2.4.0.0
module Effectful.Labeled.Writer
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

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Labeled
import Effectful.Writer.Dynamic (Writer(..))
import Effectful.Writer.Dynamic qualified as W

----------------------------------------
-- Local

-- | Run the 'Writer' effect and return the final value along with the final
-- output (via "Effectful.Writer.Static.Local").
runWriterLocal
  :: forall label w es a
   . Monoid w
  => Eff (Labeled label (Writer w) : es)
  a -> Eff es (a, w)
runWriterLocal = runLabeled @label W.runWriterLocal

-- | Run a 'Writer' effect and return the final output, discarding the final
-- value (via "Effectful.Writer.Static.Local").
execWriterLocal
  :: forall label w es a
   . Monoid w
  => Eff (Labeled label (Writer w) : es) a
  -> Eff es w
execWriterLocal = runLabeled @label W.execWriterLocal

----------------------------------------
-- Shared

-- | Run the 'Writer' effect and return the final value along with the final
-- output (via "Effectful.Writer.Static.Shared").
runWriterShared
  :: forall label w es a
   . Monoid w
  => Eff (Labeled label (Writer w) : es) a
  -> Eff es (a, w)
runWriterShared = runLabeled @label W.runWriterShared

-- | Run the 'Writer' effect and return the final output, discarding the final
-- value (via "Effectful.Writer.Static.Shared").
execWriterShared
  :: forall label w es a
   . Monoid w
  => Eff (Labeled label (Writer w) : es) a
  -> Eff es w
execWriterShared = runLabeled @label W.execWriterShared

----------------------------------------
-- Operations

-- | Append the given output to the overall output of the 'Writer'.
tell
  :: forall label w es
   . (HasCallStack, Labeled label (Writer w) :> es)
  => w
  -> Eff es ()
tell = send . Labeled @label . Tell

-- | Execute an action and append its output to the overall output of the
-- 'Writer'.
listen
  :: forall label w es a
   . (HasCallStack, Labeled label (Writer w) :> es)
  => Eff es a
  -> Eff es (a, w)
listen = send . Labeled @label . Listen

-- | Execute an action and append its output to the overall output of the
-- 'Writer', then return the final value along with a function of the recorded
-- output.
--
-- @'listens' f m ≡ 'Data.Bifunctor.second' f '<$>' 'listen' m@
listens
  :: forall label w es a b
   . (HasCallStack, Labeled label (Writer w) :> es)
  => (w -> b)
  -> Eff es a
  -> Eff es (a, b)
listens f m = do
  (a, w) <- listen @label m
  pure (a, f w)
