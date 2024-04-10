{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RequiredTypeArguments #-}
-- | Convenience functions for the 'Labeled' 'Writer' effect with visible
-- dependent quantification.
--
-- Requires GHC >= 9.10.
--
-- @since 2.4.0.0
module Effectful.Labeled.Writer.VDQ
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
import Effectful.Labeled.VDQ
import Effectful.Writer.Dynamic (Writer(..))
import Effectful.Writer.Dynamic qualified as W

----------------------------------------
-- Local

-- | Run the 'Writer' effect and return the final value along with the final
-- output (via "Effectful.Writer.Static.Local").
runWriterLocal
  :: forall label
  -> Monoid w
  => Eff (Labeled label (Writer w) : es)
  a -> Eff es (a, w)
runWriterLocal label = runLabeled label W.runWriterLocal

-- | Run a 'Writer' effect and return the final output, discarding the final
-- value (via "Effectful.Writer.Static.Local").
execWriterLocal
  :: forall label
  -> Monoid w
  => Eff (Labeled label (Writer w) : es) a
  -> Eff es w
execWriterLocal label = runLabeled label W.execWriterLocal

----------------------------------------
-- Shared

-- | Run the 'Writer' effect and return the final value along with the final
-- output (via "Effectful.Writer.Static.Shared").
runWriterShared
  :: forall label
  -> Monoid w
  => Eff (Labeled label (Writer w) : es) a
  -> Eff es (a, w)
runWriterShared label = runLabeled label W.runWriterShared

-- | Run the 'Writer' effect and return the final output, discarding the final
-- value (via "Effectful.Writer.Static.Shared").
execWriterShared
  :: forall label
  -> Monoid w
  => Eff (Labeled label (Writer w) : es) a
  -> Eff es w
execWriterShared label = runLabeled label W.execWriterShared

----------------------------------------
-- Operations

-- | Append the given output to the overall output of the 'Writer'.
tell
  :: forall label
  -> (HasCallStack, Labeled label (Writer w) :> es)
  => w
  -> Eff es ()
tell label = send . Labeled @label . Tell

-- | Execute an action and append its output to the overall output of the
-- 'Writer'.
listen
  :: forall label
  -> (HasCallStack, Labeled label (Writer w) :> es)
  => Eff es a
  -> Eff es (a, w)
listen label = send . Labeled @label . Listen

-- | Execute an action and append its output to the overall output of the
-- 'Writer', then return the final value along with a function of the recorded
-- output.
--
-- @'listens' label f m ≡ 'Data.Bifunctor.second' f '<$>' 'listen' label m@
listens
  :: forall label
  -> (HasCallStack, Labeled label (Writer w) :> es)
  => (w -> b)
  -> Eff es a
  -> Eff es (a, b)
listens label f m = do
  (a, w) <- listen label m
  pure (a, f w)
