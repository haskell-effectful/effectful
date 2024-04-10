{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
-- | Convenience functions for the 'Labeled' 'Reader' effect.
--
-- @since 2.4.0.0
module Effectful.Labeled.Reader
  ( -- * Effect
    Reader(..)

    -- ** Handlers
  , runReader

    -- ** Operations
  , ask
  , asks
  , local
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Labeled
import Effectful.Reader.Dynamic (Reader(..))
import Effectful.Reader.Dynamic qualified as R

-- | Run the 'Reader' effect with the given initial environment (via
-- "Effectful.Reader.Static").
runReader
  :: forall label r es a
   . r
  -- ^ The initial environment.
  -> Eff (Labeled label (Reader r) : es) a
  -> Eff es a
runReader = runLabeled @label . R.runReader

----------------------------------------
-- Operations

-- | Fetch the value of the environment.
ask
  :: forall label r es
  . (HasCallStack, Labeled label (Reader r) :> es)
  => Eff es r
ask = send $ Labeled @label Ask

-- | Retrieve a function of the current environment.
--
-- @'asks' f ≡ f '<$>' 'ask'@
asks
  :: forall label r es a
   . (HasCallStack, Labeled label (Reader r) :> es)
  => (r -> a)
  -- ^ The function to apply to the environment.
  -> Eff es a
asks f = f <$> ask @label

-- | Execute a computation in a modified environment.
--
-- @'runReader' r ('local' f m) ≡ 'runReader' (f r) m@
--
local
  :: forall label r es a
   . (HasCallStack, Labeled label (Reader r) :> es)
  => (r -> r)
  -- ^ The function to modify the environment.
  -> Eff es a
  -> Eff es a
local f = send . Labeled @label . Local f
