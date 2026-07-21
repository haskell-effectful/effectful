{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Convenience functions for the 'Labeled' 'ProviderList' effect.
--
-- @since 2.7.0.0
module Effectful.Labeled.Provider.List
  ( -- * Effect
    ProviderList(..)
  , ProviderList_

    -- ** Handlers
  , runProviderList
  , runProviderList_

    -- ** Operations
  , provideList
  , provideList_
  , provideListWith
  , provideListWith_

    -- * Re-exports
  , Labeled(..)
  , type (++)
  , KnownSubset
  ) where

import Data.Coerce
import Data.Functor.Identity

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Labeled
import Effectful.Provider.List (ProviderList(..), ProviderList_, type (++))
import Effectful.Provider.List qualified as P

-- | Run the labeled 'ProviderList' effect with a given handler.
runProviderList
  :: forall label providedEs input f es a
   . (HasCallStack, KnownSubset providedEs (providedEs ++ es))
  => (forall r. HasCallStack => input -> Eff (providedEs ++ es) r -> Eff es (f r))
  -- ^ The handler.
  -> Eff (Labeled label (ProviderList providedEs input f) : es) a
  -> Eff es a
runProviderList provider = runLabeled @label (P.runProviderList provider)

-- | Run the labeled 'ProviderList' effect with a given handler that doesn't
-- change its return type.
runProviderList_
  :: forall label providedEs input es a
   . (HasCallStack, KnownSubset providedEs (providedEs ++ es))
  => (forall r. HasCallStack => input -> Eff (providedEs ++ es) r -> Eff es r)
  -- ^ The handler.
  -> Eff (Labeled label (ProviderList_ providedEs input) : es) a
  -> Eff es a
runProviderList_ provider = runLabeled @label (P.runProviderList_ provider)

----------------------------------------
-- Operations

-- | Run the handler.
provideList
  :: forall label providedEs f es a
   . (HasCallStack, Labeled label (ProviderList providedEs () f) :> es)
  => Eff (providedEs ++ es) a
  -> Eff es (f a)
provideList = send . Labeled @label . P.ProvideListWith @providedEs ()

-- | Run the handler with unchanged return type.
provideList_
  :: forall label providedEs es a
   . (HasCallStack, Labeled label (ProviderList_ providedEs ()) :> es)
  => Eff (providedEs ++ es) a
  -> Eff es a
provideList_ = dropIdentity . send . Labeled @label . P.ProvideListWith @providedEs ()

-- | Run the handler with a given input.
provideListWith
  :: forall label providedEs input f es a
   . (HasCallStack, Labeled label (ProviderList providedEs input f) :> es)
  => input
  -- ^ The input to the handler.
  -> Eff (providedEs ++ es) a
  -> Eff es (f a)
provideListWith input = send . Labeled @label . P.ProvideListWith @providedEs input

-- | Run the handler that doesn't change its return type with a given input.
provideListWith_
  :: forall label providedEs input es a
   . (HasCallStack, Labeled label (ProviderList_ providedEs input) :> es)
  => input
  -- ^ The input to the handler.
  -> Eff (providedEs ++ es) a
  -> Eff es a
provideListWith_ input =
  dropIdentity . send . Labeled @label . P.ProvideListWith @providedEs input

----------------------------------------
-- Helpers

dropIdentity :: Eff es (Identity a) -> Eff es a
dropIdentity = coerce
