{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Convenience functions for the 'Labeled' 'Provider' effect.
--
-- @since 2.7.0.0
module Effectful.Labeled.Provider
  ( -- * Effect
    Provider(..)
  , Provider_

    -- ** Handlers
  , runProvider
  , runProvider_

    -- ** Operations
  , provide
  , provide_
  , provideWith
  , provideWith_

    -- * Re-exports
  , Labeled(..)
  ) where

import Data.Coerce
import Data.Functor.Identity

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Labeled
import Effectful.Provider (Provider(..), Provider_)
import Effectful.Provider qualified as P

-- | Run the labeled 'Provider' effect with a given effect handler.
runProvider
  :: forall label e input f es a
   . HasCallStack
  => (forall r. HasCallStack => input -> Eff (e : es) r -> Eff es (f r))
  -- ^ The effect handler.
  -> Eff (Labeled label (Provider e input f) : es) a
  -> Eff es a
runProvider provider = runLabeled @label (P.runProvider provider)

-- | Run the labeled 'Provider' effect with a given effect handler that doesn't
-- change its return type.
runProvider_
  :: forall label e input es a
   . HasCallStack
  => (forall r. HasCallStack => input -> Eff (e : es) r -> Eff es r)
  -- ^ The effect handler.
  -> Eff (Labeled label (Provider_ e input) : es) a
  -> Eff es a
runProvider_ provider = runLabeled @label (P.runProvider_ provider)

----------------------------------------
-- Operations

-- | Run the effect handler.
provide
  :: forall label e f es a
   . (HasCallStack, Labeled label (Provider e () f) :> es)
  => Eff (e : es) a
  -> Eff es (f a)
provide = send . Labeled @label . P.ProvideWith ()

-- | Run the effect handler with unchanged return type.
provide_
  :: forall label e es a
   . (HasCallStack, Labeled label (Provider_ e ()) :> es)
  => Eff (e : es) a
  -> Eff es a
provide_ = dropIdentity . send . Labeled @label . P.ProvideWith ()

-- | Run the effect handler with a given input.
provideWith
  :: forall label e input f es a
   . (HasCallStack, Labeled label (Provider e input f) :> es)
  => input
  -- ^ The input to the effect handler.
  -> Eff (e : es) a
  -> Eff es (f a)
provideWith input = send . Labeled @label . P.ProvideWith input

-- | Run the effect handler that doesn't change its return type with a given
-- input.
provideWith_
  :: forall label e input es a
   . (HasCallStack, Labeled label (Provider_ e input) :> es)
  => input
  -- ^ The input to the effect handler.
  -> Eff (e : es) a
  -> Eff es a
provideWith_ input = dropIdentity . send . Labeled @label . P.ProvideWith input

----------------------------------------
-- Helpers

dropIdentity :: Eff es (Identity a) -> Eff es a
dropIdentity = coerce
