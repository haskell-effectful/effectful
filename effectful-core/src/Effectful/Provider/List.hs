{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Turn a handler of multiple effects into an effectful operation.
--
-- Generalizes "Effectful.Provider".
--
-- @since 2.3.1.0
module Effectful.Provider.List
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

    -- * Misc
  , type (++)
  , KnownSubset
  ) where

import Data.Coerce
import Data.Functor.Identity
import GHC.Stack

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Internal.Effect

-- | Provide a way to run a handler of multiple @providedEs@ with a given
-- @input@.
--
-- /Note:/ @f@ can be used to alter the return type of the handler. If that's
-- unnecessary, use 'ProviderList_'.
data ProviderList (providedEs :: [Effect]) (input :: Type) (f :: Type -> Type) :: Effect where
  -- | Run the effect handlers with a given input.
  --
  -- @since 2.7.0.0
  ProvideListWith
    :: forall providedEs input f es a
     . input
    -> Eff (providedEs ++ es) a
    -> ProviderList providedEs input f (Eff es) (f a)

-- | A restricted variant of 'ProviderList' with unchanged return type of the
-- handler.
type ProviderList_ providedEs input = ProviderList providedEs input Identity

type instance DispatchOf (ProviderList providedEs input f) = Dynamic

-- | Run the 'ProviderList' effect with a given handler.
runProviderList
  :: forall providedEs input f es a
   . (HasCallStack, KnownSubset providedEs (providedEs ++ es))
  => (forall r. HasCallStack => input -> Eff (providedEs ++ es) r -> Eff es (f r))
  -- ^ The handler.
  -> Eff (ProviderList providedEs input f : es) a
  -> Eff es a
runProviderList provider = interpret $ \env -> \case
  ProvideListWith input action -> provider input $ do
    localSeqUnlift env $ \unlift -> do
      localSeqLend @providedEs env $ \lend -> do
        unlift . lend $ action

-- | Run the 'ProviderList' effect with a given handler that doesn't change its
-- return type.
runProviderList_
  :: forall providedEs input es a
   . (HasCallStack, KnownSubset providedEs (providedEs ++ es))
  => (forall r. HasCallStack => input -> Eff (providedEs ++ es) r -> Eff es r)
  -- ^ The handler.
  -> Eff (ProviderList_ providedEs input : es) a
  -> Eff es a
runProviderList_ provider = interpret $ \env -> \case
  ProvideListWith input action -> provider input $ do
    localSeqUnlift env $ \unlift -> do
      localSeqLend @providedEs env $ \lend -> do
        unlift . lend $ coerce action

-- | Run the handler.
provideList
  :: forall providedEs f es a
   . (HasCallStack, ProviderList providedEs () f :> es)
  => Eff (providedEs ++ es) a
  -> Eff es (f a)
provideList = send . ProvideListWith @providedEs ()

-- | Run the handler with unchanged return type.
provideList_
  :: forall providedEs es a
   . (HasCallStack, ProviderList_ providedEs () :> es)
  => Eff (providedEs ++ es) a
  -> Eff es a
provideList_ = dropIdentity . send . ProvideListWith @providedEs ()

-- | Run the handler with a given input.
provideListWith
  :: forall providedEs input f es a
   . (HasCallStack, ProviderList providedEs input f :> es)
  => input
  -- ^ The input to the handler.
  -> Eff (providedEs ++ es) a
  -> Eff es (f a)
provideListWith input = send . ProvideListWith @providedEs input

-- | Run the handler that doesn't change its return type with a given input.
provideListWith_
  :: forall providedEs input es a
   . (HasCallStack, ProviderList_ providedEs input :> es)
  => input
  -- ^ The input to the handler.
  -> Eff (providedEs ++ es) a
  -> Eff es a
provideListWith_ input = dropIdentity . send . ProvideListWith @providedEs input

----------------------------------------
-- Helpers

dropIdentity :: Eff es (Identity a) -> Eff es a
dropIdentity = coerce
