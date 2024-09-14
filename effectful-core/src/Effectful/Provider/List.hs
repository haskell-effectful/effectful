{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams #-}
-- | Turn a handler of multiple effects into an effectful operation.
--
-- Generalizes "Effectful.Provider".
--
-- @since 2.3.1.0
module Effectful.Provider.List
  ( -- * Effect
    ProviderList
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
  , KnownEffects
  ) where

import Control.Monad
import Data.Coerce
import Data.Functor.Identity
import Data.Primitive.PrimArray
import GHC.Stack

import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import Effectful.Internal.Effect
import Effectful.Internal.Env (Env(..))
import Effectful.Internal.Utils

-- | Provide a way to run a handler of a @list@ of effects with a given @input@.
--
-- /Note:/ @f@ can be used to alter the return type of the handler. If that's
-- unnecessary, use 'ProviderList_'.
data ProviderList (list :: [Effect]) (input :: Type) (f :: Type -> Type) :: Effect

-- | A restricted variant of 'ProviderList' with unchanged return type of the
-- handler.
type ProviderList_ list input = ProviderList list input Identity

type instance DispatchOf (ProviderList list input f) = Static NoSideEffects

data instance StaticRep (ProviderList list input f) where
  ProviderList
    :: KnownEffects list
    => !(Env providerEs)
    -> !(forall r. HasCallStack => input -> Eff (list ++ providerEs) r -> Eff providerEs (f r))
    -> StaticRep (ProviderList list input f)

-- | Run the 'ProviderList' effect with a given handler.
runProviderList
  :: (HasCallStack, KnownEffects list)
  => (forall r. HasCallStack => input -> Eff (list ++ es) r -> Eff es (f r))
  -- ^ The handler.
  -> Eff (ProviderList list input f : es) a
  -> Eff es a
runProviderList providerList m = unsafeEff $ \es0 -> do
  inlineBracket
    (consEnv (mkProviderList es0) relinkProviderList es0)
    unconsEnv
    (\es -> unEff m es)
  where
    -- Corresponds to withFrozenCallStack in provideListWith.
    mkProviderList es =
      ProviderList es (let ?callStack = thawCallStack ?callStack in providerList)

-- | Run the 'Provider' effect with a given handler that doesn't change its
-- return type.
runProviderList_
  :: (HasCallStack, KnownEffects list)
  => (forall r. HasCallStack => input -> Eff (list ++ es) r -> Eff es r)
  -- ^ The handler.
  -> Eff (ProviderList_ list input : es) a
  -> Eff es a
runProviderList_ providerList = runProviderList $ \input -> coerce . providerList input

-- | Run the handler.
provideList
  :: forall list f es a
   . (HasCallStack, ProviderList list () f :> es)
  => Eff (list ++ es) a
  -> Eff es (f a)
provideList = provideListWith @list ()

-- | Run the handler with unchanged return type.
provideList_
  :: forall list es a
   . (HasCallStack, ProviderList_ list () :> es)
  => Eff (list ++ es) a
  -> Eff es a
provideList_ = provideListWith_ @list ()

-- | Run the handler with a given input.
provideListWith
  :: forall list input f es a
   . (HasCallStack, ProviderList list input f :> es)
  => input
  -- ^ The input to the handler.
  -> Eff (list ++ es) a
  -> Eff es (f a)
provideListWith input action = unsafeEff $ \es -> do
  ProviderList (providerEs :: Env providerEs) providerList <- do
    getEnv @(ProviderList list input f) es
  (`unEff` providerEs)
    -- Corresponds to a thawCallStack in runProviderList.
    . withFrozenCallStack providerList input
    . unsafeEff $ \eHandlerEs -> do
    unEff action =<< copyRefs @list @providerEs eHandlerEs es

-- | Run the handler that doesn't change its return type with a given input.
provideListWith_
  :: forall list input es a
   . (HasCallStack, ProviderList_ list input :> es)
  => input
  -- ^ The input to the handler.
  -> Eff (list ++ es) a
  -> Eff es a
provideListWith_ input = adapt . provideListWith @list input
  where
    adapt :: Eff es (Identity a) -> Eff es a
    adapt = coerce

----------------------------------------
-- Helpers

relinkProviderList :: Relinker StaticRep (ProviderList e input f)
relinkProviderList = Relinker $ \relink (ProviderList providerEs run) -> do
  newHandlerEs <- relink providerEs
  pure $ ProviderList newHandlerEs run

copyRefs
  :: forall list providerEs es
   . (HasCallStack, KnownEffects list)
  => Env (list ++ providerEs)
  -> Env es
  -> IO (Env (list ++ es))
copyRefs (Env hoffset hrefs hstorage) (Env offset refs0 storage) = do
  when (hstorage /= storage) $ do
    error "storages do not match"
  let size = sizeofPrimArray refs0 - offset
      listSize = 2 * knownEffectsLength @list
  mrefs <- newPrimArray (size + listSize)
  copyPrimArray mrefs 0 hrefs hoffset listSize
  copyPrimArray mrefs listSize refs0 offset size
  refs <- unsafeFreezePrimArray mrefs
  pure $ Env 0 refs storage
