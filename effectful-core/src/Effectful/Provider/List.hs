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

-- | Provide a way to run a handler of multiple @providedEs@ with a given
-- @input@.
--
-- /Note:/ @f@ can be used to alter the return type of the handler. If that's
-- unnecessary, use 'ProviderList_'.
data ProviderList (providedEs :: [Effect]) (input :: Type) (f :: Type -> Type) :: Effect

-- | A restricted variant of 'ProviderList' with unchanged return type of the
-- handler.
type ProviderList_ providedEs input = ProviderList providedEs input Identity

type instance DispatchOf (ProviderList providedEs input f) = Static NoSideEffects

-- | Wrapper to prevent a space leak on reconstruction of 'ProviderList' in
-- 'relinkProviderList' (see https://gitlab.haskell.org/ghc/ghc/-/issues/25520).
newtype ProviderListImpl input f providedEs es where
  ProviderListImpl
    :: (forall r. HasCallStack => input -> Eff (providedEs ++ es) r -> Eff es (f r))
    -> ProviderListImpl input f providedEs es

data instance StaticRep (ProviderList providedEs input f) where
  ProviderList
    :: KnownEffects providedEs
    => !(Env handlerEs)
    -> !(ProviderListImpl input f providedEs handlerEs)
    -> StaticRep (ProviderList providedEs input f)

-- | Run the 'ProviderList' effect with a given handler.
runProviderList
  :: (HasCallStack, KnownEffects providedEs)
  => (forall r. HasCallStack => input -> Eff (providedEs ++ es) r -> Eff es (f r))
  -- ^ The handler.
  -> Eff (ProviderList providedEs input f : es) a
  -> Eff es a
runProviderList providerList action = runProviderListImpl action $
  ProviderListImpl (let ?callStack = thawCallStack ?callStack in providerList)

-- | Run the 'Provider' effect with a given handler that doesn't change its
-- return type.
runProviderList_
  :: (HasCallStack, KnownEffects providedEs)
  => (forall r. HasCallStack => input -> Eff (providedEs ++ es) r -> Eff es r)
  -- ^ The handler.
  -> Eff (ProviderList_ providedEs input : es) a
  -> Eff es a
runProviderList_ providerList action = runProviderListImpl action $
  ProviderListImpl $ let ?callStack = thawCallStack ?callStack
                     in \input -> coerce . providerList input

-- | Run the handler.
provideList
  :: forall providedEs f es a
   . (HasCallStack, ProviderList providedEs () f :> es)
  => Eff (providedEs ++ es) a
  -> Eff es (f a)
provideList = provideListWith @providedEs ()

-- | Run the handler with unchanged return type.
provideList_
  :: forall providedEs es a
   . (HasCallStack, ProviderList_ providedEs () :> es)
  => Eff (providedEs ++ es) a
  -> Eff es a
provideList_ = provideListWith_ @providedEs ()

-- | Run the handler with a given input.
provideListWith
  :: forall providedEs input f es a
   . (HasCallStack, ProviderList providedEs input f :> es)
  => input
  -- ^ The input to the handler.
  -> Eff (providedEs ++ es) a
  -> Eff es (f a)
provideListWith input action = unsafeEff $ \es -> do
  ProviderList (handlerEs :: Env handlerEs) (ProviderListImpl providerList) <- do
    getEnv @(ProviderList providedEs input f) es
  (`unEff` handlerEs)
    -- Corresponds to a thawCallStack in runProviderList.
    . withFrozenCallStack providerList input
    . unsafeEff $ \eHandlerEs -> do
    unEff action =<< copyRefs @providedEs @handlerEs eHandlerEs es

-- | Run the handler that doesn't change its return type with a given input.
provideListWith_
  :: forall providedEs input es a
   . (HasCallStack, ProviderList_ providedEs input :> es)
  => input
  -- ^ The input to the handler.
  -> Eff (providedEs ++ es) a
  -> Eff es a
provideListWith_ input = adapt . provideListWith @providedEs input
  where
    adapt :: Eff es (Identity a) -> Eff es a
    adapt = coerce

----------------------------------------
-- Helpers

runProviderListImpl
  :: (HasCallStack, KnownEffects providedEs)
  => Eff (ProviderList providedEs input f : es) a
  -> ProviderListImpl input f providedEs es
  -> Eff es a
runProviderListImpl action providerListImpl = unsafeEff $ \es -> do
  inlineBracket
    (consEnv (ProviderList es providerListImpl) relinkProviderList es)
    unconsEnv
    (unEff action)
{-# INLINE runProviderListImpl #-}

relinkProviderList :: Relinker StaticRep (ProviderList e input f)
relinkProviderList = Relinker $ \relink (ProviderList handlerEs run) -> do
  newHandlerEs <- relink handlerEs
  pure $ ProviderList newHandlerEs run

copyRefs
  :: forall providedEs handlerEs es
   . (HasCallStack, KnownEffects providedEs)
  => Env (providedEs ++ handlerEs)
  -> Env es
  -> IO (Env (providedEs ++ es))
copyRefs (Env hoffset hrefs hstorage) (Env offset refs0 storage) = do
  when (hstorage /= storage) $ do
    error "storages do not match"
  let providedEsSize = knownEffectsLength @providedEs
      esSize = sizeofPrimArray refs0 - offset
  mrefs <- newPrimArray (providedEsSize + esSize)
  copyPrimArray mrefs 0 hrefs hoffset providedEsSize
  copyPrimArray mrefs providedEsSize refs0 offset esSize
  refs <- unsafeFreezePrimArray mrefs
  pure $ Env 0 refs storage
