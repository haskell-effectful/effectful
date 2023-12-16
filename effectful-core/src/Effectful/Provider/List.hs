{-# LANGUAGE AllowAmbiguousTypes #-}
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

import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import Effectful.Internal.Effect
import Effectful.Internal.Env (Env(..))
import Effectful.Internal.Utils

-- | Provide a way to run a handler of multiple @effects@ with a given @input@.
--
-- /Note:/ @f@ can be used to alter the return type of the handler. If that's
-- unnecessary, use 'ProviderList_'.
data ProviderList (effects :: [Effect]) (input :: Type) (f :: Type -> Type) :: Effect

-- | A restricted variant of 'ProviderList' with unchanged return type of the
-- handler.
type ProviderList_ effs input = ProviderList effs input Identity

type instance DispatchOf (ProviderList effs input f) = Static NoSideEffects

data instance StaticRep (ProviderList effs input f) where
  ProviderList
    :: KnownEffects effs
    => !(Env handlerEs)
    -> !(forall r. input -> Eff (effs ++ handlerEs) r -> Eff handlerEs (f r))
    -> StaticRep (ProviderList effs input f)

-- | Run the 'ProviderList' effect with a given handler.
runProviderList
  :: KnownEffects effs
  => (forall r. input -> Eff (effs ++ es) r -> Eff es (f r))
  -- ^ The handler.
  -> Eff (ProviderList effs input f : es) a
  -> Eff es a
runProviderList run m = unsafeEff $ \es0 -> do
  inlineBracket
    (consEnv (ProviderList es0 run) relinkProviderList es0)
    unconsEnv
    (\es -> unEff m es)

-- | Run the 'Provider' effect with a given handler that doesn't change its
-- return type.
runProviderList_
  :: KnownEffects effs
  => (forall r. input -> Eff (effs ++ es) r -> Eff es r)
  -- ^ The handler.
  -> Eff (ProviderList_ effs input : es) a
  -> Eff es a
runProviderList_ run = runProviderList $ \input -> coerce . run input

-- | Run the handler.
provideList
  :: forall effs f es a
   . ProviderList effs () f :> es
  => Eff (effs ++ es) a
  -> Eff es (f a)
provideList = provideListWith @effs ()

-- | Run the handler with unchanged return type.
provideList_
  :: forall effs es a
   . ProviderList_ effs () :> es
  => Eff (effs ++ es) a
  -> Eff es a
provideList_ = provideListWith_ @effs ()

-- | Run the handler with a given input.
provideListWith
  :: forall effs input f es a
   . ProviderList effs input f :> es
  => input
  -- ^ The input to the handler.
  -> Eff (effs ++ es) a
  -> Eff es (f a)
provideListWith input action = unsafeEff $ \es -> do
  ProviderList (handlerEs :: Env handlerEs) run <- getEnv @(ProviderList effs input f) es
  (`unEff` handlerEs) . run input . unsafeEff $ \eHandlerEs -> do
    unEff action =<< copyRefs @effs @handlerEs eHandlerEs es

-- | Run the handler that doesn't change its return type with a given input.
provideListWith_
  :: forall effs input es a
   . ProviderList_ effs input :> es
  => input
  -- ^ The input to the handler.
  -> Eff (effs ++ es) a
  -> Eff es a
provideListWith_ input = adapt . provideListWith @effs input
  where
    adapt :: Eff es (Identity a) -> Eff es a
    adapt = coerce

----------------------------------------
-- Helpers

relinkProviderList :: Relinker StaticRep (ProviderList e input f)
relinkProviderList = Relinker $ \relink (ProviderList handlerEs run) -> do
  newHandlerEs <- relink handlerEs
  pure $ ProviderList newHandlerEs run

copyRefs
  :: forall effs handlerEs es
   . KnownEffects effs
  => Env (effs ++ handlerEs)
  -> Env es
  -> IO (Env (effs ++ es))
copyRefs (Env hoffset hrefs hstorage) (Env offset refs0 storage) = do
  when (hstorage /= storage) $ do
    error "storages do not match"
  let size = sizeofPrimArray refs0 - offset
      effsSize = 2 * knownEffectsLength @effs
  mrefs <- newPrimArray (size + effsSize)
  copyPrimArray mrefs 0 hrefs hoffset effsSize
  copyPrimArray mrefs effsSize refs0 offset size
  refs <- unsafeFreezePrimArray mrefs
  pure $ Env 0 refs storage
