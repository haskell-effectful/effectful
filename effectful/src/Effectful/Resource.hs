{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Resource management via 'R.MonadResource'.
module Effectful.Resource
  ( -- * Effect
    Resource

    -- ** Handlers
  , runResource

    -- * Registering and releasing resources
  , R.allocate
  , R.allocate_
  , R.register
  , R.release
  , R.unprotect

    -- * Internal state
  , R.InternalState
  , getInternalState
  , runInternalState
  , R.createInternalState
  , R.closeInternalState
  ) where

import Control.Exception
import qualified Control.Monad.Trans.Resource as R
import qualified Control.Monad.Trans.Resource.Internal as RI

import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive

-- | Data tag for a resource effect.
data Resource :: Effect

type instance DispatchOf Resource = Static WithSideEffects
newtype instance StaticRep Resource = Resource R.InternalState

-- | Run the resource effect.
runResource :: IOE :> es => Eff (Resource : es) a -> Eff es a
runResource m = unsafeEff $ \es0 -> do
  istate <- R.createInternalState
  mask $ \unmask -> do
    es <- consEnv (Resource istate) dummyRelinker es0
    a <- unmask (unEff m es) `catch` \e -> do
      unconsEnv es
      RI.stateCleanupChecked (Just e) istate
      throwIO e
    unconsEnv es
    RI.stateCleanupChecked Nothing istate
    pure a

----------------------------------------
-- Internal state

-- | Get the 'R.InternalState' of the current 'Resource' effect.
getInternalState :: Resource :> es => Eff es R.InternalState
getInternalState = do
  Resource istate <- getStaticRep
  pure istate

-- | Run the 'Resource' effect with existing 'R.InternalState'.
--
-- /Note:/ the 'R.InternalState' will not be closed at the end.
runInternalState :: IOE :> es => R.InternalState -> Eff (Resource : es) a -> Eff es a
runInternalState istate = evalStaticRep (Resource istate)

----------------------------------------
-- Orphan instance

instance (IOE :> es, Resource :> es) => R.MonadResource (Eff es) where
  liftResourceT (RI.ResourceT m) = unsafeEff $ \es -> do
    getEnv es >>= \(Resource istate) -> m istate
