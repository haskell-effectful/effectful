{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Resource management via 'R.MonadResource'.
module Effectful.Resource
  ( ResourceE
  , runResourceE

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

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

-- | Data tag for a resource effect.
newtype ResourceE :: Effect where
  ResourceE :: R.InternalState -> ResourceE m r

-- | Run the resource effect.
runResourceE :: IOE :> es => Eff (ResourceE : es) a -> Eff es a
runResourceE m = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  istate <- R.createInternalState
  mask $ \restore -> do
    es <- unsafeConsEnv (IdE (ResourceE istate)) noRelinker es0
    a <- restore (unEff m es) `catch` \e -> do
      unsafeTailEnv size0 es
      RI.stateCleanupChecked (Just e) istate
      throwIO e
    unsafeTailEnv size0 es
    RI.stateCleanupChecked Nothing istate
    pure a

----------------------------------------
-- Internal state

-- | Get the 'R.InternalState' of the current 'Resource' effect.
getInternalState :: ResourceE :> es => Eff es R.InternalState
getInternalState = do
  IdE (ResourceE istate) <- getEffect
  pure istate

-- | Run the 'Resource' effect with existing 'R.InternalState'.
--
-- /Note:/ the 'R.InternalState' will not be closed at the end.
runInternalState :: R.InternalState -> Eff (ResourceE : es) a -> Eff es a
runInternalState istate = evalEffect $ IdE (ResourceE istate)

----------------------------------------
-- Orphan instance

instance (IOE :> es, ResourceE :> es) => R.MonadResource (Eff es) where
  liftResourceT (RI.ResourceT m) = unsafeEff $ \es -> do
    getEnv es >>= \(IdE (ResourceE istate)) -> m istate
