{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Resource management via 'R.MonadResource'.
module Effectful.Resource
  ( Resource
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

import Effectful.Dispatch.Static
import Effectful.Monad

-- | Data tag for a resource effect.
newtype Resource :: Effect where
  Resource :: R.InternalState -> Resource m r

-- | Run the resource effect.
runResource :: IOE :> es => Eff (Resource : es) a -> Eff es a
runResource m = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  istate <- R.createInternalState
  mask $ \restore -> do
    es <- unsafeConsEnv (DataA (Resource istate)) noRelinker es0
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
getInternalState :: Resource :> es => Eff es R.InternalState
getInternalState = do
  DataA (Resource istate) <- getData
  pure istate

-- | Run the 'Resource' effect with existing 'R.InternalState'.
--
-- /Note:/ the 'R.InternalState' will not be closed at the end.
runInternalState :: R.InternalState -> Eff (Resource : es) a -> Eff es a
runInternalState istate = evalData (DataA (Resource istate))

----------------------------------------
-- Orphan instance

instance (IOE :> es, Resource :> es) => R.MonadResource (Eff es) where
  liftResourceT (RI.ResourceT m) = unsafeEff $ \es -> do
    getEnv es >>= \(DataA (Resource istate)) -> m istate
