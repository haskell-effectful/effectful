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
  ) where

import Control.Exception
import qualified Control.Monad.Trans.Resource as R
import qualified Control.Monad.Trans.Resource.Internal as RI

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

-- | Data tag for a resource effect.
newtype Resource :: Effect where
  Resource :: R.InternalState -> Resource m r

-- | Run the resource effect.
runResource :: IOE :> es => Eff (Resource : es) a -> Eff es a
runResource m = unsafeEff $ \es0 -> do
  size0 <- sizeEnv es0
  istate <- R.createInternalState
  mask $ \restore -> do
    es <- unsafeConsEnv (IdE (Resource istate)) noRelinker es0
    a <- restore (unEff m es) `catch` \e -> do
      _ <- unsafeTailEnv size0 es
      RI.stateCleanupChecked (Just e) istate
      throwIO e
    _ <- unsafeTailEnv size0 es
    RI.stateCleanupChecked Nothing istate
    pure a

instance (IOE :> es, Resource :> es) => R.MonadResource (Eff es) where
  liftResourceT (RI.ResourceT m) = unsafeEff $ \es -> do
    getEnv es >>= \(IdE (Resource istate)) -> m istate
