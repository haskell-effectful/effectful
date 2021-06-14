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

import Effectful.Internal.Env
import Effectful.Internal.Has
import Effectful.Internal.Monad

-- | Data tag for a resource effect.
newtype Resource = Resource R.InternalState

-- | Run the resource effect.
runResource :: Eff (Resource : es) a -> Eff es a
runResource (Eff m) = impureEff $ \es0 -> do
  size0 <- sizeEnv es0
  istate <- R.createInternalState
  mask $ \restore -> do
    es <- unsafeConsEnv (Resource istate) es0
    a <- restore (m es) `catch` \e -> do
      _ <- unsafeTailEnv size0 es
      RI.stateCleanupChecked (Just e) istate
      throwIO e
    _ <- unsafeTailEnv size0 es
    RI.stateCleanupChecked Nothing istate
    pure a

instance (IOE :> es, Resource :> es) => R.MonadResource (Eff es) where
  liftResourceT (RI.ResourceT m) = impureEff $ \es -> do
    getEnv es >>= \(Resource istate) -> m istate
