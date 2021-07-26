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

  -- * Dealing with nested resource effects
  , useCurrentScope
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

-- | Capture the 'R.InternalState' of the encosing 'Resource' effect and get a
-- function allowing its use later.
--
-- This can be useful in the presence of nested 'Resource' effects. Careful not
-- to use the returned runner outside the current 'Resource' effect!
useCurrentScope :: Resource :> es => Eff es (Eff (Resource : es') a -> Eff es' a)
useCurrentScope = flip runInternalState <$> getInternalState

-- | Get the 'R.InternalState' of the current 'Resource' effect. Take care that
-- this does not escape the current 'Resource' effect.
getInternalState :: Resource :> es => Eff es R.InternalState
getInternalState = do
  IdE (Resource istate) <- getEffect
  pure istate

-- | Eliminate the 'Resource' effect with a specific 'R.InternalState'.
--
-- Note that this doesn't close the 'R.InternalState'!
runInternalState :: Eff (Resource : es) a -> R.InternalState -> Eff es a
runInternalState m i = evalEffect (IdE (Resource i)) m
