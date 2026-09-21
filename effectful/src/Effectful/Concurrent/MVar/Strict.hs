-- | Lifted "Control.Concurrent.MVar.Strict".
--
-- @since 2.4.0.0
module Effectful.Concurrent.MVar.Strict
  ( -- * Effect
    Concurrent

    -- ** Handlers
  , runConcurrent

    -- * MVar
  , MVar
  , newEmptyMVar
  , newMVar
  , takeMVar
  , putMVar
  , readMVar
  , swapMVar
  , tryTakeMVar
  , tryPutMVar
  , tryReadMVar
  , isEmptyMVar
  , withMVar
  , withMVarMasked
  , modifyMVar
  , modifyMVar_
  , modifyMVarMasked
  , modifyMVarMasked_
  , mkWeakMVar
  ) where

import System.Mem.Weak (Weak)
import Control.Concurrent.MVar.Strict (MVar)
import Control.Concurrent.MVar.Strict qualified as M

import Effectful
import Effectful.Concurrent.Effect
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import Effectful.Dispatch.Static.Unsafe

-- | Lifted 'Control.Concurrent.MVar.Strict.newEmptyMVar'.
newEmptyMVar :: Concurrent :> es => Eff es (MVar a)
newEmptyMVar = unsafeEff_ M.newEmptyMVar

-- | Lifted 'Control.Concurrent.MVar.Strict.newMVar'.
newMVar :: Concurrent :> es => a -> Eff es (MVar a)
newMVar = unsafeEff_ . M.newMVar

-- | Lifted 'Control.Concurrent.MVar.Strict.takeMVar'.
takeMVar :: Concurrent :> es => MVar a -> Eff es a
takeMVar = unsafeEff_ . M.takeMVar

-- | Lifted 'Control.Concurrent.MVar.Strict.putMVar'.
putMVar :: Concurrent :> es => MVar a -> a -> Eff es ()
putMVar var = unsafeEff_ . M.putMVar var

-- | Lifted 'Control.Concurrent.MVar.Strict.readMVar'.
readMVar :: Concurrent :> es => MVar a -> Eff es a
readMVar = unsafeEff_ . M.readMVar

-- | Lifted 'Control.Concurrent.MVar.Strict.swapMVar'.
swapMVar :: Concurrent :> es => MVar a -> a -> Eff es a
swapMVar var = unsafeEff_ . M.swapMVar var

-- | Lifted 'Control.Concurrent.MVar.Strict.tryTakeMVar'.
tryTakeMVar :: Concurrent :> es => MVar a -> Eff es (Maybe a)
tryTakeMVar = unsafeEff_ . M.tryTakeMVar

-- | Lifted 'Control.Concurrent.MVar.Strict.tryPutMVar'.
tryPutMVar :: Concurrent :> es => MVar a -> a -> Eff es Bool
tryPutMVar var = unsafeEff_ . M.tryPutMVar var

-- | Lifted 'Control.Concurrent.MVar.Strict.tryReadMVar'.
tryReadMVar :: Concurrent :> es => MVar a -> Eff es (Maybe a)
tryReadMVar = unsafeEff_ . M.tryReadMVar

-- | Lifted 'Control.Concurrent.MVar.Strict.isEmptyMVar'.
isEmptyMVar :: Concurrent :> es => MVar a -> Eff es Bool
isEmptyMVar = unsafeEff_ . M.isEmptyMVar

-- | Lifted 'Control.Concurrent.MVar.Strict.withMVar'.
withMVar :: Concurrent :> es => MVar a -> (a -> Eff es b) -> Eff es b
withMVar var f = reallyUnsafeUnliftIO $ \unlift -> do
  M.withMVar var $ unlift . f
{-# INLINE withMVar #-}

-- | Lifted 'Control.Concurrent.MVar.Strict.withMVarMasked'.
withMVarMasked :: Concurrent :> es => MVar a -> (a -> Eff es b) -> Eff es b
withMVarMasked var f = reallyUnsafeUnliftIO $ \unlift -> do
  M.withMVarMasked var $ unlift . f
{-# INLINE withMVarMasked #-}

-- | Lifted 'Control.Concurrent.MVar.Strict.modifyMVar_'.
modifyMVar_ :: Concurrent :> es => MVar a -> (a -> Eff es a) -> Eff es ()
modifyMVar_ var f = reallyUnsafeUnliftIO $ \unlift -> do
  M.modifyMVar_ var $ unlift . f
{-# INLINE modifyMVar_ #-}

-- | Lifted 'Control.Concurrent.MVar.Strict.modifyMVar'.
modifyMVar :: Concurrent :> es => MVar a -> (a -> Eff es (a, b)) -> Eff es b
modifyMVar var f = reallyUnsafeUnliftIO $ \unlift -> do
  M.modifyMVar var $ unlift . f
{-# INLINE modifyMVar #-}

-- | Lifted 'Control.Concurrent.MVar.Strict.modifyMVarMasked_'.
modifyMVarMasked_ :: Concurrent :> es => MVar a -> (a -> Eff es a) -> Eff es ()
modifyMVarMasked_ var f = reallyUnsafeUnliftIO $ \unlift -> do
  M.modifyMVarMasked_ var $ unlift . f
{-# INLINE modifyMVarMasked_ #-}

-- | Lifted 'Control.Concurrent.MVar.Strict.modifyMVarMasked'.
modifyMVarMasked :: Concurrent :> es => MVar a -> (a -> Eff es (a, b)) -> Eff es b
modifyMVarMasked var f = reallyUnsafeUnliftIO $ \unlift -> do
  M.modifyMVarMasked var $ unlift . f
{-# INLINE modifyMVarMasked #-}

-- | Lifted 'Control.Concurrent.MVar.Strict.mkWeakMVar'.
--
-- /Note:/ the finalizer will run a cloned environment, so any changes it makes
-- to thread local data will not be visible outside of it.
mkWeakMVar
  :: (HasCallStack, Concurrent :> es)
  => MVar a -> Eff es ()
  -> Eff es (Weak (MVar a))
mkWeakMVar var f = unsafeEff $ \es -> do
  -- The finalizer can run at any point and in any thread.
  M.mkWeakMVar var . unEff f =<< cloneEnv es
