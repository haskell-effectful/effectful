-- | Lifted "Control.Concurrent.MVar.Strict".
--
-- @since 2.4.0.0
module Effectful.Concurrent.MVar.Strict
  ( -- * Effect
    Concurrent

    -- ** Handlers
  , runConcurrent

    -- * MVar
  , MVar'
  , newEmptyMVar'
  , newMVar'
  , takeMVar'
  , putMVar'
  , readMVar'
  , swapMVar'
  , tryTakeMVar'
  , tryPutMVar'
  , tryReadMVar'
  , isEmptyMVar'
  , withMVar'
  , withMVar'Masked
  , modifyMVar'
  , modifyMVar'_
  , modifyMVar'Masked
  , modifyMVar'Masked_
  , mkWeakMVar'
  ) where

import System.Mem.Weak (Weak)
import Control.Concurrent.MVar.Strict (MVar')
import Control.Concurrent.MVar.Strict qualified as M

import Effectful
import Effectful.Concurrent.Effect
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import Effectful.Dispatch.Static.Unsafe

-- | Lifted 'M.newEmptyMVar''.
newEmptyMVar' :: Concurrent :> es => Eff es (MVar' a)
newEmptyMVar' = unsafeEff_ M.newEmptyMVar'

-- | Lifted 'M.newMVar''.
newMVar' :: Concurrent :> es => a -> Eff es (MVar' a)
newMVar' = unsafeEff_ . M.newMVar'

-- | Lifted 'M.takeMVar''.
takeMVar' :: Concurrent :> es => MVar' a -> Eff es a
takeMVar' = unsafeEff_ . M.takeMVar'

-- | Lifted 'M.putMVar''.
putMVar' :: Concurrent :> es => MVar' a -> a -> Eff es ()
putMVar' var = unsafeEff_ . M.putMVar' var

-- | Lifted 'M.readMVar''.
readMVar' :: Concurrent :> es => MVar' a -> Eff es a
readMVar' = unsafeEff_ . M.readMVar'

-- | Lifted 'M.swapMVar''.
swapMVar' :: Concurrent :> es => MVar' a -> a -> Eff es a
swapMVar' var = unsafeEff_ . M.swapMVar' var

-- | Lifted 'M.tryTakeMVar''.
tryTakeMVar' :: Concurrent :> es => MVar' a -> Eff es (Maybe a)
tryTakeMVar' = unsafeEff_ . M.tryTakeMVar'

-- | Lifted 'M.tryPutMVar''.
tryPutMVar' :: Concurrent :> es => MVar' a -> a -> Eff es Bool
tryPutMVar' var = unsafeEff_ . M.tryPutMVar' var

-- | Lifted 'M.tryReadMVar''.
tryReadMVar' :: Concurrent :> es => MVar' a -> Eff es (Maybe a)
tryReadMVar' = unsafeEff_ . M.tryReadMVar'

-- | Lifted 'M.isEmptyMVar''.
isEmptyMVar' :: Concurrent :> es => MVar' a -> Eff es Bool
isEmptyMVar' = unsafeEff_ . M.isEmptyMVar'

-- | Lifted 'M.withMVar''.
withMVar' :: Concurrent :> es => MVar' a -> (a -> Eff es b) -> Eff es b
withMVar' var f = reallyUnsafeUnliftIO $ \unlift -> do
  M.withMVar' var $ unlift . f
{-# INLINE withMVar' #-}

-- | Lifted 'M.withMVar'Masked'.
withMVar'Masked :: Concurrent :> es => MVar' a -> (a -> Eff es b) -> Eff es b
withMVar'Masked var f = reallyUnsafeUnliftIO $ \unlift -> do
  M.withMVar'Masked var $ unlift . f
{-# INLINE withMVar'Masked #-}

-- | Lifted 'M.modifyMVar'_'.
modifyMVar'_ :: Concurrent :> es => MVar' a -> (a -> Eff es a) -> Eff es ()
modifyMVar'_ var f = reallyUnsafeUnliftIO $ \unlift -> do
  M.modifyMVar'_ var $ unlift . f
{-# INLINE modifyMVar'_ #-}

-- | Lifted 'M.modifyMVar''.
modifyMVar' :: Concurrent :> es => MVar' a -> (a -> Eff es (a, b)) -> Eff es b
modifyMVar' var f = reallyUnsafeUnliftIO $ \unlift -> do
  M.modifyMVar' var $ unlift . f
{-# INLINE modifyMVar' #-}

-- | Lifted 'M.modifyMVar'Masked_'.
modifyMVar'Masked_ :: Concurrent :> es => MVar' a -> (a -> Eff es a) -> Eff es ()
modifyMVar'Masked_ var f = reallyUnsafeUnliftIO $ \unlift -> do
  M.modifyMVar'Masked_ var $ unlift . f
{-# INLINE modifyMVar'Masked_ #-}

-- | Lifted 'M.modifyMVar'Masked'.
modifyMVar'Masked :: Concurrent :> es => MVar' a -> (a -> Eff es (a, b)) -> Eff es b
modifyMVar'Masked var f = reallyUnsafeUnliftIO $ \unlift -> do
  M.modifyMVar'Masked var $ unlift . f
{-# INLINE modifyMVar'Masked #-}

-- | Lifted 'M.mkWeakMVar''.
--
-- /Note:/ the finalizer will run a cloned environment, so any changes it makes
-- to thread local data will not be visible outside of it.
mkWeakMVar'
  :: (HasCallStack, Concurrent :> es)
  => MVar' a -> Eff es ()
  -> Eff es (Weak (MVar' a))
mkWeakMVar' var f = unsafeEff $ \es -> do
  -- The finalizer can run at any point and in any thread.
  M.mkWeakMVar' var . unEff f =<< cloneEnv es
