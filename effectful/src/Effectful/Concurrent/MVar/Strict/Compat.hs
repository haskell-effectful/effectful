-- | Lifted "Control.Concurrent.MVar" with operations that force values put
-- inside an 'MVar' to WHNF.
module Effectful.Concurrent.MVar.Strict.Compat
  {-# DEPRECATED "Use Effectful.Concurrent.MVar.Strict" #-}
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
  , isEmptyMVar
  , withMVar
  , withMVarMasked
  , modifyMVar
  , modifyMVar_
  , modifyMVarMasked
  , modifyMVarMasked_
  , tryReadMVar
  , mkWeakMVar
  ) where

import System.Mem.Weak (Weak)
import Control.Exception (evaluate)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar qualified as M

import Effectful
import Effectful.Concurrent.Effect
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import Effectful.Dispatch.Static.Unsafe

-- | Lifted 'M.newEmptyMVar'.
newEmptyMVar :: Concurrent :> es => Eff es (MVar a)
newEmptyMVar = unsafeEff_ M.newEmptyMVar

-- | Lifted 'M.newMVar' that evaluates the value to WHNF.
newMVar :: Concurrent :> es => a -> Eff es (MVar a)
newMVar a = unsafeEff_ $ M.newMVar =<< evaluate a

-- | Lifted 'M.takeMVar'.
takeMVar :: Concurrent :> es => MVar a -> Eff es a
takeMVar = unsafeEff_ . M.takeMVar

-- | Lifted 'M.putMVar'.
putMVar :: Concurrent :> es => MVar a -> a -> Eff es ()
putMVar var a = unsafeEff_ $ M.putMVar var =<< evaluate a

-- | Lifted 'M.readMVar'.
readMVar :: Concurrent :> es => MVar a -> Eff es a
readMVar = unsafeEff_ . M.readMVar

-- | Lifted 'M.swapMVar' that evaluates the new value to WHNF.
swapMVar :: Concurrent :> es => MVar a -> a -> Eff es a
swapMVar var a = unsafeEff_ $ M.swapMVar var =<< evaluate a

-- | Lifted 'M.tryTakeMVar'.
tryTakeMVar :: Concurrent :> es => MVar a -> Eff es (Maybe a)
tryTakeMVar = unsafeEff_ . M.tryTakeMVar

-- | Lifted 'M.tryPutMVar' that evaluates the new value to WHNF.
tryPutMVar :: Concurrent :> es => MVar a -> a -> Eff es Bool
tryPutMVar var a = unsafeEff_ $ M.tryPutMVar var =<< evaluate a

-- | Lifted 'M.isEmptyMVar'.
isEmptyMVar :: Concurrent :> es => MVar a -> Eff es Bool
isEmptyMVar = unsafeEff_ . M.isEmptyMVar

-- | Lifted 'M.tryReadMVar'.
tryReadMVar :: Concurrent :> es => MVar a -> Eff es (Maybe a)
tryReadMVar = unsafeEff_ . M.tryReadMVar

-- | Lifted 'M.withMVar'.
withMVar :: Concurrent :> es => MVar a -> (a -> Eff es b) -> Eff es b
withMVar var f = reallyUnsafeUnliftIO $ \unlift -> do
  M.withMVar var $ unlift . f
{-# INLINE withMVar #-}

-- | Lifted 'M.withMVarMasked'.
withMVarMasked :: Concurrent :> es => MVar a -> (a -> Eff es b) -> Eff es b
withMVarMasked var f = reallyUnsafeUnliftIO $ \unlift -> do
  M.withMVarMasked var $ unlift . f
{-# INLINE withMVarMasked #-}

-- | Lifted 'M.modifyMVar_' that evaluates the new value to WHNF.
modifyMVar_ :: Concurrent :> es => MVar a -> (a -> Eff es a) -> Eff es ()
modifyMVar_ var f = reallyUnsafeUnliftIO $ \unlift -> do
  M.modifyMVar_ var $ \a0 -> do
    a <- unlift $ f a0
    evaluate a
{-# INLINE modifyMVar_ #-}

-- | Lifted 'M.modifyMVar' that evaluates the new value to WHNF.
modifyMVar :: Concurrent :> es => MVar a -> (a -> Eff es (a, b)) -> Eff es b
modifyMVar var f = reallyUnsafeUnliftIO $ \unlift -> do
  M.modifyMVar var $ \a0 -> do
    (a, b) <- unlift $ f a0
    (, b) <$> evaluate a
{-# INLINE modifyMVar #-}

-- | Lifted 'M.modifyMVarMasked_' that evaluates the new value to WHNF.
modifyMVarMasked_ :: Concurrent :> es => MVar a -> (a -> Eff es a) -> Eff es ()
modifyMVarMasked_ var f = reallyUnsafeUnliftIO $ \unlift -> do
  M.modifyMVarMasked_ var $ \a0 -> do
    a <- unlift $ f a0
    evaluate a
{-# INLINE modifyMVarMasked_ #-}

-- | Lifted 'M.modifyMVarMasked' that evaluates the new value to WHNF.
modifyMVarMasked :: Concurrent :> es => MVar a -> (a -> Eff es (a, b)) -> Eff es b
modifyMVarMasked var f = reallyUnsafeUnliftIO $ \unlift -> do
  M.modifyMVarMasked var $ \a0 -> do
    (a, b) <- unlift $ f a0
    (, b) <$> evaluate a
{-# INLINE modifyMVarMasked #-}

-- | Lifted 'M.mkWeakMVar'.
--
-- /Note:/ the finalizer will run a cloned environment, so any changes it makes
-- to thread local data will not be visible outside of it.
mkWeakMVar
  :: (HasCallStack, Concurrent :> es)
  => MVar a
  -> Eff es ()
  -> Eff es (Weak (MVar a))
mkWeakMVar var f = unsafeEff $ \es -> do
  -- The finalizer can run at any point and in any thread.
  M.mkWeakMVar var . unEff f =<< cloneEnv es
