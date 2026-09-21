-- | Lifted "Control.Concurrent.STM".
module Effectful.Concurrent.STM
  ( -- * Effect
    Concurrent

    -- ** Handlers
  , runConcurrent

    -- * Core
  , STM.STM
  , atomically
  , STM.retry
  , STM.orElse
  , STM.check
  , STM.throwSTM
  , STM.catchSTM

    -- * TVar
  , STM.TVar
  , newTVarIO
  , readTVarIO
  , STM.newTVar
  , STM.readTVar
  , STM.writeTVar
  , STM.modifyTVar
  , STM.modifyTVar'
  , STM.swapTVar
  , registerDelay
  , mkWeakTVar

    -- * TMVar
  , STM.TMVar
  , STM.newTMVar
  , STM.newEmptyTMVar
  , newTMVarIO
  , newEmptyTMVarIO
  , STM.takeTMVar
  , STM.putTMVar
  , STM.readTMVar
  , STM.writeTMVar
  , STM.tryReadTMVar
  , STM.swapTMVar
  , STM.tryTakeTMVar
  , STM.tryPutTMVar
  , STM.isEmptyTMVar
  , mkWeakTMVar

    -- * TChan
  , STM.TChan
  , STM.newTChan
  , newTChanIO
  , STM.newBroadcastTChan
  , newBroadcastTChanIO
  , STM.dupTChan
  , STM.cloneTChan
  , STM.readTChan
  , STM.tryReadTChan
  , STM.peekTChan
  , STM.tryPeekTChan
  , STM.writeTChan
  , STM.unGetTChan
  , STM.isEmptyTChan

    -- * TQueue
  , STM.TQueue
  , STM.newTQueue
  , newTQueueIO
  , STM.readTQueue
  , STM.tryReadTQueue
  , STM.peekTQueue
  , STM.tryPeekTQueue
  , STM.flushTQueue
  , STM.writeTQueue
  , STM.unGetTQueue
  , STM.isEmptyTQueue

    -- * TBQueue
  , STM.TBQueue
  , STM.newTBQueue
  , newTBQueueIO
  , STM.readTBQueue
  , STM.tryReadTBQueue
  , STM.peekTBQueue
  , STM.tryPeekTBQueue
  , STM.flushTBQueue
  , STM.writeTBQueue
  , STM.unGetTBQueue
  , STM.lengthTBQueue
  , STM.isEmptyTBQueue
  , STM.isFullTBQueue
  ) where

import Control.Concurrent.STM (STM, TVar, TMVar, TChan, TQueue, TBQueue)
import Control.Concurrent.STM qualified as STM
import System.Mem.Weak (Weak)
import GHC.Natural (Natural)

import Effectful
import Effectful.Concurrent.Effect
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive

-- | Lifted 'Control.Concurrent.STM.atomically'.
atomically :: Concurrent :> es => STM a -> Eff es a
atomically = unsafeEff_ . STM.atomically

-- | Lifted 'Control.Concurrent.STM.newTVarIO'.
newTVarIO :: Concurrent :> es => a -> Eff es (TVar a)
newTVarIO = unsafeEff_ . STM.newTVarIO

-- | Lifted 'Control.Concurrent.STM.readTVarIO'.
readTVarIO :: Concurrent :> es => TVar a -> Eff es a
readTVarIO = unsafeEff_ . STM.readTVarIO

-- | Lifted 'Control.Concurrent.STM.registerDelay'.
registerDelay :: Concurrent :> es => Int -> Eff es (TVar Bool)
registerDelay = unsafeEff_ . STM.registerDelay

-- | Lifted 'Control.Concurrent.STM.mkWeakTVar'.
--
-- /Note:/ the finalizer will run a cloned environment, so any changes it makes
-- to thread local data will not be visible outside of it.
mkWeakTVar
  :: (HasCallStack, Concurrent :> es)
  => TVar a
  -> Eff es ()
  -> Eff es (Weak (TVar a))
mkWeakTVar var f = unsafeEff $ \es -> do
  -- The finalizer can run at any point and in any thread.
  STM.mkWeakTVar var . unEff f =<< cloneEnv es

-- | Lifted 'Control.Concurrent.STM.newTMVarIO'.
newTMVarIO :: Concurrent :> es => a -> Eff es (TMVar a)
newTMVarIO = unsafeEff_ . STM.newTMVarIO

-- | Lifted 'Control.Concurrent.STM.newEmptyTMVarIO'.
newEmptyTMVarIO :: Concurrent :> es => Eff es (TMVar a)
newEmptyTMVarIO = unsafeEff_ STM.newEmptyTMVarIO

-- | Lifted 'Control.Concurrent.STM.mkWeakTMVar'.
--
-- /Note:/ the finalizer will run a cloned environment, so any changes it makes
-- to thread local data will not be visible outside of it.
mkWeakTMVar
  :: (HasCallStack, Concurrent :> es)
  => TMVar a
  -> Eff es ()
  -> Eff es (Weak (TMVar a))
mkWeakTMVar var f = unsafeEff $ \es -> do
  -- The finalizer can run at any point and in any thread.
  STM.mkWeakTMVar var . unEff f =<< cloneEnv es

-- | Lifted 'Control.Concurrent.STM.newTChanIO'.
newTChanIO :: Concurrent :> es => Eff es (TChan a)
newTChanIO = unsafeEff_ STM.newTChanIO

-- | Lifted 'Control.Concurrent.STM.newBroadcastTChanIO'.
newBroadcastTChanIO :: Concurrent :> es => Eff es (TChan a)
newBroadcastTChanIO = unsafeEff_ STM.newBroadcastTChanIO

-- | Lifted 'Control.Concurrent.STM.newTQueueIO'.
newTQueueIO :: Concurrent :> es => Eff es (TQueue a)
newTQueueIO = unsafeEff_ STM.newTQueueIO

-- | Lifted 'Control.Concurrent.STM.newTBQueueIO'.
newTBQueueIO :: Concurrent :> es => Natural -> Eff es (TBQueue a)
newTBQueueIO = unsafeEff_ . STM.newTBQueueIO
