-- | Lifted version of "Control.Concurrent.STM".
module Effectful.Concurrent.STM
  ( -- * Core
    STM.STM
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
import qualified Control.Concurrent.STM as STM
import System.Mem.Weak (Weak)
import GHC.Natural (Natural)

import Effectful.Concurrent.Internal
import Effectful.Internal.Effect
import Effectful.Internal.Monad

-- | Lifted version of 'STM.atomically'
atomically :: Concurrent :> es => STM a -> Eff es a
atomically = unsafeEff_ . STM.atomically

-- | Lifted version of 'STM.newTVarIO'
newTVarIO :: Concurrent :> es => a -> Eff es (TVar a)
newTVarIO = unsafeEff_ . STM.newTVarIO

-- | Lifted version of 'STM.readTVarIO'
readTVarIO :: Concurrent :> es => TVar a -> Eff es a
readTVarIO = unsafeEff_ . STM.readTVarIO

-- | Lifted version of 'STM.registerDelay'
registerDelay :: Concurrent :> es => Int -> Eff es (TVar Bool)
registerDelay = unsafeEff_ . STM.registerDelay

-- | Lifted version of 'STM.mkWeakTVar'
mkWeakTVar :: Concurrent :> es => TVar a -> Eff es () -> Eff es (Weak (TVar a))
mkWeakTVar var final = unsafeEff $ STM.mkWeakTVar var . unEff final

-- | Lifted version of 'STM.newTMVarIO'
newTMVarIO :: Concurrent :> es => a -> Eff es (TMVar a)
newTMVarIO = unsafeEff_ . STM.newTMVarIO

-- | Lifted version of 'STM.newEmptyTMVarIO'
newEmptyTMVarIO :: Concurrent :> es => Eff es (TMVar a)
newEmptyTMVarIO = unsafeEff_ STM.newEmptyTMVarIO

-- | Lifted version of 'STM.mkWeakTMVar'
mkWeakTMVar :: Concurrent :> es => TMVar a -> Eff es () -> Eff es (Weak (TMVar a))
mkWeakTMVar var final = unsafeEff $ STM.mkWeakTMVar var . unEff final

-- | Lifted version of 'STM.newTChanIO'
newTChanIO :: Concurrent :> es => Eff es (TChan a)
newTChanIO = unsafeEff_ STM.newTChanIO

-- | Lifted version of 'STM.newBroadcastTChanIO'
newBroadcastTChanIO :: Concurrent :> es => Eff es (TChan a)
newBroadcastTChanIO = unsafeEff_ STM.newBroadcastTChanIO

-- | Lifted version of 'STM.newTQueueIO'
newTQueueIO :: Concurrent :> es => Eff es (TQueue a)
newTQueueIO = unsafeEff_ STM.newTQueueIO

-- | Lifted version of 'STM.newTBQueueIO'
newTBQueueIO :: Concurrent :> es => Natural -> Eff es (TBQueue a)
newTBQueueIO = unsafeEff_ . STM.newTBQueueIO
