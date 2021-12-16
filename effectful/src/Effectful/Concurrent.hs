module Effectful.Concurrent
  ( -- * The effect
    Concurrent
  , runConcurrent

    -- * Basic concurrency operations
  , myThreadId
  , forkIO
  , forkFinally
  , forkIOWithUnmask
  , killThread
  , throwTo

    -- ** Threads with affinity
  , forkOn
  , forkOnWithUnmask
  , getNumCapabilities
  , setNumCapabilities
  , getNumProcessors
  , threadCapability

    -- * Scheduling
  , yield

    -- ** Waiting
  , threadDelay
  , threadWaitRead
  , threadWaitWrite
  , threadWaitReadSTM
  , threadWaitWriteSTM

    -- * Bound threads
  , forkOS
  , forkOSWithUnmask
  , isCurrentThreadBound
  , runInBoundThread
  , runInUnboundThread

    -- * Weak references to ThreadIds
  , mkWeakThreadId

    -- * Re-exports
  , C.rtsSupportsBoundThreads
  ) where

import Control.Exception (Exception, SomeException)
import Data.Bifunctor (second)
import System.Mem.Weak (Weak)
import System.Posix.Types (Fd)
import UnliftIO.STM (STM)
import qualified Control.Concurrent as C
import qualified GHC.Conc as GHC

import Effectful.Concurrent.Effect
import Effectful.Dispatch.Static
import Effectful.Monad

----------------------------------------
-- Basic concurrency operations

-- | Lifted 'C.myThreadId'.
myThreadId :: Concurrent :> es => Eff es C.ThreadId
myThreadId = unsafeEff_ C.myThreadId

-- | Lifted 'C.forkIO'.
forkIO :: Concurrent :> es => Eff es () -> Eff es C.ThreadId
forkIO k = unsafeEff $ \es -> do
  esF <- cloneEnv es
  C.forkIO $ unEff k esF

-- | Lifted 'C.forkFinally'.
forkFinally
  :: Concurrent :> es
  => Eff es a
  -> (Either SomeException a -> Eff es ())
  -> Eff es C.ThreadId
forkFinally k cleanup = unsafeEff $ \es -> do
  esF <- cloneEnv es
  C.forkFinally (unEff k esF) ((`unEff` esF) . cleanup)

-- | Lifted 'C.forkIOWithUnmask'.
forkIOWithUnmask
  :: Concurrent :> es
  => ((forall a es'. IOE :> es' => Eff es' a -> Eff es' a)
  -> Eff es ())
  -> Eff es C.ThreadId
forkIOWithUnmask f = unsafeEff $ \es -> do
  esF <- cloneEnv es
  C.forkIOWithUnmask (\restore -> unEff (f (unsafeLiftMapIO restore)) esF)

-- | Lifted 'C.killThread'.
killThread :: Concurrent :> es => C.ThreadId -> Eff es ()
killThread = unsafeEff_ . C.killThread

-- | Lifted 'C.throwTo'.
throwTo :: (Concurrent :> es, Exception e) => C.ThreadId -> e -> Eff es ()
throwTo tid = unsafeEff_ . C.throwTo tid

----------------------------------------
-- Threads with affinity

-- | Lifted 'C.forkOn'.
forkOn :: Concurrent :> es => Int -> Eff es () -> Eff es C.ThreadId
forkOn n k = unsafeEff $ \es -> do
  esF <- cloneEnv es
  C.forkOn n (unEff k esF)

-- | Lifted 'C.forkOnWithUnmask'.
forkOnWithUnmask
  :: Concurrent :> es
  => Int
  -> ((forall a es'. IOE :> es' => Eff es' a -> Eff es' a) -> Eff es ())
  -> Eff es C.ThreadId
forkOnWithUnmask n f = unsafeEff $ \es -> do
  esF <- cloneEnv es
  C.forkOnWithUnmask n (\restore -> unEff (f (unsafeLiftMapIO restore)) esF)

-- | Lifted 'C.getNumCapabilities'.
getNumCapabilities :: Concurrent :> es => Eff es Int
getNumCapabilities = unsafeEff_ C.getNumCapabilities

-- | Lifted 'C.setNumCapabilities'.
setNumCapabilities :: Concurrent :> es => Int -> Eff es ()
setNumCapabilities = unsafeEff_ . C.setNumCapabilities

-- | Lifted 'GHC.getNumProcessors'.
getNumProcessors :: Concurrent :> es => Eff es Int
getNumProcessors = unsafeEff_ GHC.getNumProcessors

-- | Lifted 'C.threadCapability'.
threadCapability :: Concurrent :> es => C.ThreadId -> Eff es (Int, Bool)
threadCapability = unsafeEff_ . C.threadCapability

----------------------------------------
-- Scheduling

-- | Lifted 'C.yield'.
yield :: Concurrent :> es => Eff es ()
yield = unsafeEff_ C.yield

----------------------------------------
-- Waiting

-- | Lifted 'C.threadDelay'.
threadDelay :: Concurrent :> es => Int -> Eff es ()
threadDelay = unsafeEff_ . C.threadDelay

-- | Lifted 'C.threadWaitRead'.
threadWaitRead :: Concurrent :> es => Fd -> Eff es ()
threadWaitRead = unsafeEff_ . C.threadWaitRead

-- | Lifted 'C.threadWaitWrite'.
threadWaitWrite :: Concurrent :> es => Fd -> Eff es ()
threadWaitWrite = unsafeEff_ . C.threadWaitWrite

-- | Lifted 'C.threadWaitReadSTM'.
threadWaitReadSTM :: Concurrent :> es => Fd -> Eff es (STM (), Eff es ())
threadWaitReadSTM fd = unsafeEff_ $ do
  second unsafeEff_ <$> C.threadWaitReadSTM fd

-- | Lifted 'C.threadWaitWriteSTM'.
threadWaitWriteSTM :: Concurrent :> es => Fd -> Eff es (STM (), Eff es ())
threadWaitWriteSTM fd = unsafeEff_ $ do
  second unsafeEff_ <$> C.threadWaitWriteSTM fd

----------------------------------------
-- Bound threads

-- | Lifted 'C.forkOS'.
forkOS :: Concurrent :> es => Eff es () -> Eff es C.ThreadId
forkOS k = unsafeEff $ \es -> do
  esF <- cloneEnv es
  C.forkOS $ unEff k esF

-- | Lifted 'E.forkOSWithUnmask'.
forkOSWithUnmask
  :: Concurrent :> es
  => ((forall a es'. IOE :> es' => Eff es' a -> Eff es' a) -> Eff es ())
  -> Eff es C.ThreadId
forkOSWithUnmask f = unsafeEff $ \es -> do
  esF <- cloneEnv es
  C.forkOSWithUnmask (\restore -> unEff (f (unsafeLiftMapIO restore)) esF)

-- | Lifted 'C.isCurrentThreadBound'.
isCurrentThreadBound :: Concurrent :> es => Eff es Bool
isCurrentThreadBound = unsafeEff_ C.isCurrentThreadBound

-- | Lifted 'C.runInBoundThread'.
runInBoundThread :: Concurrent :> es => Eff es a -> Eff es a
runInBoundThread k = unsafeEff $ \es -> do
  esF <- cloneEnv es
  C.runInBoundThread $ unEff k esF

-- | Lifted 'C.runInUnboundThread'.
runInUnboundThread :: Concurrent :> es => Eff es a -> Eff es a
runInUnboundThread k = unsafeEff $ \es -> do
  esF <- cloneEnv es
  C.runInUnboundThread $ unEff k esF

----------------------------------------
-- Weak references to ThreadIds

-- | Lifted 'C.mkWeakThreadId'.
mkWeakThreadId :: Concurrent :> es => C.ThreadId -> Eff es (Weak C.ThreadId)
mkWeakThreadId = unsafeEff_ . C.mkWeakThreadId
