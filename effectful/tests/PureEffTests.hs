module PureEffTests (pureEffTests) where

import Control.Concurrent
import Control.Exception
import Data.IORef
import System.Mem
import System.Timeout
import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Dispatch.Static
import Effectful.State.Static.Local

pureEffTests :: TestTree
pureEffTests = testGroup "PureEff"
  [ testCase "thunk survives a killed forcer" test_killedForcer
  , testCase "masking state doesn't leak into the forcer" test_maskingState
  , testCase "worker dies with an unreachable result" test_unreachableResult
  ]

-- The computation blocks on 'gate' so that the forcing thread is guaranteed to
-- be killed in the middle of it. Sharing goes through an IORef so that GHC
-- can't turn the two forces into two separate thunks.
sharedThunk :: MVar () -> MVar () -> IO (IORef Int)
sharedThunk started gate = newIORef . runPureEff . evalState (0 :: Int) $ do
  unsafeEff_ $ putMVar started () >> takeMVar gate
  put (42 :: Int)
  get

-- Kill a thread in the middle of forcing a shared 'runPureEff' thunk, then
-- force it again.
--
-- See https://github.com/haskell-effectful/effectful/issues/380.
test_killedForcer :: Assertion
test_killedForcer = do
  started <- newEmptyMVar
  gate <- newEmptyMVar
  shared <- sharedThunk started gate
  done <- newEmptyMVar
  worker <- forkIO $ do
    r <- try @SomeException (readIORef shared >>= evaluate)
    putMVar done r
  takeMVar started
  killThread worker
  _ <- takeMVar done
  putMVar gate ()
  v <- readIORef shared >>= evaluate
  assertEqual "thunk is not poisoned" 42 v

-- A thread that finishes an evaluation abandoned by another thread must keep
-- its own masking state.
--
-- See https://gitlab.haskell.org/ghc/ghc/-/work_items/27794.
test_maskingState :: Assertion
test_maskingState = do
  started <- newEmptyMVar
  gate <- newEmptyMVar
  shared <- sharedThunk started gate
  done <- newEmptyMVar
  worker <- forkIO $ do
    r <- try @SomeException (readIORef shared >>= evaluate)
    putMVar done r
  takeMVar started
  killThread worker
  _ <- takeMVar done

  me <- myThreadId
  throwNow <- newEmptyMVar
  thrower <- forkIO $ do
    takeMVar throwNow
    throwTo me (ErrorCall "masking state leaked")
  r <- uninterruptibleMask_ $ do
    putMVar throwNow ()
    -- Wait until the throwTo above is queued on this thread.
    threadDelay 50_000
    putMVar gate ()
    r <- try @SomeException (readIORef shared >>= evaluate)
    -- Kill the thrower while still masked, otherwise its exception arrives
    -- when the mask ends.
    killThread thrower
    pure r
  case r of
    Left e -> assertFailure $ "exception delivered under a mask: " ++ show e
    Right v -> assertEqual "thunk is not poisoned" 42 v

-- Kill a thread in the middle of forcing a 'runPureEff' thunk, drop the thunk
-- and collect garbage. The worker must receive ThreadKilled from the reaper,
-- not BlockedIndefinitelyOnMVar, so 'gate' stays reachable until the end.
test_unreachableResult :: Assertion
test_unreachableResult = do
  started <- newEmptyMVar
  gate <- newEmptyMVar
  killed <- newEmptyMVar
  shared <- newIORef . runPureEff $ do
    unsafeEff_ $ do
      putMVar started ()
      takeMVar gate `catch` \e -> putMVar killed (e :: SomeException)
    pure (42 :: Int)
  done <- newEmptyMVar
  worker <- forkIO $ do
    r <- try @SomeException (readIORef shared >>= evaluate)
    putMVar done r
  takeMVar started
  killThread worker
  _ <- takeMVar done
  writeIORef shared 0
  performMajorGC
  r <- timeout 1_000_000 $ takeMVar killed
  case r of
    Nothing -> assertFailure "worker is still running"
    Just e -> assertEqual "worker got ThreadKilled" (Just ThreadKilled) (fromException e)
  putMVar gate ()
