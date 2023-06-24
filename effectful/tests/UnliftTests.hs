module UnliftTests (unliftTests) where

import Control.Concurrent
import Control.Exception
import Data.Functor
import Test.Tasty
import Test.Tasty.HUnit
import qualified UnliftIO.Async as A

import Effectful
import Effectful.State.Dynamic
import qualified Utils as U

unliftTests :: TestTree
unliftTests = testGroup "Unlift"
  [ testCase "Strategy stays the same in a new thread" test_threadStrategy
  , testCase "SeqUnlift in new thread" test_seqUnliftInNewThread
  , testGroup "SyncUnlift"
    [ testCase "SyncError works" test_syncErrorWorks
    , testCase "SyncError throws when appropriate" test_syncErrorThrows
    , testCase "SyncWait works" test_syncWaitWorks
    ]
  , testGroup "Ephemeral strategy"
    [ testCase "Invalid limit" test_ephemeralInvalid
    , testCase "Uses in same thread" test_ephemeralSameThread
    , testCase "Uses in multiple threads" test_ephemeralMultipleThreads
    ]
  , testGroup "Persistent strategy"
    [ testCase "Invalid limit" test_persistentInvalid
    , testCase "Uses in same thread" test_persistentSameThread
    , testCase "Uses in multiple threads" test_persistentMultipleThreads
    ]
  ]

test_threadStrategy :: Assertion
test_threadStrategy = runEff $ do
  let strategy = ConcUnlift Ephemeral Unlimited
  s <- withUnliftStrategy strategy $ do
    withRunInIO $ \runInIO -> do
      inThread $ runInIO unliftStrategy
  U.assertEqual "correct strategy" strategy s

test_seqUnliftInNewThread :: Assertion
test_seqUnliftInNewThread = runEff $ do
  assertThrowsUnliftError "InvalidUseOfSeqUnlift error" $ do
    withEffToIO SeqUnlift $ \runInIO -> do
      inThread $ runInIO $ return ()

test_syncErrorWorks :: Assertion
test_syncErrorWorks = runEff . evalStateLocal @Int 1 $ do
  modifyFromAsync (*2)
  modifyFromAsync (*3)
  U.assertEqual "correct state" 6 =<< get @Int
  where
    modifyFromAsync f = withEffToIO (SyncUnlift SyncError) $ \runInIO -> do
      void . A.async . runInIO $ liftIO (threadDelay 10000) >> modify @Int f
      -- Wait for the async action to start, but try exiting the scope of
      -- withEffToIO before the unlifted computation had a chance to finish to
      -- check that it waits until it does.
      threadDelay 1000

test_syncErrorThrows :: Assertion
test_syncErrorThrows = runEff $ do
  assertThrowsUnliftError "Sync error" $ do
    withEffToIO (SyncUnlift SyncError) $ \runInIO -> do
      A.race_ (runInIO . liftIO $ threadDelay 10000)
              (runInIO . liftIO $ threadDelay 10000)

test_syncWaitWorks :: Assertion
test_syncWaitWorks = runEff . evalStateLocal @Int 0 $ do
  withEffToIO (SyncUnlift SyncWait) $ \runInIO -> do
    -- SyncUnlift SyncWait turns concurrent code into sequential code.
    A.replicateConcurrently_ 100 $ runInIO $ modify @Int (+1)
  U.assertEqual "correct state" 100 =<< get @Int

test_ephemeralInvalid :: Assertion
test_ephemeralInvalid = runEff $ do
  assertThrowsUnliftError "InvalidNumberOfUses error" $ do
    withEffToIO (ConcUnlift Ephemeral $ Limited 0) $ \_ -> return ()

test_ephemeralSameThread :: Assertion
test_ephemeralSameThread = runEff $ do
  assertThrowsUnliftError "ExceededNumberOfUses error" $ do
    withEffToIO (ConcUnlift Ephemeral $ Limited 1) $ \runInIO -> inThread $ do
      runInIO $ return ()
      runInIO $ return ()

test_ephemeralMultipleThreads :: Assertion
test_ephemeralMultipleThreads = runEff $ do
  assertThrowsUnliftError "ExceededNumberOfUses error" $ do
    withEffToIO (ConcUnlift Ephemeral $ Limited 1) $ \runInIO -> do
      inThread $ runInIO $ return ()
      inThread $ runInIO $ return ()

test_persistentInvalid :: Assertion
test_persistentInvalid = runEff $ do
  assertThrowsUnliftError "InvalidNumberOfThreads error" $ do
    withEffToIO (ConcUnlift Persistent $ Limited 0) $ \_ -> return ()

test_persistentSameThread :: Assertion
test_persistentSameThread = runEff $ do
  withEffToIO (ConcUnlift Persistent $ Limited 1) $ \runInIO -> inThread $ do
    runInIO $ return ()
    runInIO $ return ()

test_persistentMultipleThreads :: Assertion
test_persistentMultipleThreads = runEff $ do
  assertThrowsUnliftError "ExceededNumberOfThreads error" $ do
    withEffToIO (ConcUnlift Persistent $ Limited 1) $ \runInIO -> do
      inThread $ runInIO $ return ()
      inThread $ runInIO $ return ()

----------------------------------------
-- Helpers

assertThrowsUnliftError
  :: IOE :> es
  => String -> Eff es a -> Eff es ()
assertThrowsUnliftError err = U.assertThrows err (\ErrorCall{} -> True)

inThread :: IO a -> IO a
inThread k = A.async k >>= A.wait
