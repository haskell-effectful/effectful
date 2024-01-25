module UnliftTests (unliftTests) where

import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO.Async qualified as A

import Effectful
import Utils qualified as U

unliftTests :: TestTree
unliftTests = testGroup "Unlift"
  [ testCase "Strategy stays the same in a new thread" test_threadStrategy
  , testCase "SeqUnlift in new thread" test_seqUnliftInNewThread
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
