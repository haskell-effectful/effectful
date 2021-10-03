{-# LANGUAGE OverloadedLabels #-}
module UnliftTests (unliftTests) where

import Optics.Core
import Test.Tasty
import Test.Tasty.HUnit
import qualified UnliftIO.Async as A

import Effectful
import qualified Utils as U

unliftTests :: TestTree
unliftTests = testGroup "Unlift"
  [ testCase "Reset strategy in new thread" test_reset_strategy
  , testCase "SeqUnlift in new thread" test_seqUnlift_in_new_thread
  , testGroup "Ephemeral strategy"
    [ testCase "Invalid limit" test_ephemeral_invalid
    , testCase "Uses in same thread" test_ephemeral_same_thread
    , testCase "Uses in multiple threads" test_ephemeral_multiple_threads
    ]
  , testGroup "Persistent strategy"
    [ testCase "Invalid limit" test_persistent_invalid
    , testCase "Uses in same thread" test_persistent_same_thread
    , testCase "Uses in multiple threads" test_persistent_multiple_threads
    ]
  ]

test_reset_strategy :: Assertion
test_reset_strategy = runEff $ do
  s <- withUnliftStrategy (ConcUnlift Ephemeral Unlimited) $ do
    withEffToIO $ \runInIO -> do
      inThread $ runInIO unliftStrategy
  U.assertEqual "correct strategy" SeqUnlift s

test_seqUnlift_in_new_thread :: Assertion
test_seqUnlift_in_new_thread = runEff $ do
  assertThrowsUnliftError "InvalidUseOfSeqUnlift error"
    (has #_InvalidUseOfSeqUnlift) $ do
      withUnliftStrategy SeqUnlift $ do
        withEffToIO $ \runInIO -> do
          inThread $ runInIO $ return ()

test_ephemeral_invalid :: Assertion
test_ephemeral_invalid = runEff $ do
  assertThrowsUnliftError "InvalidNumberOfUses error"
    (has #_InvalidNumberOfUses) $ do
      withUnliftStrategy (ConcUnlift Ephemeral $ Limited 0) $ do
        withEffToIO $ \_ -> return ()

test_ephemeral_same_thread :: Assertion
test_ephemeral_same_thread = runEff $ do
  assertThrowsUnliftError "ExceededNumberOfUses error"
    (has #_ExceededNumberOfUses) $ do
      withUnliftStrategy (ConcUnlift Ephemeral $ Limited 1) $ do
        withEffToIO $ \runInIO -> inThread $ do
          runInIO $ return ()
          runInIO $ return ()

test_ephemeral_multiple_threads :: Assertion
test_ephemeral_multiple_threads = runEff $ do
  assertThrowsUnliftError "ExceededNumberOfUses error"
    (has #_ExceededNumberOfUses) $ do
      withUnliftStrategy (ConcUnlift Ephemeral $ Limited 1) $ do
        withEffToIO $ \runInIO -> do
          inThread $ runInIO $ return ()
          inThread $ runInIO $ return ()

test_persistent_invalid :: Assertion
test_persistent_invalid = runEff $ do
  assertThrowsUnliftError "InvalidNumberOfThreads error"
    (has #_InvalidNumberOfThreads) $ do
      withUnliftStrategy (ConcUnlift Persistent $ Limited 0) $ do
        withEffToIO $ \_ -> return ()

test_persistent_same_thread :: Assertion
test_persistent_same_thread = runEff $ do
  withUnliftStrategy (ConcUnlift Persistent $ Limited 1) $ do
    withEffToIO $ \runInIO -> inThread $ do
      runInIO $ return ()
      runInIO $ return ()

test_persistent_multiple_threads :: Assertion
test_persistent_multiple_threads = runEff $ do
  assertThrowsUnliftError "ExceededNumberOfThreads error"
    (has #_ExceededNumberOfThreads) $ do
      withUnliftStrategy (ConcUnlift Persistent $ Limited 1) $ do
        withEffToIO $ \runInIO -> do
          inThread $ runInIO $ return ()
          inThread $ runInIO $ return ()

----------------------------------------
-- Helpers

assertThrowsUnliftError
  :: IOE :> es
  => String -> (UnliftError -> Bool) -> Eff es a -> Eff es ()
assertThrowsUnliftError = U.assertThrows

inThread :: IO a -> IO a
inThread k = A.async k >>= A.wait
