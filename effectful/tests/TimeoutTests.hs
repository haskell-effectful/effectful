module TimeoutTests (timeoutTests) where

import Control.Concurrent (threadDelay)
import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Timeout
import Utils qualified as U

timeoutTests :: TestTree
timeoutTests = testGroup "Timeout"
  [ testCase "return before timeout" test_returnBeforeTimeout
  , testCase "timeout before return" test_timeoutBeforeReturn
  ]

test_returnBeforeTimeout :: Assertion
test_returnBeforeTimeout = runEff $ do
  result <- runTimeout $ timeout 1000 $ return ()
  U.assertEqual "return value" (Just ()) result

test_timeoutBeforeReturn :: Assertion
test_timeoutBeforeReturn = runEff $ do
  result <- runTimeout $ timeout 0 $ liftIO $ threadDelay 1000
  U.assertEqual "no return value" Nothing result
