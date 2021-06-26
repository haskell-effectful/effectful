module AsyncTests (asyncTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful.Async
import Effectful.State.Dynamic
import Effectful.Monad
import Utils

asyncTests :: TestTree
asyncTests = testGroup "Async"
  [ testCase "thread local state" test_threadLocalState
  ]

test_threadLocalState :: Assertion
test_threadLocalState = runIOE . evalState x $ do
  runAsyncE . replicateConcurrently_ 2 $ do
    r <- goDownward 0
    assertEqual_ "x = n" x r
  where
    x :: Int
    x = 1000000

    goDownward :: State Int :> es => Int -> Eff es Int
    goDownward acc = do
      end <- state @Int $ \case
        0 -> (True,  0)
        n -> (False, n - 1)
      if end
        then pure acc
        else goDownward $ acc + 1
