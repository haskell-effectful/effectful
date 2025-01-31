module EarlyReturnTests (earlyReturnTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad (when)
import Effectful
import Effectful.EarlyReturn.Dynamic (runEarlyReturn, returnWith)
import           Effectful.Reader.Dynamic   (Reader, asks, runReader)
import Utils qualified as U

earlyReturnTests :: TestTree
earlyReturnTests = testGroup "EarlyReturn"
  [ testCase "early return" $ test_earlyReturn
  , testCase "early return within early return" $ test_nestedEarlyReturn
  , testCase "other effects with early return" $ test_earlyReturnOtherEffects
  ]

data Config = Config {
  foo  :: Int
}

helper1 :: forall es. HasCallStack => Int -> Eff es String
helper1 x = runEarlyReturn $ do
  when (x < 10) $ do
    returnWith "lt10"
  when (x < 20) $ do
    returnWith "lt20"
  pure "gte20"

helper2 :: forall es. HasCallStack => Int -> Eff es Int
helper2 x = runEarlyReturn $ do
  when (x < 20) $ do
    y <- helper1 x
    returnWith @Int $ if y == "lt10" then 1 else 2
  pure 3

earlyAndReaderHelper :: forall es. (HasCallStack, Reader Config :> es) => Eff es Bool
earlyAndReaderHelper = runEarlyReturn $ do
  x <- asks foo
  when (x < 10) $ do
    returnWith @Bool True
  pure False

test_earlyReturn
  :: Assertion
test_earlyReturn = runEff $ do
  val1 <- helper1 5
  U.assertEqual "val1" val1 "lt10"
  val2 <- helper1 30
  U.assertEqual "val2" val2 "gte20"

test_nestedEarlyReturn
  :: Assertion
test_nestedEarlyReturn = runEff $ do
  val1 <- helper2 1
  U.assertEqual "val1" val1 1
  val2 <- helper2 15
  U.assertEqual "val2" val2 2
  val3 <- helper2 25
  U.assertEqual "val3" val3 3

test_earlyReturnOtherEffects
  :: Assertion
test_earlyReturnOtherEffects = do
  let config1 = Config { foo = 1 }
  runEff $ runReader config1 $ do
    val1 <- earlyAndReaderHelper
    U.assertEqual "val1" val1 True

  let config2 = Config { foo = 20 }
  runEff $ runReader config2 $ do
    val2 <- earlyAndReaderHelper
    U.assertEqual "val2" val2 False

