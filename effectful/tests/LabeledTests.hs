module LabeledTests (labeledTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Labeled
import Effectful.Reader.Static
import Utils qualified as U

labeledTests :: TestTree
labeledTests = testGroup "Labeled"
  [ testCase "labeled behaves correctly" $ test_labeledBehavior
  , testCase "(labeled . send) and (send . Labeled) behave the same" $ test_labeledSend
  ]

test_labeledBehavior :: Assertion
test_labeledBehavior = do
  v <- runEff
    . runLabeled @"a" (runReader "a")
    . runLabeled @"b" (runReader "b")
    . runReader "c"
    $ action
  assertEqual "expected result" "abc" v
  where
    action
      :: ( Labeled "a" (Reader String) :> es
         , Labeled "b" (Reader String) :> es
         , Reader String :> es
         )
      => Eff es String
    action = do
      a <- labeled @"b" @(Reader String) $ do
        labeled @"a" @(Reader String) $ do
          ask
      b <- labeled @"b" @(Reader String) $ do
        ask
      c <- ask
      pure $ a ++ b ++ c

test_labeledSend :: Assertion
test_labeledSend = runEff $ do
  runX 1 . runLabeled @"x" (runX 2) $ do
    v0 <- send X2
    U.assertEqual "expected result" 1 v0
    v1 <- (labeled @"x" @X . send) X2
    U.assertEqual "expected result" 2 v1
    v2 <- (send . Labeled @"x") X2
    U.assertEqual "expected result" 2 v2

data X :: Effect where
  X1 :: X m Int
  X2 :: X m Int

type instance DispatchOf X = Dynamic

runX :: Int -> Eff (X : es) a -> Eff es a
runX x = interpret $ \env -> \case
  X1 -> pure x
  X2 -> localSeqUnlift env $ \unlift -> unlift $ send X1
