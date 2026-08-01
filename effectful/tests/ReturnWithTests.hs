module ReturnWithTests (returnWithTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception (finally)
import Effectful.Labeled.ReturnWith qualified as LR
import Effectful.ReturnWith.Dynamic qualified as RD
import Effectful.ReturnWith.Static qualified as RS
import Effectful.State.Static.Local
import Utils qualified as U

returnWithTests :: TestTree
returnWithTests = testGroup "ReturnWith"
  [ testCase "static short-circuits" test_static
  , testCase "static falls through" test_staticFallThrough
  , testCase "different handlers are independent" test_independentHandlers
  , testCase "cleanup actions run on early return" test_cleanup
  , testCase "dynamic short-circuits" test_dynamic
  , testCase "labeled handlers are targeted correctly" test_labeled
  ]

test_static :: Assertion
test_static = runEff . evalState @Int 0 $ do
  r <- RS.runReturnWith @String $ do
    modify @Int (+1)
    _ <- RS.returnWith "early"
    modify @Int (+1)
    pure "late"
  U.assertEqual "result" "early" r
  U.assertEqual "state changes before returnWith persist" 1 =<< get @Int

test_staticFallThrough :: Assertion
test_staticFallThrough = runEff $ do
  r <- RS.runReturnWith @String $ pure "done"
  U.assertEqual "result" "done" r

test_independentHandlers :: Assertion
test_independentHandlers = runEff $ do
  r <- RS.runReturnWith @String . runOuterReturn $ do
    inner <- RS.runReturnWith @String $ do
      outerReturn
      pure "inner"
    pure $ "inner handler caught " ++ inner
  U.assertEqual "correct value returned" "outer" r

test_cleanup :: Assertion
test_cleanup = runEff . evalState @Int 0 $ do
  r <- RS.runReturnWith @String $ do
    (RS.returnWith "early" >> pure "late") `finally` modify @Int (+1)
  U.assertEqual "result" "early" r
  U.assertEqual "cleanup ran" 1 =<< get @Int

test_dynamic :: Assertion
test_dynamic = runEff . evalState @Int 0 $ do
  r <- RD.runReturnWith @String $ do
    modify @Int (+1)
    _ <- RD.returnWith "early"
    modify @Int (+1)
    pure "late"
  U.assertEqual "result" "early" r
  U.assertEqual "state changes before returnWith persist" 1 =<< get @Int

test_labeled :: Assertion
test_labeled = runEff $ do
  r <- LR.runReturnWith @"outer" @String $ do
    n <- LR.runReturnWith @"inner" @Int $ do
      _ <- LR.returnWith @"outer" "outer wins"
      pure 0
    pure $ "inner returned " ++ show n
  U.assertEqual "value caught by the outer handler" "outer wins" r

----------------------------------------
-- Helpers

data OuterReturn :: Effect where
  OuterReturn :: OuterReturn m ()

type instance DispatchOf OuterReturn = Dynamic

outerReturn :: OuterReturn :> es => Eff es ()
outerReturn = send OuterReturn

runOuterReturn :: RS.ReturnWith String :> es => Eff (OuterReturn : es) a -> Eff es a
runOuterReturn = interpret_ $ \case
  OuterReturn -> RS.returnWith "outer"
