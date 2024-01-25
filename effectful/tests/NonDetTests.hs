module NonDetTests (nonDetTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.NonDet
import Effectful.Reader.Static
import Effectful.State.Dynamic

import Utils qualified as U

nonDetTests :: TestTree
nonDetTests = testGroup "NonDet"
  [ testCaseSteps "empty (left)"  $ test_empty leftEmpty
  , testCaseSteps "empty (right)" $ test_empty rightEmpty
  , testGroup "interaction"
    [ testCaseSteps "local state"  $ test_state evalStateLocal  expectedLocalState
    , testCaseSteps "shared state" $ test_state evalStateShared expectedSharedState
    ]
  , testCaseSteps "different handlers are independent" test_independentHandlers
  ]
  where
    leftEmpty :: NonDet :> es => Eff es Bool
    leftEmpty  = runReader () (empty >> pure False) <|> runReader () (pure True)

    rightEmpty :: NonDet :> es => Eff es Bool
    rightEmpty = runReader () (pure True) <|> runReader () (empty >> pure False)

    expectedLocalState :: OnEmptyPolicy -> Int
    expectedLocalState = \case
      OnEmptyKeep     -> 3
      OnEmptyRollback -> 2

    expectedSharedState :: OnEmptyPolicy -> Int
    expectedSharedState _ = 3

test_empty
  :: Eff [NonDet, IOE] Bool
  -> (String -> IO ())
  -> Assertion
test_empty test step = runEff $ do
  runNonDetBoth test $ \policy result -> do
    liftIO . step $ show policy
    U.assertEqual "result" (Just True) (dropLeft result)

test_state
  :: (forall r. Int -> Eff [State Int, IOE] r -> Eff '[IOE] r)
  -> (OnEmptyPolicy -> Int)
  -> (String -> IO ())
  -> IO ()
test_state evalState expectedState step = runEff $ do
  evalState 0 . runNonDetBoth test $ \policy result -> do
    liftIO . step $ show policy
    U.assertEqual "result" (Just ()) (dropLeft result)
    s <- state @Int $ \s -> (s, 0)
    U.assertEqual "state" (expectedState policy) s
  where
    test :: (NonDet :> es, State Int :> es) => Eff es ()
    test = (modify @Int (+1) >> empty) <|> modify @Int (+2)

test_independentHandlers :: (String -> IO ()) -> Assertion
test_independentHandlers step = runEff $ do
  runNonDetBoth test $ \policy result -> liftIO $ do
    step $ show policy
    case result of
      Left cs -> assertBool "stack trace points to the correct action" $
        "outerEmpty" == fst (last $ getCallStack cs)
      Right _ -> assertFailure "empty handled by the wrong (inner) handler"
  where
    test :: NonDet :> es => Eff es (Either CallStack Bool)
    test = runOuterEmpty . runNonDet OnEmptyKeep $ outerEmpty <|> pure True

----------------------------------------
-- Helpers

data OuterEmpty :: Effect where
  OuterEmpty :: OuterEmpty m a

type instance DispatchOf OuterEmpty = Dynamic

outerEmpty :: (HasCallStack, OuterEmpty :> es) => Eff es a
outerEmpty = send OuterEmpty

runOuterEmpty :: NonDet :> es => Eff (OuterEmpty : es) a -> Eff es a
runOuterEmpty = interpret $ \_ -> \case
  OuterEmpty -> emptyEff

----

dropLeft :: Either e a -> Maybe a
dropLeft = either (const Nothing) Just

runNonDetBoth
  :: Eff (NonDet : es) a
  -> (OnEmptyPolicy -> Either CallStack a -> Eff es ())
  -> Eff es ()
runNonDetBoth m k = do
  k OnEmptyKeep     =<< runNonDet OnEmptyKeep m
  k OnEmptyRollback =<< runNonDet OnEmptyRollback m
