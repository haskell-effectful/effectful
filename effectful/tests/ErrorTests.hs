module ErrorTests (errorTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static

errorTests :: TestTree
errorTests = testGroup "Error"
  [ testCase "different handlers are independent" test_independentHandlers
  ]

test_independentHandlers :: Assertion
test_independentHandlers = runEff $ do
  result <- runError @String . runOuterThrow $ do
    runError @String outerThrow
  liftIO $ case result of
    Left (cs, _) -> assertBool "stack trace points to the correct action" $
      "outerThrow" == fst (last $ getCallStack cs)
    Right _ -> assertFailure "error caught by the wrong (inner) handler"

----------------------------------------
-- Helpers

data OuterThrow :: Effect where
  OuterThrow :: OuterThrow m ()

type instance DispatchOf OuterThrow = Dynamic

outerThrow :: (HasCallStack, OuterThrow :> es) => Eff es ()
outerThrow = send OuterThrow

runOuterThrow :: Error String :> es => Eff (OuterThrow : es) a -> Eff es a
runOuterThrow = interpret $ \_ -> \case
  OuterThrow -> throwError "outer"
