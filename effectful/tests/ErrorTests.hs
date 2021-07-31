module ErrorTests (errorTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Error

errorTests :: TestTree
errorTests = testGroup "Error"
  [ testCase "error from interpret" test_errorFromInterpret
  ]

test_errorFromInterpret :: Assertion
test_errorFromInterpret = runEff $ do
  result <- runError @String . runNestedErr $ do
    runError @String nestedErr
  liftIO $ case result of
    Left (cs, _) -> assertBool "stack trace points to the correct action" $
      "nestedErr" == fst (last $ getCallStack cs)
    Right _ -> assertFailure "error caught by the wrong (inner) handler"

----------------------------------------
-- Helpers

data NestedErr :: Effect where
  NestedErr :: NestedErr m ()

nestedErr :: (HasCallStack, NestedErr :> es) => Eff es ()
nestedErr = send NestedErr

runNestedErr :: Error String :> es => Eff (NestedErr : es) a -> Eff es a
runNestedErr = interpret $ \_ -> \case
  NestedErr -> throwError "nested error"
