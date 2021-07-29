module ErrorTests (errorTests) where

import Control.Monad.IO.Class
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
  result <- runErrorE @String . runNestedErr $ do
    runErrorE @String nestedErr
  liftIO $ case result of
    Left (cs, _) -> assertBool "stack trace points to the correct action" $
      "nestedErr" == fst (last $ getCallStack cs)
    Right _ -> assertFailure "error caught by the wrong (inner) handler"

----------------------------------------
-- Helpers

data NestedErrE :: Effect where
  NestedErr :: NestedErrE m ()

nestedErr :: (HasCallStack, NestedErrE :> es) => Eff es ()
nestedErr = send NestedErr

runNestedErr :: ErrorE String :> es => Eff (NestedErrE : es) a -> Eff es a
runNestedErr = interpret $ \_ -> \case
  NestedErr -> throwError "nested error"
