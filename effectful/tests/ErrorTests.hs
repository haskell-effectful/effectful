module ErrorTests (errorTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic qualified as D
import Effectful.Error.Static

errorTests :: TestTree
errorTests = testGroup "Error"
  [ testCase "different handlers are independent" test_independentHandlers
  , testCase "call stack of dynamic throwError doesn't show internal details" test_dynamicThrowErrorCallStack
  , testCase "rethrowError attaches the given call stack" test_rethrowError
  ]

test_independentHandlers :: Assertion
test_independentHandlers = runEff $ do
  result <- runError @String . runOuterThrow $ do
    runError @String outerThrow
  liftIO $ case result of
    Left (cs, _) -> assertBool "stack trace points to the correct action" $
      "outerThrow" == fst (last $ getCallStack cs)
    Right _ -> assertFailure "error caught by the wrong (inner) handler"

test_dynamicThrowErrorCallStack :: Assertion
test_dynamicThrowErrorCallStack = do
  Left (cs, ()) <- runEff . D.runError @() $ D.throwError ()
  case getCallStack cs of
    [("throwError", _)] -> pure ()
    _ -> assertFailure $ "invalid call stack: " ++ prettyCallStack cs

test_rethrowError :: Assertion
test_rethrowError = runEff $ do
  result <- runError @Int . runError @String $ do
    originalThrow `catchError` \cs (_ :: String) -> rethrowError cs (42 :: Int)
  liftIO $ case result of
    Left (cs, e) -> do
      assertEqual "rethrown error" 42 e
      assertBool "stack trace points to the original throw" $
        "originalThrow" == fst (last $ getCallStack cs)
    Right _ -> assertFailure "error not rethrown"
  where
    originalThrow :: (HasCallStack, Error String :> es) => Eff es a
    originalThrow = throwError "oops"

----------------------------------------
-- Helpers

data OuterThrow :: Effect where
  OuterThrow :: OuterThrow m ()

type instance DispatchOf OuterThrow = Dynamic

outerThrow :: (HasCallStack, OuterThrow :> es) => Eff es ()
outerThrow = send OuterThrow

runOuterThrow :: Error String :> es => Eff (OuterThrow : es) a -> Eff es a
runOuterThrow = interpret_ $ \case
  OuterThrow -> throwError "outer"
