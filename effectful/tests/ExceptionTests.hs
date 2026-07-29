{-# LANGUAGE CPP #-}
module ExceptionTests (exceptionTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Exception
import Effectful.State.Static.Local

import Utils qualified as U

exceptionTests :: TestTree
exceptionTests = testGroup "Exception"
  [ testCase "cleanup actions run in the correct order" test_cleanupOrder
  , testCase "cleanup actions run masked" test_cleanupMasking
#if MIN_VERSION_base(4,21,0)
  , testCase "throwing cleanup action doesn't lose the original exception"
    test_cleanupKeepsOriginalException
#endif
  ]

test_cleanupOrder :: Assertion
test_cleanupOrder = runEff $ do
  checkOrder "bracket (success)" ["acquire", "action", "release"] $
    bracket (record "acquire") (\_ -> record "release") (\_ -> record "action")
  checkOrder "bracket (failure)" ["acquire", "action", "release"] $
    bracket (record "acquire") (\_ -> record "release") (\_ -> failingAction)
  checkOrder "bracket_ (failure)" ["acquire", "action", "release"] $
    bracket_ (record "acquire") (record "release") failingAction
  checkOrder "bracketOnError (success)" ["acquire", "action"] $
    bracketOnError (record "acquire") (\_ -> record "release") (\_ -> record "action")
  checkOrder "bracketOnError (failure)" ["acquire", "action", "release"] $
    bracketOnError (record "acquire") (\_ -> record "release") (\_ -> failingAction)
  checkOrder "finally (success)" ["action", "release"] $
    record "action" `finally` record "release"
  checkOrder "finally (failure)" ["action", "release"] $
    failingAction `finally` record "release"
  checkOrder "onException (success)" ["action"] $
    record "action" `onException` record "release"
  checkOrder "onException (failure)" ["action", "release"] $
    failingAction `onException` record "release"
  where
    record :: State [String] :> es => String -> Eff es ()
    record msg = modify (msg :)

    failingAction :: State [String] :> es => Eff es ()
    failingAction = record "action" >> throwIO Original

    checkOrder
      :: (HasCallStack, IOE :> es)
      => String
      -> [String]
      -> Eff (State [String] : es) a
      -> Eff es ()
    checkOrder name expected action = do
      recorded <- execState [] . trySync $ action
      U.assertEqual name expected (reverse recorded)

test_cleanupMasking :: Assertion
test_cleanupMasking = runEff $ do
  bracket getMaskingState
    (\acquire -> do
        release <- getMaskingState
        U.assertEqual "acquire is masked" MaskedInterruptible acquire
        U.assertEqual "release is masked" MaskedInterruptible release
    )
    (\_ -> do
        action <- getMaskingState
        U.assertEqual "action is unmasked" Unmasked action
    )

#if MIN_VERSION_base(4,21,0)
test_cleanupKeepsOriginalException :: Assertion
test_cleanupKeepsOriginalException = runEff $ do
  check "bracket" $
    bracket (pure ()) (\_ -> failingCleanup) (\_ -> failingAction)
  check "bracket_" $
    bracket_ (pure ()) failingCleanup failingAction
  check "bracketOnError" $
    bracketOnError (pure ()) (\_ -> failingCleanup) (\_ -> failingAction)
  check "finally" $
    failingAction `finally` failingCleanup
  check "onException" $
    failingAction `onException` failingCleanup
  check "withException" $
    withException failingAction (\Original -> failingCleanup)
  where
    failingAction, failingCleanup :: Eff es ()
    failingAction = throwIO Original
    failingCleanup = throwIO Cleanup

    check :: (HasCallStack, IOE :> es) => String -> Eff es () -> Eff es ()
    check name action = trySync action >>= \case
      Right () -> U.assertFailure $ name ++ ": no exception was thrown"
      Left ex -> do
        U.assertBool (name ++ ": exception of the cleanup action propagates")
          $ isCleanup ex
        U.assertBool (name ++ ": the original exception is preserved")
          $ any isOriginal (whileHandling ex)

    -- | Exceptions recorded in the 'WhileHandling' annotations of the given one.
    whileHandling :: SomeException -> [SomeException]
    whileHandling ex =
      [ orig
      | WhileHandling orig <- getExceptionAnnotations (someExceptionContext ex)
      ]

    isOriginal :: SomeException -> Bool
    isOriginal ex = case fromException ex of
      Just Original -> True
      Nothing -> False

    isCleanup :: SomeException -> Bool
    isCleanup ex = case fromException ex of
      Just Cleanup -> True
      Nothing -> False
#endif

----------------------------------------
-- Helpers

data Original = Original
  deriving stock (Eq, Show)
instance Exception Original

#if MIN_VERSION_base(4,21,0)
data Cleanup = Cleanup
  deriving stock (Eq, Show)
instance Exception Cleanup
#endif
