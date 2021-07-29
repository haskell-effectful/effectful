module StateTests (stateTests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Control.Monad.Catch as E
import qualified Control.Exception.Lifted as LE
import qualified UnliftIO.Exception as UE

import Effectful
import Effectful.State.Local
import qualified Utils as U

stateTests :: TestTree
stateTests = testGroup "State"
  [ testCase "runState & execState" test_runState
  , testCase "evalState" test_evalState
  , testCase "stateM" test_stateM
  , testCase "deep stack" test_deepStack
  , testCase "exceptions" test_exceptions
  , testCase "local effects" test_localEffects
  ]

test_runState :: Assertion
test_runState = runEff $ do
  (end, len) <- runStateE (0::Int) . execStateE collatzStart $ collatz
  U.assertEqual "correct end" 1 end
  U.assertEqual "correct len" collatzLength len

test_evalState :: Assertion
test_evalState = runEff $ do
  len <- evalStateE (0::Int) . evalStateE collatzStart $ collatz *> get @Int
  U.assertEqual "correct len" collatzLength len

test_stateM :: Assertion
test_stateM = runEff $ do
  (a, b) <- runStateE "hi" . stateM $ \s -> pure (s, s ++ "!!!")
  U.assertEqual "correct a" "hi"    a
  U.assertEqual "correct b" "hi!!!" b

test_deepStack :: Assertion
test_deepStack = runEff $ do
  n <- evalStateE () . execStateE (0::Int) $ do
    evalStateE () . evalStateE () $ do
      evalStateE () $ do
        evalStateE () . evalStateE () . evalStateE () $ do
          modify @Int (+1)
        modify @Int (+2)
      modify @Int (+4)
    modify @Int (+8)
  U.assertEqual "n" 15 n

test_exceptions :: Assertion
test_exceptions = runEff $ do
  testTry   "exceptions"  E.try
  testCatch "exceptions"  E.catch
  testTry   "lifted-base" LE.try
  testCatch "lifted-base" LE.catch
  testTry   "unliftio"    UE.try
  testCatch "unliftio"    UE.catch
  where
    testTry
      :: String
      -> (forall a es. IOE :> es => Eff es a -> Eff es (Either U.Ex a))
      -> Eff '[IOE] ()
    testTry lib tryImpl = do
      e <- tryImpl $ runStateE (0::Int) action
      U.assertEqual (lib ++ " - exception caught") e (Left U.Ex)
      s <- execStateE (0::Int) $ tryImpl action
      U.assertEqual (lib ++ " - state partially updated") s 1

    testCatch
      :: String
      -> (forall a es. IOE :> es => Eff es a -> (U.Ex -> Eff es a) -> Eff es a)
      -> Eff '[IOE] ()
    testCatch lib catchImpl = do
      s <- execStateE (0::Int) $ do
        _ <- (evalStateE () action) `catchImpl` \U.Ex -> modify @Int (+4)
        modify @Int (+8)
      U.assertEqual (lib ++ " - state correctly updated") s 13

    action :: StateE Int :> es => Eff es ()
    action = do
      modify @Int (+1)
      _ <- E.throwM U.Ex
      modify @Int (+2)

test_localEffects :: Assertion
test_localEffects = runEff $ do
  x <- runHasIntE 0 $ do
    putInt 1
    execStateE () $ do
      putInt 2
      execStateE () $ do
        putInt expected
    getInt
  U.assertEqual "correct x" expected x
  where
    expected :: Int
    expected = 4

----------------------------------------
-- Helpers

data HasIntE :: Effect where
  GetInt :: HasIntE m Int
  PutInt :: Int -> HasIntE m ()

getInt :: HasIntE :> es => Eff es Int
getInt = send GetInt

putInt :: HasIntE :> es => Int -> Eff es ()
putInt = send . PutInt

runHasIntE :: Int -> Eff (HasIntE : es) a -> Eff es a
runHasIntE n =
  -- reinterpret with redundant local effects
  reinterpret (evalStateE () . evalStateE n . evalStateE True) $ \_ -> \case
    GetInt   -> get
    PutInt i -> put i

----------------------------------------
-- Helpers

collatzStart :: Integer
collatzStart = 9780657630

collatzLength :: Int
collatzLength = 1132

-- | Tests multiple 'State'S, 'put', 'get' and 'modify'.
collatz :: (StateE Integer :> es, StateE Int :> es) => Eff es ()
collatz = get @Integer >>= \case
  1 -> pure ()
  n -> if even n
       then do put $ n `div` 2
               modify @Int (+1)
               collatz
       else do put $ 3*n + 1
               modify @Int (+1)
               collatz
{-# NOINLINE collatz #-}
