module Main (main) where

import Control.Concurrent.Async.Lifted
import Control.Monad.IO.Class
import Test.Tasty
import Test.Tasty.HUnit
import qualified Control.Monad.Catch as E
import qualified Control.Exception.Lifted as LE
import qualified UnliftIO.Exception as UE

import Effectful
import Effectful.State

main :: IO ()
main = defaultMain $ testGroup "effectful"
  [ testGroup "state"
    [ testCase "runState & execState" test_runState
    , testCase "evalState" test_evalState
    , testCase "stateM" test_stateM
    , testCase "deep stack" test_deepStack
    , testCase "exceptions" test_exceptions
    , testCase "concurrency" test_concurrentState
    ]
  ]

test_runState :: Assertion
test_runState = runEffIO $ do
  (end, len) <- runState (0::Int) . execState collatzStart $ collatz
  assertEqual_ "correct end" 1 end
  assertEqual_ "correct len" collatzLength len

test_evalState :: Assertion
test_evalState = runEffIO $ do
  len <- evalState (0::Int) . evalState collatzStart $ collatz *> get @Int
  assertEqual_ "correct len" collatzLength len

test_stateM :: Assertion
test_stateM = runEffIO $ do
  (a, b) <- runState "hi" . stateM $ \s -> pure (s, s ++ "!!!")
  assertEqual_ "correct a" "hi"    a
  assertEqual_ "correct b" "hi!!!" b

test_deepStack :: Assertion
test_deepStack = runEffIO $ do
  n <- evalState () . execState (0::Int) $ do
    evalState () . evalState () $ do
      evalState () $ do
        evalState () . evalState () . evalState () $ do
          modify @Int (+1)
        modify @Int (+2)
      modify @Int (+4)
    modify @Int (+8)
  assertEqual_ "n" 15 n

test_exceptions :: Assertion
test_exceptions = runEffIO $ do
  testTry   "exceptions"  E.try
  testCatch "exceptions"  E.catch
  testTry   "lifted-base" LE.try
  testCatch "lifted-base" LE.catch
  testTry   "unliftio"    UE.try
  testCatch "unliftio"    UE.catch
  where
    testTry
      :: String
      -> (forall a es. IOE :> es => Eff es a -> Eff es (Either Ex a))
      -> Eff '[IOE] ()
    testTry lib tryImpl = do
      e <- tryImpl $ runState (0::Int) action
      assertEqual_ (lib ++ " - exception caught") e (Left Ex)
      s <- execState (0::Int) $ tryImpl action
      assertEqual_ (lib ++ " - state partially updated") s 1

    testCatch
      :: String
      -> (forall a es. IOE :> es => Eff es a -> (Ex -> Eff es a) -> Eff es a)
      -> Eff '[IOE] ()
    testCatch lib catchImpl = do
      s <- execState (0::Int) $ do
        _ <- (evalState () action) `catchImpl` \Ex -> modify @Int (+4)
        modify @Int (+8)
      assertEqual_ (lib ++ " - state correctly updated") s 13

    action :: State Int :> es => Eff es ()
    action = do
      modify @Int (+1)
      _ <- E.throwM Ex
      modify @Int (+2)

test_concurrentState :: Assertion
test_concurrentState = runEffIO . evalState x $ do
  replicateConcurrently_ 2 $ do
    r <- goDownward 0
    assertEqual_ "x = n" x r
  where
    x :: Int
    x = 1000000

    goDownward :: State Int :> es => Int -> Eff es Int
    goDownward acc = get @Int >>= \case
      0 -> pure acc
      n -> do
        put $ n - 1
        goDownward $ acc + 1

----------------------------------------
-- Helpers

data Ex = Ex deriving (Eq, Show)
instance E.Exception Ex

runEffIO :: Eff '[IOE] a -> IO a
runEffIO = runEff . runIOE

assertEqual_ :: (Eq a, Show a, IOE :> es) => String -> a -> a -> Eff es ()
assertEqual_ msg expected given = liftIO $ assertEqual msg expected given

----------------------------------------

collatzStart :: Integer
collatzStart = 9780657630

collatzLength :: Int
collatzLength = 1132

-- | Tests multiple 'State'S, 'put', 'get' and 'modify'.
collatz :: (State Integer :> es, State Int :> es) => Eff es ()
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
