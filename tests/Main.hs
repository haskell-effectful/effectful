module Main (main) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Test.Tasty
import Test.Tasty.HUnit

import Effective
import Effective.State

main :: IO ()
main = defaultMain $ testGroup "effective"
  [ testGroup "state"
    [ testCase "runState & execState" test_runState
    , testCase "evalState" test_evalState
    , testCase "stateM" test_stateM
    , testCase "deep stack" test_deepStack
    , testCase "exceptions" test_exceptions
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
  e <- try @_ @Ex $ runState (0::Int) action
  assertEqual_ "exception caught" e (Left Ex)

  s1 <- execState (0::Int) $ try @_ @Ex action
  assertEqual_ "state partially updated" s1 1

  s2 <- execState (0::Int) $ do
    evalState () action `catch` \Ex -> modify @Int (+4)
    modify @Int (+8)
  assertEqual_ "state correctly updated" s2 13
  where
    action :: State Int :> es => Eff es ()
    action = do
      modify @Int (+1)
      _ <- throwM Ex
      modify @Int (+2)

----------------------------------------
-- Helpers

data Ex = Ex deriving (Eq, Show)
instance Exception Ex

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
