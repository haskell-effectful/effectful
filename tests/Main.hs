module Main (main) where

import Control.Monad.IO.Class
import Test.Tasty
import Test.Tasty.HUnit
import qualified Control.Monad.Catch as E
import qualified Control.Exception.Lifted as LE
import qualified UnliftIO.Exception as UE

import Effectful.Interpreter
import Effectful.State.Dynamic
import Effectful.Monad

main :: IO ()
main = defaultMain $ testGroup "effectful"
  [ testGroup "state"
    [ testCase "runState & execState" test_runState
    , testCase "evalState" test_evalState
    , testCase "stateM" test_stateM
    , testCase "deep stack" test_deepStack
    , testCase "exceptions" test_exceptions
    , testCase "local effects" test_localEffects
    ]
  ]

test_runState :: Assertion
test_runState = runIOE $ do
  (end, len) <- runState (0::Int) . execState collatzStart $ collatz
  assertEqual_ "correct end" 1 end
  assertEqual_ "correct len" collatzLength len

test_evalState :: Assertion
test_evalState = runIOE $ do
  len <- evalState (0::Int) . evalState collatzStart $ collatz *> get @Int
  assertEqual_ "correct len" collatzLength len

test_stateM :: Assertion
test_stateM = runIOE $ do
  (a, b) <- runState "hi" . stateM $ \s -> pure (s, s ++ "!!!")
  assertEqual_ "correct a" "hi"    a
  assertEqual_ "correct b" "hi!!!" b

test_deepStack :: Assertion
test_deepStack = runIOE $ do
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
test_exceptions = runIOE $ do
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

test_localEffects :: Assertion
test_localEffects = runIOE $ do
  x <- runHasInt 0 $ do
    putInt 1
    execState () $ do
      putInt 2
      execState () $ do
        putInt 4
    getInt
  assertEqual_ "correct x" 4 x

runHasInt :: Int -> Eff (HasInt : es) a -> Eff es a
runHasInt n =
  -- reinterpret with redundant local effects
  reinterpret (evalState () . evalState n . evalState True) $ \case
    GetInt   -> get
    PutInt i -> put i

data HasInt :: Effect where
  GetInt :: HasInt m Int
  PutInt :: Int -> HasInt m ()

getInt :: HasInt :> es => Eff es Int
getInt = send GetInt

putInt :: HasInt :> es => Int -> Eff es ()
putInt = send . PutInt

----------------------------------------
-- Helpers

data Ex = Ex deriving (Eq, Show)
instance E.Exception Ex

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
