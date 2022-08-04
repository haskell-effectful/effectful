module EnvTests (envTests) where

import Control.Exception
import Data.Either
import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Effectful.State.Static.Local
import qualified Utils as U

envTests :: TestTree
envTests = testGroup "Env"
  [ testCase "tailEnv works" test_tailEnv
  , testCase "subsume works" test_subsumeEnv
  , testCase "inject works" test_injectEnv
  , testCase "unsafeCoerce doesn't leak" test_noUnsafeCoerce
  , testCase "interpose works" test_interpose
  ]

test_tailEnv :: Assertion
test_tailEnv = runEff . evalState s0 $ do
  runReader () $ do
    runReader () $ do
      raise $ do
        runReader () $ do
          raise . raise $ do
            U.assertEqual "expected result" s0 =<< get
  where
    s0 :: Int
    s0 = 1337

test_subsumeEnv :: Assertion
test_subsumeEnv = runEff $ do
  s <- execState @Int 0 . subsume @(State Int) $ do
    modify @Int (+1)
    raise $ modify @Int (+2)
  U.assertEqual "exepcted result" 3 s

test_injectEnv :: Assertion
test_injectEnv = runEff $ runReader () $ do
  s <- execState @Int 0 $ inject action
  U.assertEqual "expected result" 15 s
  where
    action :: Eff [State Int, IOE, Reader (), State Int] ()
    action = do
      modify @Int (+1)
      raise $ do
        modify @Int (+2)
        inject innerAction

    innerAction :: Eff [State Int, State Int, IOE] ()
    innerAction = do
      modify @Int (+4)
      raise $ modify @Int (+8)

test_noUnsafeCoerce :: Assertion
test_noUnsafeCoerce = do
  r <- try @ErrorCall $ funToInt id
  assertBool "unsafeCoerce leaks" (isLeft r)
  where
    funToInt :: (a -> b) -> IO Int
    funToInt f = runEff $ do
      oops <- runReader @Int 0 $ do
        withEffToIO $ \unlift -> do
          pure . unlift $ ask @Int
      runReader f $ liftIO oops

----------------------------------------

test_interpose :: IO ()
test_interpose = runEff $ do
  runA . runB . runReader () $ do
    a1 <- send A
    b1 <- send B
    U.assertEqual "a1 original" 3 a1
    U.assertEqual "b1 original" 3 b1
    doubleA $ do
      a2 <- send A
      b2 <- send B
      U.assertEqual "a2 interposed" 6 a2
      U.assertEqual "b2 interposed" 6 b2
    a3 <- send A
    b3 <- send B
    U.assertEqual "a3 original" 3 a3
    U.assertEqual "b3 original" 3 b3

data A :: Effect where
  A :: A m Int
type instance DispatchOf A = Dynamic

runA :: IOE :> es => Eff (A : es) a -> Eff es a
runA = interpret $ \_ -> \case
  A -> pure 3

doubleA :: (IOE :> es, A :> es) => Eff es a -> Eff es a
doubleA = interpose $ \_ -> \case
  A -> (+) <$> send A <*> send A

data B :: Effect where
  B :: B m Int
type instance DispatchOf B = Dynamic

runB :: (IOE :> es, A :> es) => Eff (B : es) a -> Eff es a
runB = interpret $ \_ -> \case
  B -> send A
