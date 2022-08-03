module EnvTests (envTests) where

import Control.Exception
import Data.Either
import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
import qualified Utils as U

envTests :: TestTree
envTests = testGroup "Env"
  [ testCase "tailEnv works" test_tailEnv
  , testCase "subsume works" test_subsumeEnv
  , testCase "inject works" test_injectEnv
  , testCase "unsafeCoerce doesn't leak" test_noUnsafeCoerce
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
