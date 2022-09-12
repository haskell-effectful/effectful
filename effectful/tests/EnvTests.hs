module EnvTests (envTests) where

import Control.Exception
import Data.Either
import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import Effectful.Reader.Static
import Effectful.State.Static.Local
import qualified Utils as U

envTests :: TestTree
envTests = testGroup "Env"
  [ testCase "tailEnv works" test_tailEnv
  , testCase "subsume works" test_subsumeEnv
  , testCase "inject works" test_injectEnv
  , testCase "unsafeCoerce doesn't work" test_noUnsafeCoerce
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
test_injectEnv = runEff . runReader () $ do
  s <- execState @Int 0 $ inject action
  U.assertEqual "expected result" 63 s
  where
    action :: Eff [State Int, IOE, Reader (), State Int] ()
    action = do
      modify @Int (+1)
      raise $ do
        modify @Int (+2)
        inject innerAction

    innerAction :: Eff [State Int, Reader (), State Int] ()
    innerAction = do
      modify @Int (+4)
      raise $ modify @Int (+8)
      hideReader $ do
        modify @Int (+16)
        raise $ modify @Int (+32)

    hideReader :: Eff (State s : es) r -> Eff (State s : Reader r : es) r
    hideReader = inject

----------------------------------------

test_noUnsafeCoerce :: Assertion
test_noUnsafeCoerce = do
  r1 <- try @ErrorCall . evaluate $ unsafeCoerce1 @Int 'a'
  assertBool "unsafeCoerce1" (isLeft r1)
  r2 <- try @ErrorCall . evaluate $ unsafeCoerce2 @Int 'a'
  assertBool "unsafeCoerce2" (isLeft r2)

unsafeCoerce1 :: forall b a. a -> b
unsafeCoerce1 a = runPureEff $ do
  -- 'oops' gains access to the effect stack with Reader (Box b) via the
  -- unlifting function that escaped its scope. The problem here is that this
  -- effect is no longer in scope.
  oops <- runReader @(Box b) (Box undefined) $ do
    raiseWith SeqUnlift $ \unlift -> do
      pure . unlift $ ask @(Box b)
  -- Put Reader (Box a) where the Reader (Box b) was before and attempt to
  -- retrieve 'a' coerced to 'b'. It should fail because 'getLocation' in
  -- 'Effectful.Internal.Env' checks that version of the reference is the same
  -- as version of the effect.
  Box b <- runReader (Box a) $ raise oops
  pure b

unsafeCoerce2 :: forall b a. a -> b
unsafeCoerce2 a = runPureEff $ do
  backupEs <- unsafeEff cloneEnv
  -- 'oops' gains access to the effect stack with Reader (Box b) via the
  -- unlifting function that escaped its scope. The problem here is that this
  -- effect is no longer in scope.
  oops <- runReader @(Box b) (Box undefined) $ do
    raiseWith SeqUnlift $ \unlift -> do
      pure . unlift $ ask @(Box b)
  -- If restoreEnv messes up versioning (i.e. restores the version counter from
  -- before the above Reader was used), below code will succeed because the
  -- Reader (Box a) will have the same version as Reader (Box b).
  unsafeEff $ \es -> restoreEnv es backupEs
  -- Put Reader (Box a) where the Reader (Box b) was before and attempt to
  -- retrieve 'a' coerced to 'b'. It should fail because 'getLocation' in
  -- 'Effectful.Internal.Env' checks that version of the reference is the same
  -- as version of the effect.
  Box b <- runReader (Box a) $ raise oops
  pure b

data Box a = Box a

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
