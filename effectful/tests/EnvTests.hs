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
import Effectful.Provider
import Effectful.State.Static.Local
import Utils qualified as U

envTests :: TestTree
envTests = testGroup "Env"
  [ testCase "tailEnv works" test_tailEnv
  , testCase "subsume works" test_subsumeEnv
  , testCase "inject works" test_injectEnv
  , testCase "unsafeCoerce doesn't work" test_noUnsafeCoerce
  , testCase "interpose works" test_interpose
  , testCase "interpose/provider works" test_interposeProvider
  , testCase "borrow/lend works" test_borrowLend
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
  assertEnvSize "runEff" 2
  s <- execState @Int 0 $ inject action
  U.assertEqual "expected result" 127 s
  where
    action :: Eff [State Int, IOE, Reader (), State Int] ()
    action = do
      assertEnvSize "action" 4
      injectId $ do
        assertEnvSize "injectId" 4
        modify @Int (+1)
        raise $ do
          assertEnvSize "action.raise" 3
          modify @Int (+2)
          inject innerAction
        injectPure $
          assertEnvSize "injectPure" 0

    innerAction :: Eff [State Int, Reader (), State Int] ()
    innerAction = do
      assertEnvSize "innerAction" 3
      modify @Int (+4)
      raise $ do
        assertEnvSize "innerAction.raise" 2
        modify @Int (+8)
        only @(State Int) $ do
          assertEnvSize "only" 1
          modify @Int (+16)
      hideReader $ do
        assertEnvSize "hideReader" 2
        modify @Int (+32)
        onlyState $ do
          assertEnvSize "onlyState" 1
          modify @Int (+64)

    hideReader :: Eff (State s : es) r -> Eff (State s : Reader r : es) r
    hideReader = inject

    only :: e :> es => Eff '[e] a -> Eff es a
    only = inject

    onlyState :: Eff '[State s] a -> Eff (State s : es) a
    onlyState = inject

    injectId :: Eff es a -> Eff es a
    injectId = inject

    injectPure :: Eff '[] a -> Eff es a
    injectPure = inject

    assertEnvSize label n = unsafeEff $ \es -> do
      assertEqual ("expected Env size (" ++ label ++ ")") n =<< sizeEnv es

----------------------------------------

test_noUnsafeCoerce :: Assertion
test_noUnsafeCoerce = do
  r1 <- try @ErrorCall . evaluate $ unsafeCoerce1 @Int 'a'
  assertBool "unsafeCoerce1" (isLeft r1)

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

data Box a = Box a

----------------------------------------

test_interpose :: IO ()
test_interpose = runEff $ do
  runA 2 . runB . runReader () $ do
    a1 <- send A
    b1 <- send B
    U.assertEqual "a1 original" 2 a1
    U.assertEqual "b1 original" 2 b1
    doubleA $ do
      a2 <- send A
      b2 <- send B
      U.assertEqual "a2 interposed" 4 a2
      U.assertEqual "b2 interposed" 4 b2
    a3 <- send A
    b3 <- send B
    U.assertEqual "a3 original" 2 a3
    U.assertEqual "b3 original" 2 b3

test_interposeProvider :: IO ()
test_interposeProvider = runEff $ do
  runA 2 . runProvider_ (\() -> runB) $ do
    runA 3 . provide_ @B $ do
      b0 <- send B
      U.assertEqual "b0" 2 b0
    provide_ @B $ do
      b1 <- send B
      b2 <- doubleA $ send B
      U.assertEqual "b1" 2 b1
      U.assertEqual "b2" 4 b2
      runA 5 $ do
        b3 <- send B
        b4 <- doubleA $ send B
        b5 <- raise . doubleA $ send B
        U.assertEqual "b3" 2 b3
        U.assertEqual "b4" 2 b4
        U.assertEqual "b5" 4 b5
      doubleB $ do
        b6 <- send B
        b7 <- doubleA $ send B
        U.assertEqual "b6" 4 b6
        U.assertEqual "b7" 8 b7

data A :: Effect where
  A :: A m Int
type instance DispatchOf A = Dynamic

runA :: Int -> Eff (A : es) a -> Eff es a
runA n = interpret_ $ \case
  A -> pure n

doubleA :: A :> es => Eff es a -> Eff es a
doubleA = interpose_ $ \case
  A -> (+) <$> send A <*> send A

data B :: Effect where
  B :: B m Int
type instance DispatchOf B = Dynamic

runB :: A :> es => Eff (B : es) a -> Eff es a
runB = interpret_ $ \case
  B -> send A

doubleB :: B :> es => Eff es a -> Eff es a
doubleB = interpose_ $ \case
  B -> (+) <$> send B <*> send B

----------------------------------------

test_borrowLend :: Assertion
test_borrowLend = runEff $ do
  runX 1 2 . evalState @[Int] [3] . runReader () . runReader @[Int] [4] $ do
    U.assertEqual "expected result" [1,2,3,4,1,2,3,4] =<< send X

data X :: Effect where
  X :: (State [Int] :> es, Reader [Int] :> es, Reader () :> es) => X (Eff es) [Int]
type instance DispatchOf X = Dynamic

runX :: Int -> Int -> Eff (X : es) a -> Eff es a
runX s0 r0 = reinterpret (evalState s0 . evalState () . runReader r0) $ \env -> \case
  X -> localSeqUnlift env $ \unlift -> do
    as <- localSeqLend @[State Int, Reader Int] env $ \withHandlerEffs -> do
      unlift . withHandlerEffs $ do
        () <- ask
        s <- get @Int
        r <- ask @Int
        ss <- get @[Int]
        rs <- ask @[Int]
        pure $ [s, r] ++ ss ++ rs
    bs <- localSeqBorrow @[Reader [Int], State [Int], Reader ()] env $ \withEffs -> do
      withEffs $ do
        () <- ask
        s <- get @Int
        r <- ask @Int
        ss <- get @[Int]
        rs <- ask @[Int]
        pure $ [s, r] ++ ss ++ rs
    pure $ as ++ bs
