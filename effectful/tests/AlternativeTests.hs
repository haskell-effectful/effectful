module AlternativeTests (alternativeTests) where

import Data.Bifunctor (second)
import Control.Applicative ((<|>), empty)
import Control.Exception.Lifted (catch)
import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Dispatch.Static (unsafeEff)
import Effectful.Internal.Env (Env(..))
import Effectful.State.Dynamic

import qualified Utils as U

alternativeTests :: TestTree
alternativeTests = testGroup "Alternative"
  [ testCase "empty" test_empty
  , testGroup "left"
    [ testCase "identity" test_leftIdentity
    , testCase "interpose" test_leftInterpose
    ]
  , testGroup "right"
    [ testCase "identity" test_rightIdentity
    , testCase "interpose" test_rightInterpose
    ]
  ]

test_empty :: Assertion
test_empty = runEff $ do
  r <- (empty >> pure False) `catch` (\EmptyAlternative -> pure True)
  U.assertEqual "expected result" True r

test_leftIdentity :: Assertion
test_leftIdentity = runEff . evalStateLocal x $ do
  mx <|> my
  r <- get @Int
  U.assertEqual "expected result" 1 r
  where
    x :: Int
    x = 0

    mx, my :: State Int :> es => Eff es ()
    mx = put (1 :: Int)
    my = empty

test_leftInterpose :: Assertion
test_leftInterpose = do
  (es0, r0) <- runEff . evalStateLocal x $ do
    es <- mx
    r <- get @Int
    pure (es, r)
  (es, r) <- runEff . evalStateLocal x $ do
    es <- mx <|> my
    r <- get @Int
    pure (es, r)
  assertEqual "expected environment offset" (envOffset es0) (envOffset es)
  assertEqual "expected environment refs" (envRefs es0) (envRefs es)
  assertEqual "expected result" r0 r
  where
    x :: Int
    x = 0

    mx, my :: State Int :> es => Eff es (Env es)
    mx = plusOne $ put (1 :: Int) >> unsafeEff pure
    my = plusOne $ empty >> unsafeEff pure

test_rightIdentity :: Assertion
test_rightIdentity = runEff . evalStateLocal x $ do
  mx <|> my
  r <- get @Int
  U.assertEqual "expected result" 2 r
  where
    x :: Int
    x = 0

    mx, my :: State Int :> es => Eff es ()
    mx = empty
    my = put (2 :: Int)

test_rightInterpose :: Assertion
test_rightInterpose = do
  (es0, r0) <- runEff . evalStateLocal x $ do
    es <- my
    r <- get @Int
    pure (es, r)
  (es, r) <- runEff . evalStateLocal x $ do
    es <- mx <|> my
    r <- get @Int
    pure (es, r)
  assertEqual "expected environment offset" (envOffset es0) (envOffset es)
  assertEqual "expected environment refs" (envRefs es0) (envRefs es)
  assertEqual "expected result" r0 r
  where
    x :: Int
    x = 0

    mx, my :: State Int :> es => Eff es (Env es)
    mx = plusOne $ empty >> unsafeEff pure
    my = plusOne $ put (2 :: Int) >> unsafeEff pure

----------------------------------------
-- Helper

plusOne :: State Int :> es => Eff es a -> Eff es a
plusOne = interpose @(State Int) $ \localEnv -> \case
  Get -> send Get
  Put i -> send (Put (i + 1))
  State f -> send (State (second (+1) . f))
  StateM f -> localSeqUnlift localEnv $ \runLocal ->
    send (StateM (fmap (second (+1)) . runLocal . f))
