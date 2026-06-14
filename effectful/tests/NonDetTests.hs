module NonDetTests (nonDetTests) where

import Data.IORef.Strict
import Data.Primitive.PrimArray
import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import Effectful.Internal.Env qualified as I
import Effectful.Internal.Utils
import Effectful.NonDet
import Effectful.Reader.Static
import Effectful.State.Dynamic

import Utils qualified as U

nonDetTests :: TestTree
nonDetTests = testGroup "NonDet"
  [ testCaseSteps "empty (left)"  $ test_empty leftEmpty
  , testCaseSteps "empty (right)" $ test_empty rightEmpty
  , testGroup "interaction"
    [ testCaseSteps "local state"  $ test_state evalStateLocal  expectedLocalState
    , testCaseSteps "shared state" $ test_state evalStateShared expectedSharedState
    ]
  , testCaseSteps "local mutable state" $ test_mutState expectedLocalState
  , testCaseSteps "different handlers are independent" test_independentHandlers
  , testCase "stale reference is detected after rollback" test_staleReferenceAfterRollback
  ]
  where
    leftEmpty :: NonDet :> es => Eff es Bool
    leftEmpty  = runReader () (empty >> pure False) <|> runReader () (pure True)

    rightEmpty :: NonDet :> es => Eff es Bool
    rightEmpty = runReader () (pure True) <|> runReader () (empty >> pure False)

    expectedLocalState :: OnEmptyPolicy -> Int
    expectedLocalState = \case
      OnEmptyKeep     -> 7
      OnEmptyRollback -> 0

    expectedSharedState :: OnEmptyPolicy -> Int
    expectedSharedState _ = 7

test_empty
  :: Eff [NonDet, IOE] Bool
  -> (String -> IO ())
  -> Assertion
test_empty test step = runEff $ do
  runNonDetBoth test $ \policy result -> do
    liftIO . step $ show policy
    U.assertEqual "result" (Just True) (dropLeft result)

test_state
  :: (forall r. Int -> Eff [State Int, IOE] r -> Eff '[IOE] r)
  -> (OnEmptyPolicy -> Int)
  -> (String -> IO ())
  -> IO ()
test_state evalState expectedState step = runEff $ do
  evalState 0 . runNonDetBoth test $ \policy result -> do
    liftIO . step $ show policy
    U.assertEqual "result" Nothing (dropLeft result)
    s <- state @Int $ \s -> (s, 0)
    U.assertEqual "state" (expectedState policy) s
  where
    test :: (NonDet :> es, State Int :> es) => Eff es ()
    test = do
      modify @Int (+1)
      _<- (modify @Int (+2) >> empty) <|> (modify @Int (+4) >> empty)
      modify @Int (+8)

test_mutState
  :: (OnEmptyPolicy -> Int)
  -> (String -> IO ())
  -> IO ()
test_mutState expectedState step = runEff $ do
  runMutInt 0 . runNonDetBoth test $ \policy result -> do
    liftIO . step $ show policy
    s <- stateMutInt $ \s -> (s, 0)
    U.assertEqual "result" Nothing (dropLeft result)
    U.assertEqual "state" (expectedState policy) s
  where
    test :: (NonDet :> es, MutInt :> es) => Eff es ()
    test = do
      modifyMutInt (+1)
      _<- (modifyMutInt (+2) >> empty) <|> (modifyMutInt (+4) >> empty)
      modifyMutInt (+8)

test_independentHandlers :: (String -> IO ()) -> Assertion
test_independentHandlers step = runEff $ do
  runNonDetBoth test $ \policy result -> liftIO $ do
    step $ show policy
    case result of
      Left cs -> assertBool "stack trace points to the correct action" $
        "outerEmpty" == fst (last $ getCallStack cs)
      Right _ -> assertFailure "empty handled by the wrong (inner) handler"
  where
    test :: NonDet :> es => Eff es (Either CallStack Bool)
    test = runOuterEmpty . runNonDet OnEmptyKeep $ outerEmpty <|> pure True

-- | A reference to an effect captured inside a rolled back branch must be
-- detected as out of date even if the storage was grown within the branch, in
-- which case the rollback must not shrink its capacity.
test_staleReferenceAfterRollback :: Assertion
test_staleReferenceAfterRollback = runEff $ do
  ref <- liftIO $ newIORef' Nothing
  result <- runNonDet OnEmptyRollback $
    (do stale <- captureStaleReference 16
        capacity <- getStorageCapacity
        liftIO $ writeIORef' ref $ Just (capacity, stale)
        emptyEff)
    <|> pure True
  U.assertEqual "result" (Just True) (dropLeft result)
  liftIO (readIORef' ref) >>= \case
    Nothing -> U.assertFailure "stale reference not captured"
    Just (capacity, stale) -> do
      -- If the rollback shrunk the capacity of the storage, evaluation of the
      -- stale reference below would read out of bounds.
      U.assertEqual "capacity" capacity =<< getStorageCapacity
      U.assertThrowsErrorCall "stale reference detected" $ liftIO stale

-- | Run a computation under the given number of effect handlers to grow the
-- storage, then capture an unlifted reference to the innermost one.
captureStaleReference :: IOE :> es => Int -> Eff es (IO Int)
captureStaleReference n = evalStateLocal @Int n $
  if n <= 0
    then withEffToIO SeqUnlift $ \unlift -> pure . unlift $ get @Int
    else captureStaleReference (n - 1)

-- | Get the current capacity of the underlying storage of effects.
getStorageCapacity :: Eff es Int
getStorageCapacity = unsafeEff $ \es -> do
  I.Storage _ storageData <- readIORef' es.storage
  getSizeofMutablePrimArray storageData.versions

----------------------------------------
-- Helpers

data MutInt :: Effect
type instance DispatchOf MutInt = Static NoSideEffects
newtype instance StaticRep MutInt = MutInt (IORef' Int)

runMutInt :: Int -> Eff (MutInt : es) a -> Eff es a
runMutInt s action = unsafeEff $ \es -> do
  ref <- newIORef' s
  inlineBracket
    (consEnv (MutInt ref) relinkMutInt es)
    unconsEnv
    (unEff action)
  where
    relinkMutInt :: Relinker StaticRep MutInt
    relinkMutInt = Relinker $ \_ (MutInt ref0) -> do
      ref <- newIORef' =<< readIORef' ref0
      pure $ MutInt ref

stateMutInt :: MutInt :> es => (Int -> (a, Int)) -> Eff es a
stateMutInt f = unsafeEff $ \es -> do
  MutInt ref <- getEnv es
  (r, s) <- f <$> readIORef' ref
  writeIORef' ref s
  pure r

modifyMutInt :: MutInt :> es => (Int -> Int) -> Eff es ()
modifyMutInt f = stateMutInt $ ((),) . f

----

data OuterEmpty :: Effect where
  OuterEmpty :: OuterEmpty m a

type instance DispatchOf OuterEmpty = Dynamic

outerEmpty :: (HasCallStack, OuterEmpty :> es) => Eff es a
outerEmpty = send OuterEmpty

runOuterEmpty :: NonDet :> es => Eff (OuterEmpty : es) a -> Eff es a
runOuterEmpty = interpret_ $ \case
  OuterEmpty -> emptyEff

----

dropLeft :: Either e a -> Maybe a
dropLeft = either (const Nothing) Just

runNonDetBoth
  :: Eff (NonDet : es) a
  -> (OnEmptyPolicy -> Either CallStack a -> Eff es ())
  -> Eff es ()
runNonDetBoth m k = do
  k OnEmptyKeep     =<< runNonDet OnEmptyKeep m
  k OnEmptyRollback =<< runNonDet OnEmptyRollback m
