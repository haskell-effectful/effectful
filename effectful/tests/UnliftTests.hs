module UnliftTests (unliftTests) where

import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO.Async qualified as A

import Effectful
import Effectful.Concurrent.Async qualified as E
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Utils qualified as U

unliftTests :: TestTree
unliftTests = testGroup "Unlift"
  [ testCase "Strategy stays the same in a new thread" test_threadStrategy
  , testCase "SeqUnlift in new thread" test_seqUnliftInNewThread
  , testGroup "Ephemeral strategy"
    [ testCase "Invalid limit" test_ephemeralInvalid
    , testCase "Uses in same thread" test_ephemeralSameThread
    , testCase "Uses in multiple threads" test_ephemeralMultipleThreads
    ]
  , testGroup "Persistent strategy"
    [ testCase "Invalid limit" test_persistentInvalid
    , testCase "Uses in same thread" test_persistentSameThread
    , testCase "Uses in multiple threads" test_persistentMultipleThreads
    ]
  , testCase "Unlifting functions work correctly" test_unliftingFunctions
  ]

test_threadStrategy :: Assertion
test_threadStrategy = runEff $ do
  let strategy = ConcUnlift Ephemeral Unlimited
  s <- withUnliftStrategy strategy $ do
    withRunInIO $ \runInIO -> do
      inThread $ runInIO unliftStrategy
  U.assertEqual "correct strategy" strategy s

test_seqUnliftInNewThread :: Assertion
test_seqUnliftInNewThread = runEff $ do
  U.assertThrowsErrorCall "InvalidUseOfSeqUnlift error" $ do
    withEffToIO SeqUnlift $ \runInIO -> do
      inThread $ runInIO $ return ()

test_ephemeralInvalid :: Assertion
test_ephemeralInvalid = runEff $ do
  U.assertThrowsErrorCall "InvalidNumberOfUses error" $ do
    withEffToIO (ConcUnlift Ephemeral $ Limited 0) $ \_ -> return ()

test_ephemeralSameThread :: Assertion
test_ephemeralSameThread = runEff $ do
  U.assertThrowsErrorCall "ExceededNumberOfUses error" $ do
    withEffToIO (ConcUnlift Ephemeral $ Limited 1) $ \runInIO -> inThread $ do
      runInIO $ return ()
      runInIO $ return ()

test_ephemeralMultipleThreads :: Assertion
test_ephemeralMultipleThreads = runEff $ do
  U.assertThrowsErrorCall "ExceededNumberOfUses error" $ do
    withEffToIO (ConcUnlift Ephemeral $ Limited 1) $ \runInIO -> do
      inThread $ runInIO $ return ()
      inThread $ runInIO $ return ()

test_persistentInvalid :: Assertion
test_persistentInvalid = runEff $ do
  U.assertThrowsErrorCall "InvalidNumberOfThreads error" $ do
    withEffToIO (ConcUnlift Persistent $ Limited 0) $ \_ -> return ()

test_persistentSameThread :: Assertion
test_persistentSameThread = runEff $ do
  withEffToIO (ConcUnlift Persistent $ Limited 1) $ \runInIO -> inThread $ do
    runInIO $ return ()
    runInIO $ return ()

test_persistentMultipleThreads :: Assertion
test_persistentMultipleThreads = runEff $ do
  U.assertThrowsErrorCall "ExceededNumberOfThreads error" $ do
    withEffToIO (ConcUnlift Persistent $ Limited 1) $ \runInIO -> do
      inThread $ runInIO $ return ()
      inThread $ runInIO $ return ()

test_unliftingFunctions :: Assertion
test_unliftingFunctions = runEff . E.runConcurrent $ do
  testFork "runFork1" runFork1
  testFork "runFork2" runFork2
  testFork "runFork3" runFork3
  testFork "runFork4" runFork4
  testFork "runFork5" runFork5
  where
    testFork description runFork = do
      a <- runFork . send $ ForkWithUnmask $ \unmask -> do
        evalState @Int 0 $ raiseWith SeqUnlift $ \unlift -> do
          unlift $ modify @Int (+1)
          unmask . unlift $ modify @Int (+2)
          unlift $ modify @Int (+4)
          unmask . unlift $ modify @Int (+8)
          unlift $ U.assertEqual (description ++ ": correct state") 15 =<< get @Int
      E.waitCatch a >>= \case
        Right () -> pure ()
        Left err -> U.assertFailure $ description ++ ": " ++ show err

data Fork :: Effect where
  ForkWithUnmask :: ((forall a. m a -> m a) -> m r) -> Fork m (A.Async r)
type instance DispatchOf Fork = Dynamic

-- | Uses 'localUnliftIO' and 'withLiftMapIO'.
runFork1 :: IOE :> es => Eff (Fork : es) a -> Eff es a
runFork1 = interpret $ \env -> \case
  ForkWithUnmask m -> do
    withLiftMapIO env $ \liftMap -> do
      localUnliftIO env strategy $ \unlift -> do
        A.asyncWithUnmask $ \unmask -> unlift $ m $ liftMap unmask
  where
    strategy = ConcUnlift Ephemeral $ Limited 1

-- | Uses 'localUnlift' and 'withLiftMap'.
runFork2 :: (IOE :> es, E.Concurrent :> es) => Eff (Fork : es) a -> Eff es a
runFork2 = interpret $ \env -> \case
  ForkWithUnmask m -> do
    withLiftMap env $ \liftMap -> do
      localUnlift env strategy $ \unlift -> do
        E.asyncWithUnmask $ \unmask -> unlift $ m $ liftMap unmask
  where
    strategy = ConcUnlift Ephemeral $ Limited 1

-- | Uses 'localLiftUnliftIO'.
runFork3 :: IOE :> es => Eff (Fork : es) a -> Eff es a
runFork3 = interpret $ \env -> \case
  ForkWithUnmask m -> do
    localLiftUnliftIO env strategy $ \lift unlift -> do
      A.asyncWithUnmask $ \unmask -> unlift $ m $ lift . unmask . unlift
  where
    strategy = ConcUnlift Persistent $ Limited 1

-- | Uses 'localLiftUnlift'.
runFork4 :: (IOE :> es, E.Concurrent :> es) => Eff (Fork : es) a -> Eff es a
runFork4 = interpret $ \env -> \case
  ForkWithUnmask m -> do
    localLiftUnlift env strategy $ \lift unlift -> do
      E.asyncWithUnmask $ \unmask -> unlift $ m $ lift . unmask . unlift
  where
    strategy = ConcUnlift Persistent $ Limited 1

-- | Uses 'localLift' and 'localUnlift'.
runFork5 :: (IOE :> es, E.Concurrent :> es) => Eff (Fork : es) a -> Eff es a
runFork5 = interpret $ \env -> \case
  ForkWithUnmask m -> do
    localLift env strategy $ \lift -> do
      localUnlift env strategy $ \unlift -> do
        E.asyncWithUnmask $ \unmask -> unlift $ m $ lift . unmask . unlift
  where
    strategy = ConcUnlift Persistent $ Limited 1

----------------------------------------
-- Helpers

inThread :: IO a -> IO a
inThread k = A.async k >>= A.wait
