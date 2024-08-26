module ReaderTests (readerTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic
import Utils qualified as U

readerTests :: TestTree
readerTests = testGroup "Reader"
  [ testCase "local works in handlers (dynamic/static)" $ test_localInHandler runReader
  , testCase "local works in handlers (dynamic/pure)" $ test_localInHandler runPureReader
  ]

data SomeEff :: Effect where
  SomeAction :: SomeEff m ()

type instance DispatchOf SomeEff = Dynamic

test_localInHandler
  :: (forall r es a. r -> Eff (Reader r : es) a -> Eff es a)
  -> Assertion
test_localInHandler runR = runEff . runR "global" . interpret f $ do
  local (const "local") $ send SomeAction
  where
    f :: (IOE :> es, Reader String :> es) => EffectHandler SomeEff es
    f _ SomeAction = U.assertEqual "expected result" "local" =<< ask

-- | Purely dynamic Reader for testing purposes.
runPureReader :: r -> Eff (Reader r : es) a -> Eff es a
runPureReader r0 = interpret (handler r0)
  where
    handler :: r -> EffectHandler (Reader r) handlerEs
    handler r env = \case
      Ask       -> pure r
      Local f m -> localSeqUnlift env $ \unlift -> do
        unlift $ interpose (handler $ f r) m
