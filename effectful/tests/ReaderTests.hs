module ReaderTests (readerTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Dynamic qualified as D
import Effectful.Reader.Static qualified as S
import Utils qualified as U

readerTests :: TestTree
readerTests = testGroup "Reader"
  [ testCase "local works in handlers (static)" test_localInHandlerStatic
  , testCase "local works in handlers (dynamic)" test_localInHandlerDynamic
  ]

data SomeEff :: Effect where
  SomeAction :: SomeEff m ()

type instance DispatchOf SomeEff = Dynamic

test_localInHandlerStatic :: Assertion
test_localInHandlerStatic = runEff . S.runReader "global" . interpret f $ do
  S.local (const "local") $ send SomeAction
  where
    f :: (IOE :> es, S.Reader String :> es) => EffectHandler SomeEff es
    f _ SomeAction = U.assertEqual "expected result" "local" =<< S.ask

test_localInHandlerDynamic :: Assertion
test_localInHandlerDynamic = runEff . D.runReader "global" . interpret f $ do
  D.local (const "local") $ send SomeAction
  where
    f :: (IOE :> es, D.Reader String :> es) => EffectHandler SomeEff es
    f _ SomeAction = U.assertEqual "expected result" "local" =<< D.ask
