module ReaderTests (readerTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import qualified Utils as U

readerTests :: TestTree
readerTests = testGroup "Reader"
  [ testCase "local works in handlers" test_localInHandler
  ]

data SomeEff :: Effect where
  SomeAction :: SomeEff m ()

type instance DispatchOf SomeEff = Dynamic

test_localInHandler :: Assertion
test_localInHandler = runEff . runReader "global" . interpret f $ do
  local (const "local") $ send SomeAction
  where
    f :: [IOE, Reader String] :>> es => EffectHandler SomeEff es
    f _ SomeAction = U.assertEqual "expected result" "local" =<< ask
