module STTests (stTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Effectful
import Effectful.ST
import Effectful.ST.STRef
import Utils qualified as U

import Data.Foldable (for_)
import Data.Proxy (Proxy (..))

stTests :: TestTree
stTests = testGroup "ST"
    [ testCase "sumSTRef" test_sumSTRef
    ]

test_sumSTRef :: Assertion
test_sumSTRef = runEff $ runSTE $ \(Proxy :: Proxy s) -> do
    sumRef <- newSTRef @s @Int 0
    for_ [0 .. 20] $ \i -> do
        modifySTRef' sumRef (+ i)
    result <- readSTRef sumRef
    U.assertEqual "correct sum" result (sum [0 .. 20])
