module Word64MapTests (word64MapTests) where

import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Word
import System.Random
import Test.Tasty
import Test.Tasty.HUnit

import Effectful.Internal.Utils.Word64Map qualified as WM

word64MapTests :: TestTree
word64MapTests = testGroup "Word64Map"
  [ testCase "Matches Data.Map over a mix of operations" test_differential
  , testCase "Distinguishes keys colliding in the low 32 bits" test_truncation
  , testCase "Handles boundary keys" test_boundary
  ]

-- | Run a large mix of inserts and deletes through both 'WM.Word64Map' and
-- 'M.Map' and check that they agree on the value of every key ever touched.
test_differential :: Assertion
test_differential = do
  let poolSize = 5000 :: Word64
      flags    = randoms (mkStdGen 1) :: [Bool]
      keys     = randomRs (0, poolSize - 1) (mkStdGen 2)
      ops :: [(Bool, Word64, Int)]
      ops = take 200000 (zip3 flags keys [0 ..])
      step (wm, m) (ins, k, v)
        | ins       = (WM.insert k v wm, M.insert k v m)
        | otherwise = (WM.delete k   wm, M.delete k   m)
      (wmFinal, mFinal) = L.foldl' step (WM.empty, M.empty) ops
  case [ k | (_, k, _) <- ops, WM.lookup k wmFinal /= M.lookup k mFinal ] of
    []      -> pure ()
    (k : _) -> assertFailure $
      "Word64Map and Data.Map disagree on key " ++ show k ++ ": "
      ++ show (WM.lookup k wmFinal) ++ " vs " ++ show (M.lookup k mFinal)

-- | Two keys that are equal in their low 32 bits but differ in the high bits
-- must not collide (the reason for not using @IntMap@, which truncates keys to
-- 'Int' on 32-bit platforms).
test_truncation :: Assertion
test_truncation = do
  let a = 0x0000000100000001 :: Word64
      b = 0x0000000200000001 :: Word64
      m = WM.insert b 'b' (WM.insert a 'a' WM.empty)
  WM.lookup a m @?= Just 'a'
  WM.lookup b m @?= Just 'b'

-- | Boundary keys, in particular the ones around the sign bit, are handled the
-- same as any other (the keys are compared as unsigned).
test_boundary :: Assertion
test_boundary = do
  let ks = [ 0, 1, maxBound, maxBound - 1
           , 0x8000000000000000, 0x7fffffffffffffff
           ] :: [Word64]
      m  = L.foldl' (\acc k -> WM.insert k k acc) WM.empty ks
  -- Everything inserted is present with the correct value.
  mapM_ (\k -> WM.lookup k m @?= Just k) ks
  -- A key that was never inserted is absent.
  WM.lookup 12345 m @?= Nothing
  -- Deleting one key leaves the others intact.
  let removed = 0x8000000000000000
      m'      = WM.delete removed m
  WM.lookup removed m' @?= Nothing
  mapM_ (\k -> WM.lookup k m' @?= Just k) (filter (/= removed) ks)
