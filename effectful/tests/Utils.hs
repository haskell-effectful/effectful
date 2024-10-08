module Utils
  ( assertBool
  , assertEqual
  , assertFailure
  , assertThrows
  , assertThrowsErrorCall
  , Ex(..)
  ) where

import GHC.Stack
import Test.Tasty.HUnit qualified as T

import Effectful
import Effectful.Exception

assertBool :: (HasCallStack, IOE :> es) => String -> Bool -> Eff es ()
assertBool msg p = liftIO $ T.assertBool msg p

assertEqual
  :: (HasCallStack, Eq a, Show a, IOE :> es)
  => String
  -> a
  -> a
  -> Eff es ()
assertEqual msg expected given = liftIO $ T.assertEqual msg expected given

assertFailure :: (HasCallStack, IOE :> es) => String -> Eff es a
assertFailure msg = liftIO $ T.assertFailure msg

assertThrows
  :: (Exception e, IOE :> es)
  => String
  -> (e -> Bool)
  -> Eff es a
  -> Eff es ()
assertThrows msg p k = catchJust
  (\e -> if p e then Just () else Nothing)
  (k >> liftIO (T.assertFailure msg))
  pure

assertThrowsErrorCall
  :: IOE :> es
  => String -> Eff es a -> Eff es ()
assertThrowsErrorCall err = assertThrows err (\ErrorCall{} -> True)

data Ex = Ex
  deriving stock (Eq, Show)
instance Exception Ex
