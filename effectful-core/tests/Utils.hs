module Utils
  ( assertBool
  , assertEqual
  , assertFailure
  , Ex(..)
  ) where

import Control.Exception
import Control.Monad.IO.Class
import GHC.Stack
import qualified Test.Tasty.HUnit as T

import Effectful.Monad

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

data Ex = Ex deriving (Eq, Show)
instance Exception Ex
