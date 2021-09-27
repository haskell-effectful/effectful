module Utils
  ( assertBool
  , assertEqual
  , assertFailure
  , assertThrows
  , Ex(..)
  ) where

import Control.Exception
import Control.Monad
import GHC.Stack
import qualified Test.Tasty.HUnit as T

import Effectful

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
assertThrows msg p k = withEffToIO $ \runInIO -> do
  let k' = do
        void $ runInIO k
        T.assertFailure msg

  k' `catch` \e -> if p e
    then return ()
    else throwIO e

data Ex = Ex deriving (Eq, Show)
instance Exception Ex
