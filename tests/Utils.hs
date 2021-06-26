module Utils where

import Control.Exception
import Control.Monad.IO.Class
import Test.Tasty.HUnit

import Effectful.Monad

assertEqual_ :: (Eq a, Show a, IOE :> es) => String -> a -> a -> Eff es ()
assertEqual_ msg expected given = liftIO $ assertEqual msg expected given

data Ex = Ex deriving (Eq, Show)
instance Exception Ex
