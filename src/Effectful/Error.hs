-- | Support for checked exceptions.
module Effectful.Error
 ( Error
 , runError
 , throwError
 , catchError
 , tryError
 ) where

import Control.Exception
import Data.Typeable
import GHC.Stack

import Effectful.Internal.Env
import Effectful.Internal.Has
import Effectful.Internal.Monad
import Effectful.Internal.Utils

type role Error nominal
data Error e = Error

runError
  :: forall e es a. Exception e
  => Eff (Error e : es) a
  -> Eff es (Either ([String], e) a)
runError (Eff m) = impureEff $ \es0 -> mask $ \release -> do
  size0 <- sizeEnv es0
  es <- unsafeConsEnv (Error @e) es0
  try (release $ m es) >>= \case
    Right a           -> Right a      <$ unsafeTailEnv size0 es
    Left (WrapE cs e) -> Left (cs, e) <$ unsafeTailEnv size0 es

throwError
  :: (HasCallStack, Exception e, Error e :> es)
  => e
  -> Eff es a
throwError e = impureEff_ $ do
  throwIO $ WrapE (ppCallStack <$> getCallStack callStack) e

catchError
  :: (Exception e, Error e :> es)
  => Eff es a
  -> ([String] -> e -> Eff es a)
  -> Eff es a
catchError (Eff m) handler = impureEff $ \es -> do
  size <- sizeEnv es
  m es `catch` \(WrapE e cs) -> do
    checkSizeEnv size es
    unEff (handler e cs) es

tryError
  :: (Exception e, Error e :> es)
  => Eff es a
  -> Eff es (Either ([String], e) a)
tryError m = (Right <$> m) `catchError` \es e -> pure (Left (es, e))

----------------------------------------
-- WrapE

data WrapE e = WrapE [String] e
  deriving Show
instance (Show e, Typeable e) => Exception (WrapE e)
