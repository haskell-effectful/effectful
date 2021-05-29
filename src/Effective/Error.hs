-- | Support for checked exceptions.
module Effective.Error
 ( Error
 , runError
 , throwError
 , catchError
 , tryError
 ) where

import Control.Exception
import Data.Typeable
import GHC.Stack

import Effective.Internal.Env
import Effective.Internal.Has
import Effective.Internal.Monad
import Effective.Internal.Utils

type role Error nominal
data Error e = Error

runError
  :: forall e es a. Exception e
  => Eff (Error e : es) a
  -> Eff es (Either ([String], e) a)
runError (Eff m) = impureEff $ \es0 -> mask $ \release -> do
  size <- sizeEnv es0
  es <- unsafeConsEnv (Error @e) es0
  try (release $ m es) >>= \case
    Right a           -> Right a      <$ unsafeTrimEnv size es
    Left (WrapE cs e) -> Left (cs, e) <$ unsafeTrimEnv size es

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
