{- | Support for checked exceptions.

   The `Error` effect provided by this module is a suitable replacement for the
   `Control.Monad.Except.ExceptT` monad transformer found in the mtl library.
   It is __not__ intended to be a general mechanism for catching errors like
   the functions of the 'exceptions' package. For example, if you want to catch
   an unchecked exception like `Control.Exception.SomeException` or
   `Control.Exception.ErrorCall` you could rely on `Control.Monad.catch`:

   >>> :{
     let boom :: Eff '[IOE] String
         boom = error "BOOM!"
     in
     runIOE $ boom `Control.Monad.Catch.catch` \(e :: ErrorCall) ->
       return "caught some error"
   :}
   "caught some error"
-}
module Effectful.Error
 ( Error
 , runError
 , throwError
 , catchError
 , tryError

 -- * Re-exports
 , Exception
 ) where

import Control.Exception
import Data.Typeable
import Data.Unique
import GHC.Stack

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad
import Effectful.Internal.Utils

-- $setup
-- >>> import Control.Monad.Catch
-- >>> import Control.Monad.IO.Class
-- >>> import Effectful.Interpreter

data Error e :: Effect where
  Error :: Unique -> Error e m r

-- | TODO: write about possibility of an error escaping the scope of 'runError'
-- when misused with 'AsyncE'.
runError
  :: forall e es a. Typeable e
  => Eff (Error e : es) a
  -> Eff es (Either ([String], e) a)
runError (Eff m) = unsafeEff $ \es0 -> mask $ \release -> do
  -- A unique tag is picked so that different runError handlers for the same
  -- type don't catch each other's exceptions.
  tag <- newUnique
  size0 <- sizeEnv es0
  es <- unsafeConsEnv (IdE (Error @e tag)) noRelinker es0
  r <- tryErrorIO tag (release $ m es) `onException` unsafeTailEnv size0 es
  _ <- unsafeTailEnv size0 es
  pure r

throwError
  :: forall e es a. (HasCallStack, Typeable e, Error e :> es)
  => e
  -> Eff es a
throwError e = readerEffectM @(Error e) $ \(IdE (Error tag)) -> unsafeEff_ $ do
  throwIO $ WrapErr tag (ppCallStack <$> getCallStack callStack) e

catchError
  :: forall e es a. (Typeable e, Error e :> es)
  => Eff es a
  -> ([String] -> e -> Eff es a)
  -> Eff es a
catchError (Eff m) handler = do
  readerEffectM @(Error e) $ \(IdE (Error tag)) -> unsafeEff $ \es -> do
    size <- sizeEnv es
    catchErrorIO tag (m es) $ \cs e -> do
      checkSizeEnv size es
      unEff (handler cs e) es

tryError
  :: forall e es a. (Typeable e, Error e :> es)
  => Eff es a
  -> Eff es (Either ([String], e) a)
tryError m = (Right <$> m) `catchError` \es e -> pure $ Left (es, e)

----------------------------------------
-- Helpers

data WrapErr e = WrapErr Unique [String] e

instance Typeable e => Show (WrapErr e) where
  showsPrec p (WrapErr _ cs e)
    = showsPrec p "Effectful.Error.WrapErr ("
    . showsPrec p (typeOf e)
    . showsPrec p ") "
    . showsPrec p cs
instance Typeable e => Exception (WrapErr e)

catchErrorIO :: Typeable e => Unique -> IO a -> ([String] -> e -> IO a) -> IO a
catchErrorIO tag m handler = do
  m `catch` \err@(WrapErr etag e cs) -> do
    if tag == etag
      then handler e cs
      else throwIO err

tryErrorIO :: Typeable e => Unique -> IO a -> IO (Either ([String], e) a)
tryErrorIO tag m = catchErrorIO tag (Right <$> m) $ \es e -> pure $ Left (es, e)
