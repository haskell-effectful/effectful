{- | Support for checked exceptions.

The 'Error' effect provided by this module is a suitable replacement for the
'Control.Monad.Except.ExceptT' monad transformer found in the @mtl@ library. It
is __not__ intended to be a general mechanism for catching errors, that's what
functions from the "Control.Monad.Catch" module are for.

For example, if you want to catch an unchecked exception like
'Control.Exception.ErrorCall' you could use 'Control.Monad.Catch.catch':

>>> import qualified Control.Monad.Catch as E
>>> :{
  let boom :: Eff es String
      boom = error "BOOM!"
  in
  runEff $ boom `E.catch` \(e :: ErrorCall) -> pure "caught some error"
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
 , HasCallStack
 , CallStack
 , getCallStack
 , prettyCallStack
 ) where

import Control.Exception
import Data.Typeable
import Data.Unique
import GHC.Stack

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad

newtype Error e :: Effect where
  Error :: ErrorId -> Error e m r

runError
  :: forall e es a. Typeable e
  => Eff (Error e : es) a
  -> Eff es (Either (CallStack, e) a)
runError m = unsafeEff $ \es0 -> mask $ \release -> do
  eid <- newErrorId
  size0 <- sizeEnv es0
  es <- unsafeConsEnv (IdE (Error @e eid)) noRelinker es0
  r <- tryErrorIO release eid es `onException` unsafeTailEnv size0 es
  unsafeTailEnv size0 es
  pure r
  where
    tryErrorIO release eid es = try (release $ unEff m es) >>= \case
      Right a -> pure $ Right a
      Left ex -> tryHandler ex eid (\cs e -> Left (cs, e))
               $ throwIO ex

throwError
  :: forall e es a. (HasCallStack, Typeable e, Error e :> es)
  => e
  -> Eff es a
throwError e = unsafeEff $ \es -> do
  IdE (Error eid) <- getEnv @(Error e) es
  throwIO $ ErrorEx eid callStack e

catchError
  :: forall e es a. (Typeable e, Error e :> es)
  => Eff es a
  -> (CallStack -> e -> Eff es a)
  -> Eff es a
catchError m handler = unsafeEff $ \es -> do
  IdE (Error eid) <- getEnv @(Error e) es
  size <- sizeEnv es
  catchErrorIO eid (unEff m es) $ \cs e -> do
    checkSizeEnv size es
    unEff (handler cs e) es

tryError
  :: forall e es a. (Typeable e, Error e :> es)
  => Eff es a
  -> Eff es (Either (CallStack, e) a)
tryError m = (Right <$> m) `catchError` \es e -> pure $ Left (es, e)

----------------------------------------
-- Helpers

newtype ErrorId = ErrorId Unique
  deriving Eq

-- | A unique is picked so that distinct 'Error' handlers for the same type
-- don't catch each other's exceptions.
newErrorId :: IO ErrorId
newErrorId = ErrorId <$> newUnique

tryHandler
  :: Typeable e
  => SomeException
  -> ErrorId
  -> (CallStack -> e -> r)
  -> IO r
  -> IO r
tryHandler ex eid0 handler next = case fromException ex of
  Just (ErrorEx eid cs e)
    | eid0 == eid -> pure $ handler cs e
    | otherwise   -> next
  Nothing -> next

data ErrorEx e = ErrorEx !ErrorId CallStack e
instance Typeable e => Show (ErrorEx e) where
  showsPrec p (ErrorEx _ cs e)
    = ("Effectful.Error.ErrorEx (" ++)
    . showsPrec p (typeOf e)
    . (") " ++)
    . showsPrec p cs
instance Typeable e => Exception (ErrorEx e)

catchErrorIO :: Typeable e => ErrorId -> IO a -> (CallStack -> e -> IO a) -> IO a
catchErrorIO eid m handler = do
  m `catch` \err@(ErrorEx etag e cs) -> do
    if eid == etag
      then handler e cs
      else throwIO err
