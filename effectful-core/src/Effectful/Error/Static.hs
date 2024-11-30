-- | Support for handling errors of a particular type, i.e. checked exceptions.
--
-- The 'Error' effect is __not__ a general mechanism for handling regular
-- exceptions, that's what functions from the "Effectful.Exception" module are
-- for.
--
-- In particular, regular exceptions of type @e@ are distinct from errors of
-- type @e@ and will __not__ be caught by functions from this module:
--
-- >>> import Effectful.Exception qualified as E
--
-- >>> boom = error "BOOM!"
--
-- >>> runEff . runError @ErrorCall $ boom `catchError` \_ (_::ErrorCall) -> pure "caught"
-- *** Exception: BOOM!
-- ...
--
-- If you want to catch regular exceptions, you should use
-- 'Effectful.Exception.catch' (or a similar function):
--
-- >>> runEff $ boom `E.catch` \(_::ErrorCall) -> pure "caught"
-- "caught"
--
-- On the other hand, functions for safe finalization and management of
-- resources such as 'Effectful.Exception.finally' and
-- 'Effectful.Exception.bracket' work as expected:
--
-- >>> msg = liftIO . putStrLn
--
-- >>> :{
-- runEff . runErrorNoCallStack @String $ do
--   E.bracket_ (msg "Beginning.")
--              (msg "Cleaning up.")
--              (msg "Computing." >> throwError "oops" >> msg "More.")
-- :}
-- Beginning.
-- Computing.
-- Cleaning up.
-- Left "oops"
--
-- /Note:/ unlike the 'Control.Monad.Trans.Except.ExceptT' monad transformer
-- from the @transformers@ library, the order in which you handle the 'Error'
-- effect with regard to other stateful effects does not matter. Consider the
-- following:
--
-- >>> import Control.Monad.State.Strict qualified as T
-- >>> import Control.Monad.Except qualified as T
--
-- >>> m1 = (T.modify (++ " there!") >> T.throwError "oops") `T.catchError` \_ -> pure ()
--
-- >>> (`T.runStateT` "Hi") . T.runExceptT $ m1
-- (Right (),"Hi there!")
--
-- >>> T.runExceptT . (`T.runStateT` "Hi") $ m1
-- Right ((),"Hi")
--
-- Here, whether state updates within the 'catchError' block are discarded or
-- not depends on the shape of the monad transformer stack, which is surprising
-- and can be a source of subtle bugs. On the other hand:
--
-- >>> import Effectful.State.Static.Local
--
-- >>> m2 = (modify (++ " there!") >> throwError "oops") `catchError` \_ (_::String) -> pure ()
--
-- >>> runEff . runState "Hi" . runError @String $ m2
-- (Right (),"Hi there!")
--
-- >>> runEff . runError @String . runState "Hi" $ m2
-- Right ((),"Hi there!")
--
-- Here, no matter the order of effects, state updates made within the
-- @catchError@ block before the error happens always persist, giving
-- predictable behavior.
--
-- /Hint:/ if you'd like to reproduce the transactional behavior with the
-- t'Effectful.State.Static.Local.State' effect, appropriate usage of
-- 'Effectful.Exception.bracketOnError' will do the trick.
module Effectful.Error.Static
  ( -- * Effect
    Error

    -- ** Handlers
  , runError
  , runErrorWith
  , runErrorNoCallStack
  , runErrorNoCallStackWith

    -- ** Operations
  , throwErrorWith
  , throwError
  , throwError_
  , catchError
  , handleError
  , tryError

  -- * Re-exports
  , HasCallStack
  , CallStack
  , getCallStack
  , prettyCallStack
  ) where

import Data.Kind
import GHC.Stack

import Effectful
import Effectful.Dispatch.Static
import Effectful.Exception
import Effectful.Internal.Utils

-- | Provide the ability to handle errors of type @e@.
data Error (e :: Type) :: Effect

type instance DispatchOf (Error e) = Static NoSideEffects
newtype instance StaticRep (Error e) = Error ErrorId

-- | Handle errors of type @e@.
runError
  :: forall e es a
   . HasCallStack
  => Eff (Error e : es) a
  -> Eff es (Either (CallStack, e) a)
runError action = do
  eid <- unsafeEff_ newErrorId
  evalStaticRep (Error @e eid) $ do
    tryJust (matchError eid) action

-- | Handle errors of type @e@ with a specific error handler.
--
-- @since 2.3.0.0
runErrorWith
  :: HasCallStack
  => (CallStack -> e -> Eff es a)
  -- ^ The error handler.
  -> Eff (Error e : es) a
  -> Eff es a
runErrorWith handler action = runError action >>= \case
  Left (cs, e) -> handler cs e
  Right a -> pure a

-- | Handle errors of type @e@. In case of an error discard the 'CallStack'.
--
-- @since 2.3.0.0
runErrorNoCallStack
  :: forall e es a
   . HasCallStack
  => Eff (Error e : es) a
  -> Eff es (Either e a)
runErrorNoCallStack = fmap (either (Left . snd) Right) . runError

-- | Handle errors of type @e@ with a specific error handler. In case of an
-- error discard the 'CallStack'.
runErrorNoCallStackWith
  :: HasCallStack
  => (e -> Eff es a)
  -- ^ The error handler.
  -> Eff (Error e : es) a
  -> Eff es a
runErrorNoCallStackWith handler action = runErrorNoCallStack action >>= \case
  Left e -> handler e
  Right a -> pure a

-- | Throw an error of type @e@ and specify a display function in case a
-- third-party code catches the internal exception and 'show's it.
--
-- @since 2.4.0.0
throwErrorWith
  :: forall e es a. (HasCallStack, Error e :> es)
  => (e -> String)
  -- ^ The display function.
  -> e
  -- ^ The error.
  -> Eff es a
throwErrorWith display e = do
  Error eid <- getStaticRep @(Error e)
  withFrozenCallStack throwIO $ ErrorWrapper eid callStack (display e) (toAny e)

-- | Throw an error of type @e@ with 'show' as a display function.
throwError
  :: forall e es a. (HasCallStack, Error e :> es, Show e)
  => e
  -- ^ The error.
  -> Eff es a
throwError = withFrozenCallStack throwErrorWith show

-- | Throw an error of type @e@ with no display function.
--
-- @since 2.4.0.0
throwError_
  :: forall e es a. (HasCallStack, Error e :> es)
  => e
  -- ^ The error.
  -> Eff es a
throwError_ = withFrozenCallStack throwErrorWith (const "<opaque>")

-- | Handle an error of type @e@.
catchError
  :: forall e es a. (HasCallStack, Error e :> es)
  => Eff es a
  -- ^ The inner computation.
  -> (CallStack -> e -> Eff es a)
  -- ^ A handler for errors in the inner computation.
  -> Eff es a
catchError action handler = do
  Error eid <- getStaticRep @(Error e)
  catchJust (matchError eid) action $ \(cs, e) -> handler cs e

-- | The same as @'flip' 'catchError'@, which is useful in situations where the
-- code for the handler is shorter.
handleError
  :: forall e es a. (HasCallStack, Error e :> es)
  => (CallStack -> e -> Eff es a)
  -- ^ A handler for errors in the inner computation.
  -> Eff es a
  -- ^ The inner computation.
  -> Eff es a
handleError = flip catchError

-- | Similar to 'catchError', but returns an 'Either' result which is a 'Right'
-- if no error was thrown and a 'Left' otherwise.
tryError
  :: forall e es a. (HasCallStack, Error e :> es)
  => Eff es a
  -- ^ The inner computation.
  -> Eff es (Either (CallStack, e) a)
tryError action = do
  Error eid <- getStaticRep @(Error e)
  tryJust (matchError eid) action

----------------------------------------
-- Helpers

newtype ErrorId = ErrorId Unique
  deriving newtype Eq

-- | A unique is picked so that distinct 'Error' handlers for the same type
-- don't catch each other's exceptions.
newErrorId :: IO ErrorId
newErrorId = ErrorId <$> newUnique

data ErrorWrapper = ErrorWrapper !ErrorId CallStack String Any

instance Show ErrorWrapper where
  showsPrec _ (ErrorWrapper _ cs errRep _)
    = ("Effectful.Error.Static.ErrorWrapper: " ++)
    . (errRep ++)
    . ("\n" ++)
    . (prettyCallStack cs ++)

instance Exception ErrorWrapper where
  -- See discussion in https://github.com/haskell-effectful/effectful/pull/232.
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

matchError :: ErrorId -> ErrorWrapper -> Maybe (CallStack, e)
matchError eid (ErrorWrapper etag cs _ e)
  | eid == etag = Just (cs, fromAny e)
  | otherwise = Nothing
