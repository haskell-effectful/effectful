{-# LANGUAGE ImplicitParams #-}
-- | Turn an effect handler into an effectful operation.
--
-- @since 2.3.0.0
module Effectful.Provider
  ( -- * Example
    -- $example

    -- * Effect
    Provider
  , Provider_

    -- ** Handlers
  , runProvider
  , runProvider_

    -- ** Operations
  , provide
  , provide_
  , provideWith
  , provideWith_
  ) where

import Control.Monad
import Data.Coerce
import Data.Functor.Identity
import Data.Kind (Type)
import Data.Primitive.PrimArray
import GHC.Stack

import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import Effectful.Internal.Env (Env(..))
import Effectful.Internal.Utils

-- $example
--
-- >>> import Control.Monad.IO.Class
-- >>> import Data.Map.Strict qualified as M
-- >>> import Effectful.Dispatch.Dynamic
-- >>> import Effectful.State.Static.Local
--
-- Given an effect:
--
-- >>> :{
--   data Write :: Effect where
--     Write :: String -> Write m ()
--   type instance DispatchOf Write = Dynamic
-- :}
--
-- >>> :{
--   write :: Write :> es => String -> Eff es ()
--   write = send . Write
-- :}
--
-- its handler can be turned into an effectful operation with the 'Provider'
-- effect:
--
-- >>> :{
--   action :: Provider_ Write FilePath :> es => Eff es ()
--   action = do
--     provideWith_ @Write "in.txt" $ do
--       write "hi"
--       write "there"
--     provideWith_ @Write "out.txt" $ do
--       write "good"
--       write "bye"
-- :}
--
-- Then, given multiple interpreters:
--
-- >>> :{
--   runWriteIO
--     :: IOE :> es
--     => FilePath
--     -> Eff (Write : es) a
--     -> Eff es a
--   runWriteIO fp = interpret_ $ \case
--     Write msg -> liftIO . putStrLn $ fp ++ ": " ++ msg
-- :}
--
-- >>> :{
--   runWritePure
--     :: State (M.Map FilePath [String]) :> es
--     => FilePath
--     -> Eff (Write : es) a
--     -> Eff es a
--   runWritePure fp = interpret_ $ \case
--     Write msg -> modify $ M.insertWith (++) fp [msg]
-- :}
--
-- @action@ can be supplied with either of them for the appropriate behavior:
--
-- >>> :{
--   runEff
--     . runProvider_ runWriteIO
--     $ action
-- :}
-- in.txt: hi
-- in.txt: there
-- out.txt: good
-- out.txt: bye
--
-- >>> :{
--   runPureEff
--     . fmap (fmap reverse)
--     . execState @(M.Map FilePath [String]) M.empty
--     . runProvider_ runWritePure
--     $ action
-- :}
-- fromList [("in.txt",["hi","there"]),("out.txt",["good","bye"])]

-- | Provide a way to run a handler of @e@ with a given @input@.
--
-- /Note:/ @f@ can be used to alter the return type of the effect handler. If
-- that's unnecessary, use 'Provider_'.
data Provider (e :: Effect) (input :: Type) (f :: Type -> Type) :: Effect

-- | A restricted variant of 'Provider' with unchanged return type of the effect
-- handler.
type Provider_ e input = Provider e input Identity

type instance DispatchOf (Provider e input f) = Static NoSideEffects

data instance StaticRep (Provider e input f) where
  Provider
    :: !(Env providerEs)
    -> !(forall r. HasCallStack => input -> Eff (e : providerEs) r -> Eff providerEs (f r))
    -> StaticRep (Provider e input f)

-- | Run the 'Provider' effect with a given effect handler.
runProvider
  :: HasCallStack
  => (forall r. HasCallStack => input -> Eff (e : es) r -> Eff es (f r))
  -- ^ The effect handler.
  -> Eff (Provider e input f : es) a
  -> Eff es a
runProvider provider m = unsafeEff $ \es0 -> do
  inlineBracket
    (consEnv (mkProvider es0) relinkProvider es0)
    unconsEnv
    (\es -> unEff m es)
  where
    -- Corresponds to withFrozenCallStack in provideWith.
    mkProvider es = Provider es (let ?callStack = thawCallStack ?callStack in provider)

-- | Run the 'Provider' effect with a given effect handler that doesn't change
-- its return type.
runProvider_
  :: HasCallStack
  => (forall r. HasCallStack => input -> Eff (e : es) r -> Eff es r)
  -- ^ The effect handler.
  -> Eff (Provider_ e input : es) a
  -> Eff es a
runProvider_ provider = runProvider $ \input -> coerce . provider input

-- | Run the effect handler.
provide :: (HasCallStack, Provider e () f :> es) => Eff (e : es) a -> Eff es (f a)
provide = provideWith ()

-- | Run the effect handler with unchanged return type.
provide_ :: (HasCallStack, Provider_ e () :> es) => Eff (e : es) a -> Eff es a
provide_ = provideWith_ ()

-- | Run the effect handler with a given input.
provideWith
  :: (HasCallStack, Provider e input f :> es)
  => input
  -- ^ The input to the effect handler.
  -> Eff (e : es) a
  -> Eff es (f a)
provideWith input action = unsafeEff $ \es -> do
  Provider providerEs handler <- getEnv es
  (`unEff` providerEs)
    -- Corresponds to thawCallStack in runProvider.
    . withFrozenCallStack handler input
    . unsafeEff $ \eProviderEs -> do
    unEff action =<< copyRef eProviderEs es

-- | Run the effect handler that doesn't change its return type with a given
-- input.
provideWith_
  :: (HasCallStack, Provider_ e input :> es)
  => input
  -- ^ The input to the effect handler.
  -> Eff (e : es) a
  -> Eff es a
provideWith_ input = adapt . provideWith input
  where
    adapt :: Eff es (Identity a) -> Eff es a
    adapt = coerce

----------------------------------------
-- Helpers

relinkProvider :: Relinker StaticRep (Provider e input f)
relinkProvider = Relinker $ \relink (Provider providerEs run) -> do
  newHandlerEs <- relink providerEs
  pure $ Provider newHandlerEs run

copyRef :: HasCallStack => Env (e : providerEs) -> Env es -> IO (Env (e : es))
copyRef (Env hoffset hrefs hstorage) (Env offset refs0 storage) = do
  when (hstorage /= storage) $ do
    error "storages do not match"
  let size = sizeofPrimArray refs0 - offset
  mrefs <- newPrimArray (size + 2)
  copyPrimArray mrefs 0 hrefs hoffset 2
  copyPrimArray mrefs 2 refs0 offset size
  refs <- unsafeFreezePrimArray mrefs
  pure $ Env 0 refs storage
