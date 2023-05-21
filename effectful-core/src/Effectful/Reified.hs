-- | Turn an effect handler into an effectful operation.
module Effectful.Reified
  ( -- * Example
    -- $example

    -- * Effect
    Reified
  , Reified_

    -- ** Handlers
  , runReified
  , runReified_

    -- ** Operations
  , reify
  , reify_
  , reifyWith
  , reifyWith_
  ) where

import Control.Monad
import Data.Coerce
import Data.Functor.Identity
import Data.Kind (Type)
import Data.Primitive.PrimArray

import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import Effectful.Internal.Env (Env(..))
import Effectful.Internal.Utils

-- $example
--
-- >>> import Control.Monad.IO.Class
-- >>> import Effectful.Dispatch.Dynamic
-- >>> import Effectful.State.Static.Local
-- >>> import qualified Data.Map.Strict as M
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
-- the opearation of handling it can be turned into an effectful operation with
-- 'Reified':
--
-- >>> :{
--   action :: Reified_ Write FilePath :> es => Eff es ()
--   action = do
--     reifyWith_ @Write "in.txt" $ do
--       write "hi"
--       write "there"
--     reifyWith_ @Write "out.txt" $ do
--       write "good"
--       write "bye"
-- :}
--
-- Then, given two interpreters:
--
-- >>> :{
--   runWriteIO
--     :: IOE :> es
--     => FilePath
--     -> Eff (Write : es) a
--     -> Eff es a
--   runWriteIO fp = interpret $ \_ -> \case
--     Write msg -> liftIO . putStrLn $ fp ++ ": " ++ msg
-- :}
--
-- >>> :{
--   runWritePure
--     :: State (M.Map FilePath [String]) :> es
--     => FilePath
--     -> Eff (Write : es) a
--     -> Eff es a
--   runWritePure fp = interpret $ \_ -> \case
--     Write msg -> modify $ M.insertWith (++) fp [msg]
-- :}
--
-- @action@ can be supplied with the appropriate one for different behavior:
--
-- >>> :{
--   runEff
--     . runReified_ runWriteIO
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
--     . runReified_ runWritePure
--     $ action
-- :}
-- fromList [("in.txt",["hi","there"]),("out.txt",["good","bye"])]

-- | Provide a way to run a handler of @e@ with a given @input@.
--
-- /Note:/ @f@ can be used to alter the return type of the handler. If you don't
-- need it, use 'Reified_'.
data Reified (e :: Effect) (input :: Type) (f :: Type -> Type) :: Effect

-- | A restricted variant of 'Reified' with unchanged return type of the effect
-- handler.
type Reified_ e input = Reified e input Identity

type instance DispatchOf (Reified e input f) = Static NoSideEffects

data instance StaticRep (Reified e input f) where
  Reified :: !(Env handlerEs)
          -> !(forall r. input -> Eff (e : handlerEs) r -> Eff handlerEs (f r))
          -> StaticRep (Reified e input f)

-- | Run the 'Reified' effect with a given effect handler.
runReified
  :: (forall r. input -> Eff (e : es) r -> Eff es (f r))
  -- ^ The effect handler.
  -> Eff (Reified e input f : es) a
  -> Eff es a
runReified run m = unsafeEff $ \es0 -> do
  inlineBracket
    (consEnv (Reified es0 run) relinkReified es0)
    unconsEnv
    (\es -> unEff m es)

-- | Run the 'Reified' effect with a given effect handler that doesn't change
-- its return type.
runReified_
  :: (forall r. input -> Eff (e : es) r -> Eff es r)
  -- ^ The effect handler.
  -> Eff (Reified_ e input : es) a
  -> Eff es a
runReified_ run = runReified $ \input -> coerce . run input

-- | Run the effect handler.
reify :: Reified e () f :> es => Eff (e : es) a -> Eff es (f a)
reify = reifyWith ()

-- | Run the effect handler with unchanged return type.
reify_ :: Reified_ e () :> es => Eff (e : es) a -> Eff es a
reify_ = reifyWith_ ()

-- | Run the effect handler with a given input.
reifyWith
  :: Reified e input f :> es
  => input
  -- ^ The input to the effect handler.
  -> Eff (e : es) a
  -> Eff es (f a)
reifyWith input action = unsafeEff $ \es -> do
  Reified handlerEs run <- getEnv es
  (`unEff` handlerEs) . run input . unsafeEff $ \eHandlerEs -> do
    unEff action =<< copyRef eHandlerEs es

-- | Run the effect handler that doesn't change its return type with a given
-- input.
reifyWith_
  :: Reified_ e input :> es
  => input
  -- ^ The input to the effect handler.
  -> Eff (e : es) a
  -> Eff es a
reifyWith_ input = adapt . reifyWith input
  where
    adapt :: Eff es (Identity a) -> Eff es a
    adapt = coerce

----------------------------------------
-- Helpers

relinkReified :: Relinker StaticRep (Reified e input f)
relinkReified = Relinker $ \relink (Reified handlerEs run) -> do
  newHandlerEs <- relink handlerEs
  pure $ Reified newHandlerEs run

copyRef :: Env (e : handlerEs) -> Env es -> IO (Env (e : es))
copyRef (Env hoffset hrefs hstorage) (Env offset refs0 storage) = do
  when (hstorage /= storage) $ do
    error "storages do not match"
  let size = sizeofPrimArray refs0 - offset
  mrefs <- newPrimArray (size + 2)
  copyPrimArray mrefs 2 refs0 offset size
  writePrimArray mrefs 0 $ indexPrimArray hrefs  hoffset
  writePrimArray mrefs 1 $ indexPrimArray hrefs (hoffset + 1)
  refs <- unsafeFreezePrimArray mrefs
  pure $ Env 0 refs storage
