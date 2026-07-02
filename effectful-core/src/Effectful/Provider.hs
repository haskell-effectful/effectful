-- | Turn an effect handler into an effectful operation.
--
-- @since 2.3.0.0
module Effectful.Provider
  ( -- * Example
    -- $example

    -- * Effect
    Provider(..)
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

import Data.Coerce
import Data.Functor.Identity
import Data.Kind (Type)
import GHC.Stack

import Effectful
import Effectful.Dispatch.Dynamic

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
--
-- Moreover, operations of the 'Provider' effect can be intercepted with
-- 'interpose', e.g. to adjust the input of the effect handler:
--
-- >>> :{
--   adjustPaths
--     :: Provider_ Write FilePath :> es
--     => Eff es a
--     -> Eff es a
--   adjustPaths = interpose @(Provider_ Write FilePath) $ \env -> \case
--     ProvideWith fp action -> do
--       passthrough env $ ProvideWith ("logs/" ++ fp) action
-- :}
--
-- >>> :{
--   runEff
--     . runProvider_ runWriteIO
--     . adjustPaths
--     $ action
-- :}
-- logs/in.txt: hi
-- logs/in.txt: there
-- logs/out.txt: good
-- logs/out.txt: bye

-- | Provide a way to run a handler of @e@ with a given @input@.
--
-- /Note:/ @f@ can be used to alter the return type of the effect handler. If
-- that's unnecessary, use 'Provider_'.
data Provider (e :: Effect) (input :: Type) (f :: Type -> Type) :: Effect where
  -- | Run the effect handler with a given input.
  --
  -- @since 2.7.0.0
  ProvideWith :: input -> Eff (e : es) a -> Provider e input f (Eff es) (f a)

-- | A restricted variant of 'Provider' with unchanged return type of the effect
-- handler.
type Provider_ e input = Provider e input Identity

type instance DispatchOf (Provider e input f) = Dynamic

-- | Run the 'Provider' effect with a given effect handler.
runProvider
  :: forall e input f es a
   . HasCallStack
  => (forall r. HasCallStack => input -> Eff (e : es) r -> Eff es (f r))
  -- ^ The effect handler.
  -> Eff (Provider e input f : es) a
  -> Eff es a
runProvider provider = interpret $ \env -> \case
  ProvideWith input action -> provider input $ do
    localSeqUnlift env $ \unlift -> do
      localSeqLend @'[e] env $ \lend -> do
        unlift . lend $ action

-- | Run the 'Provider' effect with a given effect handler that doesn't change
-- its return type.
runProvider_
  :: forall e input es a
   . HasCallStack
  => (forall r. HasCallStack => input -> Eff (e : es) r -> Eff es r)
  -- ^ The effect handler.
  -> Eff (Provider_ e input : es) a
  -> Eff es a
runProvider_ provider = interpret $ \env -> \case
  ProvideWith input action -> provider input $ do
    localSeqUnlift env $ \unlift -> do
      localSeqLend @'[e] env $ \lend -> do
        unlift . lend $ coerce action

-- | Run the effect handler.
provide :: (HasCallStack, Provider e () f :> es) => Eff (e : es) a -> Eff es (f a)
provide = send . ProvideWith ()

-- | Run the effect handler with unchanged return type.
provide_ :: (HasCallStack, Provider_ e () :> es) => Eff (e : es) a -> Eff es a
provide_ = dropIdentity . send . ProvideWith ()

-- | Run the effect handler with a given input.
provideWith
  :: (HasCallStack, Provider e input f :> es)
  => input
  -- ^ The input to the effect handler.
  -> Eff (e : es) a
  -> Eff es (f a)
provideWith input = send . ProvideWith input

-- | Run the effect handler that doesn't change its return type with a given
-- input.
provideWith_
  :: (HasCallStack, Provider_ e input :> es)
  => input
  -- ^ The input to the effect handler.
  -> Eff (e : es) a
  -> Eff es a
provideWith_ input = dropIdentity . send . ProvideWith input

----------------------------------------
-- Helpers

dropIdentity :: Eff es (Identity a) -> Eff es a
dropIdentity = coerce
