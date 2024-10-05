{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Dynamically dispatched effects.
module Effectful.Dispatch.Dynamic
  ( -- * Introduction
    -- $intro

    -- ** An example
    -- $example

    -- ** First order and higher order effects
    -- $order

    -- ** Integration with @mtl@ style effects
    -- $integration

    -- *** Functional dependencies
    -- $mtl-fundeps

    -- * Sending operations to the handler
    send

    -- * Handling effects
  , EffectHandler
  , interpret
  , interpretWith
  , reinterpret
  , reinterpretWith
  , interpose
  , interposeWith
  , impose
  , imposeWith

    -- ** Handling local 'Eff' computations
  , LocalEnv

    -- *** Unlifts
  , localSeqUnlift
  , localSeqUnliftIO
  , localUnlift
  , localUnliftIO

    -- *** Lifts
  , localSeqLift
  , localLift
  , withLiftMap
  , withLiftMapIO

    -- *** Bidirectional lifts
  , localLiftUnlift
  , localLiftUnliftIO

    -- *** Misc
  , localSeqLend
  , localLend
  , localSeqBorrow
  , localBorrow
  , SharedSuffix
  , KnownSubset

    -- ** Utils for first order effects
  , EffectHandler_
  , interpret_
  , interpretWith_
  , reinterpret_
  , reinterpretWith_
  , interpose_
  , interposeWith_
  , impose_
  , imposeWith_

    -- * Re-exports
  , HasCallStack
  ) where

import Data.Primitive.PrimArray
import GHC.Stack (HasCallStack)
import GHC.TypeLits

import Effectful.Internal.Effect
import Effectful.Internal.Env
import Effectful.Internal.Monad
import Effectful.Internal.Utils

-- $intro
--
-- A dynamically dispatched effect is a collection of operations that can be
-- interpreted in different ways at runtime, depending on the handler that is
-- used to run the effect.
--
-- This allows a programmer to separate the __what__ from the __how__,
-- i.e. define effects that model what the code should do, while providing
-- handlers that determine how it should do it later. Moreover, different
-- environments can use different handlers to change the behavior of specific
-- parts of the application if appropriate.
--

-- $example
--
-- Let's create an effect for basic file access, i.e. writing and reading files.
--
-- First, we need to define a generalized algebraic data type of kind 'Effect',
-- where each constructor corresponds to a specific operation of the effect in
-- question.
--
-- >>> :{
--   data FileSystem :: Effect where
--     ReadFile  :: FilePath -> FileSystem m String
--     WriteFile :: FilePath -> String -> FileSystem m ()
-- :}
--
-- >>> type instance DispatchOf FileSystem = Dynamic
--
-- The @FileSystem@ effect has two operations:
--
-- - @ReadFile@, which takes a @FilePath@ and returns a @String@ in the monadic
--   context.
--
-- - @WriteFile@, which takes a @FilePath@, a @String@ and returns a @()@ in the
--   monadic context.
--
-- For people familiar with @mtl@ style effects, note that the syntax looks very
-- similar to defining an appropriate type class:
--
-- @
-- class FileSystem m where
--   readFile  :: FilePath -> m String
--   writeFile :: FilePath -> String -> m ()
-- @
--
-- The biggest difference between these two is that the definition of a type
-- class gives us operations as functions, while the definition of an effect
-- gives us operations as data constructors. They can be turned into functions
-- with the help of 'send':
--
-- >>> :{
--   readFile :: (HasCallStack, FileSystem :> es) => FilePath -> Eff es String
--   readFile path = send (ReadFile path)
-- :}
--
-- >>> :{
--   writeFile :: (HasCallStack, FileSystem :> es) => FilePath -> String -> Eff es ()
--   writeFile path content = send (WriteFile path content)
-- :}
--
-- /Note:/ the above functions and the 'DispatchOf' instance can also be
-- automatically generated by the
-- [@makeEffect@](https://hackage.haskell.org/package/effectful-th/docs/Effectful-TH.html#v:makeEffect)
-- function from the
-- [effectful-th](https://hackage.haskell.org/package/effectful-th) package.
--
-- The following defines an 'EffectHandler' that reads and writes files from the
-- drive:
--
-- >>> import Effectful.Error.Static
-- >>> import Effectful.Exception
-- >>> import System.IO qualified as IO
--
-- >>> newtype FsError = FsError String deriving Show
--
-- >>> :{
--  runFileSystemIO
--    :: (IOE :> es, Error FsError :> es)
--    => Eff (FileSystem : es) a
--    -> Eff es a
--  runFileSystemIO = interpret $ \_ -> \case
--    ReadFile path           -> adapt $ IO.readFile path
--    WriteFile path contents -> adapt $ IO.writeFile path contents
--    where
--      adapt m = liftIO m `catch` \(e::IOException) -> throwError . FsError $ show e
-- :}
--
-- Here, we use 'interpret' and simply execute corresponding 'IO' actions for
-- each operation, additionally doing a bit of error management.
--
-- On the other hand, maybe there is a situation in which instead of interacting
-- with the outside world, a pure, in-memory storage is preferred:
--
-- >>> import Data.Map.Strict qualified as M
-- >>> import Effectful.State.Static.Local
--
-- >>> :{
--   runFileSystemPure
--     :: Error FsError :> es
--     => M.Map FilePath String
--     -> Eff (FileSystem : es) a
--     -> Eff es a
--   runFileSystemPure fs0 = reinterpret (evalState fs0) $ \_ -> \case
--     ReadFile path -> gets (M.lookup path) >>= \case
--       Just contents -> pure contents
--       Nothing       -> throwError . FsError $ "File not found: " ++ show path
--     WriteFile path contents -> modify $ M.insert path contents
-- :}
--
-- Here, we use 'reinterpret' and introduce a
-- t'Effectful.State.Static.Local.State' effect for the storage that is private
-- to the effect handler and cannot be accessed outside of it.
--
-- Let's compare how these differ.
--
-- >>> :{
--   action = do
--     file <- readFile "effectful-core.cabal"
--     pure $ length file > 0
-- :}
--
-- >>> :t action
-- action :: (FileSystem :> es) => Eff es Bool
--
-- >>> runEff . runError @FsError . runFileSystemIO $ action
-- Right True
--
-- >>> runPureEff . runErrorNoCallStack @FsError . runFileSystemPure M.empty $ action
-- Left (FsError "File not found: \"effectful-core.cabal\"")
--

-- $order
--
-- Note that the definition of the @FileSystem@ effect from the previous section
-- doesn't use the @m@ type parameter. What is more, when the effect is
-- interpreted, the 'LocalEnv' argument of the 'EffectHandler' is also not
-- used. Such effects are /first order/.
--
-- If an effect makes use of the @m@ parameter, it is a /higher order effect/.
--
-- /Note:/ for handling first order effects you can use 'interpret_' or
-- 'reinterpret_' whose 'EffectHandler_' doesn't take the 'LocalEnv' parameter.
--
-- Interpretation of higher order effects is slightly more involving. To see
-- why, let's consider the @Profiling@ effect for logging how much time a
-- specific action took to run:
--
-- >>> :{
--   data Profiling :: Effect where
--     Profile :: String -> m a -> Profiling m a
-- :}
--
-- >>> type instance DispatchOf Profiling = Dynamic
--
-- >>> :{
--   profile :: (HasCallStack, Profiling :> es) => String -> Eff es a -> Eff es a
--   profile label action = send (Profile label action)
-- :}
--
-- If we naively try to interpret it, we will run into trouble:
--
-- >>> import GHC.Clock (getMonotonicTime)
--
-- >>> :{
--  runProfiling :: IOE :> es => Eff (Profiling : es) a -> Eff es a
--  runProfiling = interpret $ \_ -> \case
--    Profile label action -> do
--      t1 <- liftIO getMonotonicTime
--      r <- action
--      t2 <- liftIO getMonotonicTime
--      liftIO . putStrLn $ "Action '" ++ label ++ "' took " ++ show (t2 - t1) ++ " seconds."
--      pure r
-- :}
-- ...
-- ... Couldn't match type ‘localEs’ with ‘es’
-- ...
--
-- The problem is that @action@ has a type @Eff localEs a@, while the monad of
-- the effect handler is @Eff es@. @localEs@ represents the /local environment/
-- in which the @Profile@ operation was called, which is opaque as the effect
-- handler cannot possibly know how it looks like.
--
-- The solution is to use the 'LocalEnv' that an 'EffectHandler' is given to run
-- the action using one of the functions from the 'localUnlift' family:
--
-- >>> :{
--  runProfiling :: IOE :> es => Eff (Profiling : es) a -> Eff es a
--  runProfiling = interpret $ \env -> \case
--    Profile label action -> localSeqUnliftIO env $ \unlift -> do
--      t1 <- getMonotonicTime
--      r <- unlift action
--      t2 <- getMonotonicTime
--      putStrLn $ "Action '" ++ label ++ "' took " ++ show (t2 - t1) ++ " seconds."
--      pure r
-- :}
--
-- In a similar way we can define a dummy interpreter that does no profiling:
--
-- >>> :{
--  runNoProfiling :: Eff (Profiling : es) a -> Eff es a
--  runNoProfiling = interpret $ \env -> \case
--    Profile label action -> localSeqUnlift env $ \unlift -> unlift action
-- :}
--
-- ...and it's done.
--
-- >>> action = profile "greet" . liftIO $ putStrLn "Hello!"
--
-- >>> :t action
-- action :: (Profiling :> es, IOE :> es) => Eff es ()
--
-- >>> runEff . runProfiling $ action
-- Hello!
-- Action 'greet' took ... seconds.
--
-- >>> runEff . runNoProfiling $ action
-- Hello!
--

-- $integration
--
-- #integration#
--
-- There exists a lot of libraries that provide their functionality as an @mtl@
-- style effect, which generally speaking is a type class that contains core
-- operations of the library in question.
--
-- Such effects are quite easy to use with the 'Eff' monad. As an example,
-- consider the @mtl@ style effect for generation of random numbers:
--
-- >>> :{
--   class Monad m => MonadRNG m where
--     randomInt :: m Int
-- :}
--
-- Let's say the library also defines a helper function for generation of random
-- strings:
--
-- >>> import Control.Monad
-- >>> import Data.Char
--
-- >>> :{
--  randomString :: MonadRNG m => Int -> m String
--  randomString n = map chr <$> replicateM n randomInt
-- :}
--
-- To make it possible to use it with the 'Eff' monad, the first step is to
-- create an effect with operations that mirror the ones of a type class:
--
-- >>> :{
--   data RNG :: Effect where
--     RandomInt :: RNG m Int
-- :}
--
-- >>> type instance DispatchOf RNG = Dynamic
--
-- If we continued as in the example above, we'd now create top level helper
-- functions that execute effect operations using 'send', in this case
-- @randomInt@ tied to @RandomInt@. But this function is already declared by the
-- @MonadRNG@ type class! Therefore, what we do instead is provide an
-- __orphan__, __canonical__ instance of @MonadRNG@ for 'Eff' that delegates to
-- the @RNG@ effect:
--
-- >>> :set -XUndecidableInstances
--
-- >>> :{
--   instance RNG :> es => MonadRNG (Eff es) where
--     randomInt = send RandomInt
-- :}
--
-- Now we only need an interpreter:
--
-- >>> :{
--   runDummyRNG :: Eff (RNG : es) a -> Eff es a
--   runDummyRNG = interpret_ $ \case
--     RandomInt -> pure 55
-- :}
--
-- and we can use any function that requires a @MonadRNG@ constraint with the
-- 'Eff' monad as long as the @RNG@ effect is in place:
--
-- >>> runEff . runDummyRNG $ randomString 3
-- "777"
--

-- $mtl-fundeps
--
-- For dealing with classes that employ functional dependencies an additional
-- trick is needed.
--
-- Consider the following:
--
-- >>> :set -XFunctionalDependencies
--
-- >>> :{
--   class Monad m => MonadInput i m | m -> i where
--     input :: m i
-- :}
--
-- An attempt to define the instance as in the example above leads to violation
-- of the liberal coverage condition:
--
-- >>> :{
--   instance Reader i :> es => MonadInput i (Eff es) where
--     input = ask
-- :}
-- ...
-- ...Illegal instance declaration for ‘MonadInput i (Eff es)’...
-- ...  The liberal coverage condition fails in class ‘MonadInput’...
-- ...    for functional dependency: ‘m -> i’...
-- ...
--
-- However, there exists a [dirty
-- trick](https://www.youtube.com/watch?v=ZXtdd8e7CQQ) for bypassing the
-- coverage condition, i.e. including the instance head in the context:
--
-- >>> :{
--   instance (MonadInput i (Eff es), Reader i :> es) => MonadInput i (Eff es) where
--     input = ask
-- :}
--
-- Now the @MonadInput@ class can be used with the 'Eff' monad:
--
-- >>> :{
--   double :: MonadInput Int m => m Int
--   double = (+) <$> input <*> input
-- :}
--
-- >>> runPureEff . runReader @Int 3 $ double
-- 6

----------------------------------------
-- Handling effects

-- | Interpret an effect.
--
-- /Note:/ 'interpret' can be turned into a 'reinterpret' with the use of
-- 'inject'.
interpret
  :: (HasCallStack, DispatchOf e ~ Dynamic)
  => EffectHandler e es
  -- ^ The effect handler.
  -> Eff (e : es) a
  -> Eff      es  a
interpret handler m = unsafeEff $ \es -> do
  (`unEff` es) $ runHandler (mkHandler es) m
  where
    mkHandler es = Handler es (let ?callStack = thawCallStack ?callStack in handler)

-- | 'interpret' with the effect handler as the last argument.
--
-- @since 2.4.0.0
interpretWith
  :: (HasCallStack, DispatchOf e ~ Dynamic)
  => Eff (e : es) a
  -> EffectHandler e es
  -- ^ The effect handler.
  -> Eff      es  a
interpretWith m handler = interpret handler m

-- | Interpret an effect using other, private effects.
--
-- @'interpret' ≡ 'reinterpret' 'id'@
reinterpret
  :: (HasCallStack, DispatchOf e ~ Dynamic)
  => (Eff handlerEs a -> Eff es b)
  -- ^ Introduction of effects encapsulated within the handler.
  -> EffectHandler e handlerEs
  -- ^ The effect handler.
  -> Eff (e : es) a
  -> Eff      es  b
reinterpret runHandlerEs handler m = unsafeEff $ \es -> do
  (`unEff` es) . runHandlerEs . unsafeEff $ \handlerEs -> do
    (`unEff` es) $ runHandler (mkHandler handlerEs) m
  where
    mkHandler es = Handler es (let ?callStack = thawCallStack ?callStack in handler)

-- | 'reinterpret' with the effect handler as the last argument.
--
-- @since 2.4.0.0
reinterpretWith
  :: (HasCallStack, DispatchOf e ~ Dynamic)
  => (Eff handlerEs a -> Eff es b)
  -- ^ Introduction of effects encapsulated within the handler.
  -> Eff (e : es) a
  -> EffectHandler e handlerEs
  -- ^ The effect handler.
  -> Eff      es  b
reinterpretWith runHandlerEs m handler = reinterpret runHandlerEs handler m

-- | Replace the handler of an existing effect with a new one.
--
-- /Note:/ this function allows for augmenting handlers with a new functionality
-- as the new handler can send operations to the old one.
--
-- >>> :{
--   data E :: Effect where
--     Op1 :: E m ()
--     Op2 :: E m ()
--   type instance DispatchOf E = Dynamic
-- :}
--
-- >>> :{
--   runE :: IOE :> es => Eff (E : es) a -> Eff es a
--   runE = interpret_ $ \case
--     Op1 -> liftIO (putStrLn "op1")
--     Op2 -> liftIO (putStrLn "op2")
-- :}
--
-- >>> runEff . runE $ send Op1 >> send Op2
-- op1
-- op2
--
-- >>> :{
--   augmentOp2 :: (E :> es, IOE :> es) => Eff es a -> Eff es a
--   augmentOp2 = interpose_ $ \case
--     Op1 -> send Op1
--     Op2 -> liftIO (putStrLn "augmented op2") >> send Op2
-- :}
--
-- >>> runEff . runE . augmentOp2 $ send Op1 >> send Op2
-- op1
-- augmented op2
-- op2
--
-- /Note:/ when using 'interpose' to modify only specific operations of the
-- effect, your first instinct might be to match on them, then handle the rest
-- with a generic match. Unfortunately, this doesn't work out of the box:
--
-- >>> :{
--   genericAugmentOp2 :: (E :> es, IOE :> es) => Eff es a -> Eff es a
--   genericAugmentOp2 = interpose_ $ \case
--     Op2 -> liftIO (putStrLn "augmented op2") >> send Op2
--     op  -> send op
-- :}
-- ...
-- ...Couldn't match type ‘localEs’ with ‘es’
-- ...
--
-- This is because within the generic match, 'send' expects @Op (Eff es) a@, but
-- @op@ has a type @Op (Eff localEs) a@. If the effect in question is first
-- order (i.e. its @m@ type parameter is phantom), you can use 'coerce':
--
-- >>> import Data.Coerce
-- >>> :{
--   genericAugmentOp2 :: (E :> es, IOE :> es) => Eff es a -> Eff es a
--   genericAugmentOp2 = interpose_ $ \case
--     Op2 -> liftIO (putStrLn "augmented op2") >> send Op2
--     op  -> send @E (coerce op)
-- :}
--
-- >>> runEff . runE . genericAugmentOp2 $ send Op1 >> send Op2
-- op1
-- augmented op2
-- op2
--
-- On the other hand, when dealing with higher order effects you need to pattern
-- match on each operation and unlift where necessary.
--
interpose
  :: forall e es a. (HasCallStack, DispatchOf e ~ Dynamic, e :> es)
  => EffectHandler e es
  -- ^ The effect handler.
  -> Eff es a
  -> Eff es a
interpose handler m = unsafeEff $ \es -> do
  inlineBracket
    (do
        origHandler <- getEnv @e es
        replaceEnv origHandler relinkHandler es
    )
    (\newEs -> do
        -- Restore the original handler.
        putEnv es =<< getEnv @e newEs
        unreplaceEnv @e newEs
    )
    (\newEs -> do
        -- Replace the original handler with a new one. Note that 'newEs'
        -- will still see the original handler.
        putEnv es $ mkHandler newEs
        unEff m es
    )
  where
    mkHandler es = Handler es (let ?callStack = thawCallStack ?callStack in handler)

-- | 'interpose' with the effect handler as the last argument.
--
-- @since 2.4.0.0
interposeWith
  :: (HasCallStack, DispatchOf e ~ Dynamic, e :> es)
  => Eff es a
  -> EffectHandler e es
  -- ^ The effect handler.
  -> Eff es a
interposeWith m handler = interpose handler m

-- | Replace the handler of an existing effect with a new one that uses other,
-- private effects.
--
-- @'interpose' ≡ 'impose' 'id'@
impose
  :: forall e es handlerEs a b. (HasCallStack, DispatchOf e ~ Dynamic, e :> es)
  => (Eff handlerEs a -> Eff es b)
  -- ^ Introduction of effects encapsulated within the handler.
  -> EffectHandler e handlerEs
  -- ^ The effect handler.
  -> Eff es a
  -> Eff es b
impose runHandlerEs handler m = unsafeEff $ \es -> do
  inlineBracket
    (do
        origHandler <- getEnv @e es
        replaceEnv origHandler relinkHandler es
    )
    (\newEs -> do
        -- Restore the original handler.
        putEnv es =<< getEnv @e newEs
        unreplaceEnv @e newEs
    )
    (\newEs -> do
        (`unEff` newEs) . runHandlerEs . unsafeEff $ \handlerEs -> do
          -- Replace the original handler with a new one. Note that
          -- 'newEs' (and thus 'handlerEs') wil still see the original
          -- handler.
          putEnv es $ mkHandler handlerEs
          unEff m es
    )
  where
    mkHandler es = Handler es (let ?callStack = thawCallStack ?callStack in handler)

-- | 'impose' with the effect handler as the last argument.
--
-- @since 2.4.0.0
imposeWith
  :: (HasCallStack, DispatchOf e ~ Dynamic, e :> es)
  => (Eff handlerEs a -> Eff es b)
  -- ^ Introduction of effects encapsulated within the handler.
  -> Eff es a
  -> EffectHandler e handlerEs
  -- ^ The effect handler.
  -> Eff es b
imposeWith runHandlerEs m handler = impose runHandlerEs handler m

----------------------------------------
-- First order effects

-- | Type signature of a first order effect handler.
--
-- @since 2.4.0.0
type EffectHandler_ (e :: Effect) (es :: [Effect])
  = forall a localEs. HasCallStack
  => e (Eff localEs) a
  -- ^ The operation.
  -> Eff es a

-- | 'interpret' for first order effects.
--
-- @since 2.4.0.0
interpret_
  :: (HasCallStack, DispatchOf e ~ Dynamic)
  => EffectHandler_ e es
  -- ^ The effect handler.
  -> Eff (e : es) a
  -> Eff      es  a
interpret_ handler = interpret (const handler)

-- | 'interpretWith' for first order effects.
--
-- @since 2.4.0.0
interpretWith_
  :: (HasCallStack, DispatchOf e ~ Dynamic)
  => Eff (e : es) a
  -> EffectHandler_ e es
  -- ^ The effect handler.
  -> Eff      es  a
interpretWith_ m handler = interpretWith m (const handler)

-- | 'reinterpret' for first order effects.
--
-- @since 2.4.0.0
reinterpret_
  :: (HasCallStack, DispatchOf e ~ Dynamic)
  => (Eff handlerEs a -> Eff es b)
  -- ^ Introduction of effects encapsulated within the handler.
  -> EffectHandler_ e handlerEs
  -- ^ The effect handler.
  -> Eff (e : es) a
  -> Eff      es  b
reinterpret_ runHandlerEs handler = reinterpret runHandlerEs (const handler)

-- | 'reinterpretWith' for first order effects.
--
-- @since 2.4.0.0
reinterpretWith_
  :: (HasCallStack, DispatchOf e ~ Dynamic)
  => (Eff handlerEs a -> Eff es b)
  -- ^ Introduction of effects encapsulated within the handler.
  -> Eff (e : es) a
  -> EffectHandler_ e handlerEs
  -- ^ The effect handler.
  -> Eff      es  b
reinterpretWith_ runHandlerEs m handler = reinterpretWith runHandlerEs m (const handler)

-- | 'interpose' for first order effects.
--
-- @since 2.4.0.0
interpose_
  :: (HasCallStack, DispatchOf e ~ Dynamic, e :> es)
  => EffectHandler_ e es
  -- ^ The effect handler.
  -> Eff es a
  -> Eff es a
interpose_ handler = interpose (const handler)

-- | 'interposeWith' for first order effects.
--
-- @since 2.4.0.0
interposeWith_
  :: (HasCallStack, DispatchOf e ~ Dynamic, e :> es)
  => Eff es a
  -> EffectHandler_ e es
  -- ^ The effect handler.
  -> Eff es a
interposeWith_ m handler = interposeWith m (const handler)

-- | 'impose' for first order effects.
--
-- @since 2.4.0.0
impose_
  :: (HasCallStack, DispatchOf e ~ Dynamic, e :> es)
  => (Eff handlerEs a -> Eff es b)
  -- ^ Introduction of effects encapsulated within the handler.
  -> EffectHandler_ e handlerEs
  -- ^ The effect handler.
  -> Eff es a
  -> Eff es b
impose_ runHandlerEs handler = impose runHandlerEs (const handler)

-- | 'imposeWith' for first order effects.
--
-- @since 2.4.0.0
imposeWith_
  :: (HasCallStack, DispatchOf e ~ Dynamic, e :> es)
  => (Eff handlerEs a -> Eff es b)
  -- ^ Introduction of effects encapsulated within the handler.
  -> Eff es a
  -> EffectHandler_ e handlerEs
  -- ^ The effect handler.
  -> Eff es b
imposeWith_ runHandlerEs m handler = imposeWith runHandlerEs m (const handler)

----------------------------------------
-- Unlifts

-- | Create a local unlifting function with the 'SeqUnlift' strategy. For the
-- general version see 'localUnlift'.
localSeqUnlift
  :: (HasCallStack, SharedSuffix es handlerEs)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> ((forall r. Eff localEs r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localSeqUnlift (LocalEnv les) k = unsafeEff $ \es -> do
  requireMatchingStorages es les
  seqUnliftIO les $ \unlift -> do
    (`unEff` es) $ k $ unsafeEff_ . unlift
{-# INLINE localSeqUnlift #-}

-- | Create a local unlifting function with the 'SeqUnlift' strategy. For the
-- general version see 'localUnliftIO'.
localSeqUnliftIO
  :: (HasCallStack, SharedSuffix es handlerEs, IOE :> es)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> ((forall r. Eff localEs r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localSeqUnliftIO (LocalEnv les) k = unsafeEff $ \es -> do
  requireMatchingStorages es les
  seqUnliftIO les k
{-# INLINE localSeqUnliftIO #-}

-- | Create a local unlifting function with the given strategy.
localUnlift
  :: (HasCallStack, SharedSuffix es handlerEs)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. Eff localEs r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localUnlift (LocalEnv les) strategy k = unsafeEff $ \es -> do
  requireMatchingStorages es les
  case strategy of
    SeqUnlift -> seqUnliftIO les $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift
    SeqForkUnlift -> seqForkUnliftIO les $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift
    ConcUnlift p l -> concUnliftIO les p l $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift
{-# INLINE localUnlift #-}

-- | Create a local unlifting function with the given strategy.
localUnliftIO
  :: (HasCallStack, SharedSuffix es handlerEs, IOE :> es)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. Eff localEs r -> IO r) -> IO a)
  -- ^ Continuation with the unlifting function in scope.
  -> Eff es a
localUnliftIO (LocalEnv les) strategy k = unsafeEff $ \es -> do
  requireMatchingStorages es les
  case strategy of
    SeqUnlift -> seqUnliftIO les k
    SeqForkUnlift -> seqForkUnliftIO les k
    ConcUnlift p l -> concUnliftIO les p l k
{-# INLINE localUnliftIO #-}

----------------------------------------
-- Lifts

-- | Create a local lifting function with the 'SeqUnlift' strategy. For the
-- general version see 'localLift'.
--
-- @since 2.2.1.0
localSeqLift
  :: (HasCallStack, SharedSuffix es handlerEs)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> ((forall r. Eff es r -> Eff localEs r) -> Eff es a)
  -- ^ Continuation with the lifting function in scope.
  -> Eff es a
localSeqLift (LocalEnv les) k = unsafeEff $ \es -> do
  requireMatchingStorages es les
  seqUnliftIO es $ \unlift -> do
    (`unEff` es) $ k $ unsafeEff_ . unlift
{-# INLINE localSeqLift #-}

-- | Create a local lifting function with the given strategy.
--
-- @since 2.2.1.0
localLift
  :: (HasCallStack, SharedSuffix es handlerEs)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. Eff es r -> Eff localEs r) -> Eff es a)
  -- ^ Continuation with the lifting function in scope.
  -> Eff es a
localLift (LocalEnv les) strategy k = unsafeEff $ \es -> do
  requireMatchingStorages es les
  case strategy of
    SeqUnlift -> seqUnliftIO es $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift
    SeqForkUnlift -> seqForkUnliftIO es $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift
    ConcUnlift p l -> concUnliftIO es p l $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift
{-# INLINE localLift #-}

-- | Utility for lifting 'Eff' computations of type
--
-- @'Eff' es a -> 'Eff' es b@
--
-- to
--
-- @'Eff' localEs a -> 'Eff' localEs b@
--
-- /Note:/ the computation must not run its argument in a different thread,
-- attempting to do so will result in a runtime error.
withLiftMap
  :: (HasCallStack, SharedSuffix es handlerEs)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> ((forall a b. (Eff es a -> Eff es b) -> Eff localEs a -> Eff localEs b) -> Eff es r)
  -- ^ Continuation with the lifting function in scope.
  -> Eff es r
withLiftMap (LocalEnv les) k = unsafeEff $ \es -> do
  requireMatchingStorages es les
  (`unEff` es) $ k $ \mapEff m -> unsafeEff $ \localEs -> do
    seqUnliftIO localEs $ \unlift -> do
      (`unEff` es) . mapEff . unsafeEff_ $ unlift m
{-# INLINE withLiftMap #-}

-- | Utility for lifting 'IO' computations of type
--
-- @'IO' a -> 'IO' b@
--
-- to
--
-- @'Eff' localEs a -> 'Eff' localEs b@
--
-- /Note:/ the computation must not run its argument in a different thread,
-- attempting to do so will result in a runtime error.
--
-- Useful e.g. for lifting the unmasking function in
-- 'Control.Exception.mask'-like computations:
--
-- >>> :{
-- data Fork :: Effect where
--   ForkWithUnmask :: ((forall a. m a -> m a) -> m ()) -> Fork m ThreadId
-- type instance DispatchOf Fork = Dynamic
-- :}
--
-- >>> :{
-- runFork :: IOE :> es => Eff (Fork : es) a -> Eff es a
-- runFork = interpret $ \env (ForkWithUnmask m) -> withLiftMapIO env $ \liftMap -> do
--   localUnliftIO env (ConcUnlift Ephemeral $ Limited 1) $ \unlift -> do
--     forkIOWithUnmask $ \unmask -> unlift $ m $ liftMap unmask
-- :}
withLiftMapIO
  :: (HasCallStack, SharedSuffix es handlerEs, IOE :> es)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> ((forall a b. (IO a -> IO b) -> Eff localEs a -> Eff localEs b) -> Eff es r)
  -- ^ Continuation with the lifting function in scope.
  -> Eff es r
withLiftMapIO (LocalEnv les) k = k $ \mapIO m -> unsafeEff $ \es -> do
  requireMatchingStorages es les
  seqUnliftIO es $ \unlift -> mapIO $ unlift m
{-# INLINE withLiftMapIO #-}

----------------------------------------
-- Bidirectional lifts

-- | Create a local lifting and unlifting function with the given strategy.
--
-- Useful for lifting complicated 'Eff' computations where the monadic action
-- shows in both positive (as a result) and negative (as an argument) position.
--
-- /Note:/ depending on the computation you're lifting 'localUnlift' along with
-- 'withLiftMap' might be enough and is more efficient.
localLiftUnlift
  :: (HasCallStack, SharedSuffix es handlerEs)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. Eff es r -> Eff localEs r) -> (forall r. Eff localEs r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the lifting and unlifting function in scope.
  -> Eff es a
localLiftUnlift (LocalEnv les) strategy k = unsafeEff $ \es -> do
  requireMatchingStorages es les
  case strategy of
    SeqUnlift -> seqUnliftIO es $ \unliftEs -> do
      seqUnliftIO les $ \unliftLocalEs -> do
        (`unEff` es) $ k (unsafeEff_ . unliftEs) (unsafeEff_ . unliftLocalEs)
    SeqForkUnlift -> seqForkUnliftIO es $ \unliftEs -> do
      seqForkUnliftIO les $ \unliftLocalEs -> do
        (`unEff` es) $ k (unsafeEff_ . unliftEs) (unsafeEff_ . unliftLocalEs)
    ConcUnlift p l -> concUnliftIO es p l $ \unliftEs -> do
      concUnliftIO les p l $ \unliftLocalEs -> do
        (`unEff` es) $ k (unsafeEff_ . unliftEs) (unsafeEff_ . unliftLocalEs)
{-# INLINE localLiftUnlift #-}

-- | Create a local unlifting function with the given strategy along with an
-- unrestricted lifting function.
--
-- Useful for lifting complicated 'IO' computations where the monadic action
-- shows in both positive (as a result) and negative (as an argument) position.
--
-- /Note:/ depending on the computation you're lifting 'localUnliftIO' along
-- with 'withLiftMapIO' might be enough and is more efficient.
localLiftUnliftIO
  :: (HasCallStack, SharedSuffix es handlerEs, IOE :> es)
  => LocalEnv localEs handlerEs
  -- ^ Local environment.
  -> UnliftStrategy
  -> ((forall r. IO r -> Eff localEs r) -> (forall r. Eff localEs r -> IO r) -> IO a)
  -- ^ Continuation with the lifting and unlifting function in scope.
  -> Eff es a
localLiftUnliftIO (LocalEnv les) strategy k = unsafeEff $ \es -> do
  requireMatchingStorages es les
  case strategy of
    SeqUnlift      -> seqUnliftIO les $ k unsafeEff_
    SeqForkUnlift  -> seqForkUnliftIO les $ k unsafeEff_
    ConcUnlift p l -> concUnliftIO les p l $ k unsafeEff_
{-# INLINE localLiftUnliftIO #-}

----------------------------------------
-- Misc

-- | Lend effects to the local environment.
--
-- Consider the following effect:
--
-- >>> :{
--   data D :: Effect where
--     D :: D m ()
--   type instance DispatchOf D = Dynamic
-- :}
--
-- and an auxiliary effect that requires both @IOE@ and @D@ to run:
--
-- >>> :{
--   data E :: Effect
--   runE :: (IOE :> es, D :> es) => Eff (E : es) a -> Eff es a
--   runE = error "runE"
-- :}
--
-- Trying to use @runE@ inside the handler of @D@ doesn't work out of the box:
--
-- >>> :{
--   runD :: IOE :> es => Eff (D : es) a -> Eff es a
--   runD = interpret $ \env -> \case
--     D -> localSeqUnlift env $ \unlift -> do
--       unlift . runE $ pure ()
-- :}
-- ...
-- ...Could not deduce ...IOE :> localEs... arising from a use of ‘runE’
-- ...from the context: IOE :> es
-- ...
--
-- The problem is that @runE@ needs @IOE :> localEs@, but only @IOE :> es@ is
-- available. This function allows us to bridge the gap:
--
-- >>> :{
--   runD :: IOE :> es => Eff (D : es) a -> Eff es a
--   runD = interpret $ \env -> \case
--     D -> localSeqUnlift env $ \unlift -> do
--       localSeqLend @'[IOE] env $ \useIOE -> do
--         unlift . useIOE . runE $ pure ()
-- :}
--
-- @since 2.4.0.0
localSeqLend
  :: forall lentEs es handlerEs localEs a
   . (HasCallStack, KnownSubset lentEs es, SharedSuffix es handlerEs)
  => LocalEnv localEs handlerEs
  -> ((forall r. Eff (lentEs ++ localEs) r -> Eff localEs r) -> Eff es a)
  -- ^ Continuation with the lent handler in scope.
  -> Eff es a
localSeqLend (LocalEnv les) k = unsafeEff $ \es -> do
  eles <- copyRefs @lentEs es les
  seqUnliftIO eles $ \unlift -> (`unEff` es) $ k $ unsafeEff_ . unlift
{-# INLINE localSeqLend #-}

-- | Lend effects to the local environment with a given unlifting strategy.
--
-- Generalizes 'localSeqLend'.
--
-- @since 2.4.0.0
localLend
  :: forall lentEs es handlerEs localEs a
   . (HasCallStack, KnownSubset lentEs es, SharedSuffix es handlerEs)
  => LocalEnv localEs handlerEs
  -> UnliftStrategy
  -> ((forall r. Eff (lentEs ++ localEs) r -> Eff localEs r) -> Eff es a)
  -- ^ Continuation with the lent handler in scope.
  -> Eff es a
localLend (LocalEnv les) strategy k = unsafeEff $ \es -> do
  eles <- copyRefs @lentEs es les
  case strategy of
    SeqUnlift -> seqUnliftIO eles $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift
    SeqForkUnlift -> seqForkUnliftIO eles $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift
    ConcUnlift p l -> concUnliftIO eles p l $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift
{-# INLINE localLend #-}

-- | Borrow effects from the local environment.
--
-- @since 2.4.0.0
localSeqBorrow
  :: forall borrowedEs es handlerEs localEs a
   . (HasCallStack, KnownSubset borrowedEs localEs, SharedSuffix es handlerEs)
  => LocalEnv localEs handlerEs
  -> ((forall r. Eff (borrowedEs ++ es) r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the borrowed handler in scope.
  -> Eff es a
localSeqBorrow (LocalEnv les) k = unsafeEff $ \es -> do
  ees <- copyRefs @borrowedEs les es
  seqUnliftIO ees $ \unlift -> (`unEff` es) $ k $ unsafeEff_ . unlift
{-# INLINE localSeqBorrow #-}

-- | Borrow effects from the local environment with a given unlifting
-- strategy.
--
-- Generalizes 'localSeqBorrow'.
--
-- @since 2.4.0.0
localBorrow
  :: forall borrowedEs es handlerEs localEs a
   . (HasCallStack, KnownSubset borrowedEs localEs, SharedSuffix es handlerEs)
  => LocalEnv localEs handlerEs
  -> UnliftStrategy
  -> ((forall r. Eff (borrowedEs ++ es) r -> Eff es r) -> Eff es a)
  -- ^ Continuation with the borrowed handler in scope.
  -> Eff es a
localBorrow (LocalEnv les) strategy k = unsafeEff $ \es -> do
  ees <- copyRefs @borrowedEs les es
  case strategy of
    SeqUnlift -> seqUnliftIO ees $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift
    SeqForkUnlift -> seqForkUnliftIO ees $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift
    ConcUnlift p l -> concUnliftIO ees p l $ \unlift -> do
      (`unEff` es) $ k $ unsafeEff_ . unlift
{-# INLINE localBorrow #-}

copyRefs
  :: forall es srcEs destEs
   . (HasCallStack, KnownSubset es srcEs)
  => Env srcEs
  -> Env destEs
  -> IO (Env (es ++ destEs))
copyRefs src@(Env soffset srefs _) dest@(Env doffset drefs storage) = do
  requireMatchingStorages src dest
  let size = sizeofPrimArray drefs - doffset
      es = reifyIndices @es @srcEs
      esSize = 2 * length es
  mrefs <- newPrimArray (esSize + size)
  copyPrimArray mrefs esSize drefs doffset size
  let writeRefs i = \case
        [] -> pure ()
        (x : xs) -> do
          let ix = soffset + 2 * x
          writePrimArray mrefs  i      $ indexPrimArray srefs  ix
          writePrimArray mrefs (i + 1) $ indexPrimArray srefs (ix + 1)
          writeRefs (i + 2) xs
  writeRefs 0 es
  refs <- unsafeFreezePrimArray mrefs
  pure $ Env 0 refs storage
{-# NOINLINE copyRefs #-}

requireMatchingStorages :: HasCallStack => Env es1 -> Env es2 -> IO ()
requireMatchingStorages es1 es2
  | envStorage es1 /= envStorage es2 = error
    $ "Env and LocalEnv point to different Storages.\n"
    ++ "If you passed LocalEnv to a different thread and tried to create an "
    ++ "unlifting function there, it's not allowed. You need to create it in "
    ++ "the thread of the effect handler."
  | otherwise = pure ()

-- | Require that both effect stacks share an opaque suffix.
--
-- Functions from the 'localUnlift' family utilize this constraint to guarantee
-- sensible usage of unlifting functions.
--
-- As an example, consider the following higher order effect:
--
-- >>> :{
--   data E :: Effect where
--     E :: m a -> E m a
--   type instance DispatchOf E = Dynamic
-- :}
--
-- Running local actions in a more specific environment is fine:
--
-- >>> :{
--  runE1 :: Eff (E : es) a -> Eff es a
--  runE1 = interpret $ \env -> \case
--    E m -> runReader () $ do
--      localSeqUnlift env $ \unlift -> unlift m
-- :}
--
-- Running local actions in a more general environment is fine:
--
-- >>> :{
--  runE2 :: Eff (E : es) a -> Eff es a
--  runE2 = reinterpret (runReader ()) $ \env -> \case
--    E m -> raise $ do
--      localSeqUnlift env $ \unlift -> unlift m
-- :}
--
-- However, running local actions in an unrelated environment is not fine as
-- this would make it possible to run anything within 'runPureEff':
--
-- >>> :{
--  runE3 :: Eff (E : es) a -> Eff es a
--  runE3 = reinterpret (runReader ()) $ \env -> \case
--    E m -> pure . runPureEff $ do
--      localSeqUnlift env $ \unlift -> unlift m
-- :}
-- ...
-- ...Could not deduce ...SharedSuffix '[] es...
-- ...
--
-- Running local actions in a monomorphic effect stack is also not fine as
-- this makes a special case of the above possible:
--
-- >>> :{
--  runE4 :: Eff [E, IOE] a -> Eff '[IOE] a
--  runE4 = interpret $ \env -> \case
--    E m -> pure . runPureEff $ do
--      localSeqUnlift env $ \unlift -> unlift m
-- :}
-- ...
-- ...Running local actions in monomorphic effect stacks is not supported...
-- ...
--
-- @since 1.2.0.0
class SharedSuffix (es1 :: [Effect]) (es2 :: [Effect])

instance {-# INCOHERENT #-} SharedSuffix es es
instance {-# INCOHERENT #-} SharedSuffix es1 es2 => SharedSuffix (e : es1) es2
instance {-# INCOHERENT #-} SharedSuffix es1 es2 => SharedSuffix es1 (e : es2)

-- | This is always preferred to @SharedSuffix es es@ as it's not incoherent.
instance
  TypeError
  ( Text "Running local actions in monomorphic effect stacks is not supported." :$$:
    Text "As a solution simply change the stack to have a polymorphic suffix."
  ) => SharedSuffix '[] '[]

-- $setup
-- >>> import Control.Concurrent (ThreadId, forkIOWithUnmask)
-- >>> import Control.Monad.IO.Class
-- >>> import Effectful.Reader.Static
