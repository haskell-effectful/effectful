{-# OPTIONS_HADDOCK not-home #-}
-- | The enviroment for 'Effective.Internal.Monad.Eff'.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Effective.Internal.Env
  ( Env

  -- * Safe operations
  , emptyEnv
  , cloneEnv
  , sizeEnv
  , takeLastEnv
  , getEnv
  , checkSizeEnv

  -- * Extending and shrinking
  , unsafeReplaceEnv
  , unsafeConsEnv
  , unsafeAppendEnv
  , unsafeTrimEnv

  -- * Data retrieval and update
  , unsafePutEnv
  , unsafeModifyEnv
  , unsafeStateEnv
  ) where

import Control.Monad
import Control.Monad.Primitive
import Data.IORef
import Data.Primitive.SmallArray
import GHC.Exts (Any)
import GHC.Stack
import Unsafe.Coerce

import Effective.Internal.Has

type role Env nominal

-- | A mutable, extensible record indexed by effect data types.
newtype Env (es :: [Effect]) = Env (IORef EnvRef)

data EnvRef = EnvRef Int (SmallMutableArray RealWorld Any)

-- | Create an empty environment.
emptyEnv :: IO (Env '[])
emptyEnv = fmap Env . newIORef . EnvRef 0
  =<< newSmallArray 0 (error "undefined field")

-- | Clone the environment.
cloneEnv :: Env es -> IO (Env es)
cloneEnv (Env ref) = do
  EnvRef n es <- readIORef ref
  fmap Env . newIORef . EnvRef n
    =<< cloneSmallMutableArray es 0 (sizeofSmallMutableArray es)

-- | Get the current size of the environment.
sizeEnv :: Env es -> IO Int
sizeEnv (Env ref) = do
  EnvRef n _ <- readIORef ref
  pure n

-- | Take last @k@ values from the top of the environment.
takeLastEnv :: HasCallStack => Int -> Env es0 -> IO (Env es)
takeLastEnv k (Env ref) = do
  EnvRef n es <- readIORef ref
  if k > n
    then error $ "k (" ++ show k ++ ") > n (" ++ show n ++ ")"
    else fmap Env . newIORef . EnvRef k =<< cloneSmallMutableArray es (n - k) k

-- | Extract a specific data type from the environment.
getEnv :: forall e es. (HasCallStack, e :> es) => Env es -> IO e
getEnv (Env ref) = do
  EnvRef n es <- readIORef ref
  fromAny <$> readSmallArray es (ixEnv @e @es n)

-- | Check that the size of the environment is the same as the expected value.
checkSizeEnv :: HasCallStack => Int -> Env es -> IO ()
checkSizeEnv k (Env ref) = do
  EnvRef n _ <- readIORef ref
  when (k /= n) . error $ "k (" ++ show k ++ ") /= n (" ++ show n ++ ")"

----------------------------------------
-- Extending and shrinking

-- | Replace the first argument with the second one in place.
unsafeReplaceEnv :: HasCallStack => Env es -> Env es -> IO ()
unsafeReplaceEnv (Env ref0) (Env ref1) = writeIORef ref0 =<< readIORef ref1

-- | Extend the environment with a new data type in place.
unsafeConsEnv :: HasCallStack => e -> Env es -> IO (Env (e : es))
unsafeConsEnv e (Env ref) = do
  EnvRef n es0 <- readIORef ref
  let len = sizeofSmallMutableArray es0
  case n `compare` len of
    GT -> error $ "n (" ++ show n ++ ") > len (" ++ show len ++ ")"
    LT -> do
      e `seq` writeSmallArray es0 n (toAny e)
      writeIORef ref $! EnvRef (n + 1) es0
    EQ -> do
      let newLen = max 1 len * 2
      es <- newSmallArray newLen (error "undefined field")
      copySmallMutableArray es 0 es0 0 len
      e `seq` writeSmallArray es n (toAny e)
      writeIORef ref $! EnvRef (n + 1) es
  pure $ Env ref
{-# NOINLINE unsafeConsEnv #-}

-- | Extend the first environment with the second one in place.
unsafeAppendEnv :: HasCallStack => Env es0 -> Env es1 -> IO (Env es)
unsafeAppendEnv (Env ref0) (Env ref1) = do
  EnvRef n0 es0 <- readIORef ref0
  EnvRef n1 es1 <- readIORef ref1
  let n = n0 + n1
  if n <= sizeofSmallMutableArray es0
    then do
      copySmallMutableArray es0 n0 es1 0 n1
      writeIORef ref0 $! EnvRef n es0
      pure $ Env ref0
    else do
      es <- newSmallArray n (error "undefined field")
      copySmallMutableArray es 0  es0 0 n0
      copySmallMutableArray es n0 es1 0 n1
      writeIORef ref0 $! EnvRef n es
      pure $ Env ref0
{-# NOINLINE unsafeAppendEnv #-}

-- | Trim the environment to the given size in place.
unsafeTrimEnv :: HasCallStack => Int -> Env es -> IO (Env es0)
unsafeTrimEnv k (Env ref) = do
  EnvRef n es <- readIORef ref
  if k > n
    then error $ "k (" ++ show k ++ ") > n (" ++ show n ++ ")"
    else do
      overwrite es k (n - k)
      writeIORef ref $! EnvRef k es
      pure $ Env ref
  where
    overwrite es base = \case
      0 -> pure ()
      i -> do
        writeSmallArray es (base + i - 1) (error "undefined field")
        overwrite es base (i - 1)
{-# NOINLINE unsafeTrimEnv #-}

----------------------------------------
-- Data retrieval and update

-- | Replace the data type in the environment with a new value in place.
unsafePutEnv
  :: forall e es. (HasCallStack, e :> es)
  => e
  -> Env es
  -> IO ()
unsafePutEnv e (Env ref) = do
  EnvRef n es <- readIORef ref
  e `seq` writeSmallArray es (ixEnv @e @es n) (toAny e)

-- | Modify the data type in the environment in place.
unsafeModifyEnv
  :: forall e es. (HasCallStack, e :> es)
  => (e -> e)
  -> Env es
  -> IO ()
unsafeModifyEnv f (Env ref) = do
  EnvRef n es <- readIORef ref
  let i = ixEnv @e @es n
  e <- f . fromAny <$> readSmallArray es i
  e `seq` writeSmallArray es i (toAny e)

-- | Modify the data type in the enviroment in place and return a value.
unsafeStateEnv
  :: forall e es a. (HasCallStack, e :> es)
  => (e -> (a, e))
  -> Env es -> IO a
unsafeStateEnv f (Env ref) = do
  EnvRef n es <- readIORef ref
  let i = ixEnv @e @es n
  (a, e) <- f . fromAny <$> readSmallArray es i
  e `seq` writeSmallArray es i (toAny e)
  pure a

----------------------------------------
-- Internal helpers

toAny :: a -> Any
toAny = unsafeCoerce

fromAny :: Any -> a
fromAny = unsafeCoerce
