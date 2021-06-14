{-# OPTIONS_HADDOCK not-home #-}
-- | The enviroment for 'Effective.Internal.Monad.Eff'.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Effectful.Internal.Env
  ( Env

  -- * Safe operations
  , emptyEnv
  , cloneEnv
  , sizeEnv
  , getEnv
  , checkSizeEnv

  -- * Extending and shrinking
  , unsafeConsEnv
  , unsafeTailEnv

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

import Effectful.Internal.Has

type role Env nominal

-- | A mutable, strict (elements are kept in weak head normal form), extensible
-- record indexed by effect data types.
--
-- Offers pretty much perfect performance characteristics:
--
-- - Extending: /O(1)/ (amortized).
--
-- - Shrinking: /O(1)/.
--
-- - Indexing via '(:>)': /O(1)/ (amortized).
--
-- - Modification of a specific element: /O(1)/.
--
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

-- | Extend the environment with a new data type (in place).
unsafeConsEnv :: HasCallStack => e -> Env es -> IO (Env (e : es))
unsafeConsEnv e (Env ref) = do
  EnvRef n es0 <- readIORef ref
  let len0 = sizeofSmallMutableArray es0
  case n `compare` len0 of
    GT -> error $ "n (" ++ show n ++ ") > len0 (" ++ show len0 ++ ")"
    LT -> do
      e `seq` writeSmallArray es0 n (toAny e)
      writeIORef ref $! EnvRef (n + 1) es0
    EQ -> do
      let len = doubleCapacity len0
      es <- newSmallArray len (error "undefined field")
      copySmallMutableArray es 0 es0 0 len0
      e `seq` writeSmallArray es n (toAny e)
      writeIORef ref $! EnvRef (n + 1) es
  pure $ Env ref
  where
    doubleCapacity :: Int -> Int
    doubleCapacity n = max 1 n * 2
{-# NOINLINE unsafeConsEnv #-}

-- | Shrink the environment by one data type (in place). Makes sure the size of
-- the environment is as expected.
unsafeTailEnv :: HasCallStack => Int -> Env (e : es) -> IO (Env es)
unsafeTailEnv k (Env ref) = do
  EnvRef n es <- readIORef ref
  if k /= n - 1
    then error $ "k (" ++ show k ++ ") /= n - 1 (" ++ show (n - 1) ++ ")"
    else do
      writeSmallArray es k (error "undefined field")
      writeIORef ref $! EnvRef k es
      pure $ Env ref
{-# NOINLINE unsafeTailEnv #-}

----------------------------------------
-- Data retrieval and update

-- | Replace the data type in the environment with a new value (in place).
unsafePutEnv
  :: forall e es. (HasCallStack, e :> es)
  => e
  -> Env es
  -> IO ()
unsafePutEnv e (Env ref) = do
  EnvRef n es <- readIORef ref
  e `seq` writeSmallArray es (ixEnv @e @es n) (toAny e)

-- | Modify the data type in the environment (in place).
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

-- | Modify the data type in the enviroment (in place) and return a value.
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
