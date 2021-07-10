{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | The environment for the 'Effectful.Internal.Monad.Eff' monad.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Effectful.Internal.Env
  ( Env
  , Relinker(..)
  , noRelinker

  -- * Safe operations
  , emptyEnv
  , cloneEnv
  , forkEnv
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
import Unsafe.Coerce
import qualified Data.IntMap.Strict as IM

import Effectful.Internal.Effect

type role Env nominal

-- | A strict (WHNF), thread local, mutable, extensible record indexed by types
-- of kind 'Effect'.
--
-- Supports forking, i.e. introduction of local branches for encapsulation of
-- data specific to effect interpreters.
--
-- Offers very good performance characteristics:
--
-- - Extending: /O(1)/ (amortized).
--
-- - Shrinking: /O(1)/.
--
-- - Indexing via '(:>)': /O(forks)/, usually /O(1)/ (amortized).
--
-- - Modification of a specific element: /O(1)/.
--
-- Here's an example of how the environment might look:
--
-- @
-- e00 - e01 - e05 - e07 (*)
--        |     |     |
--        |    e06   e08 - e09
--        |
--       e02 - e03
--              |
--             e04
-- @
--
-- The point of execution is currently at (*), i.e. the mainline. Moreover,
-- currently:
--
-- - Mainline sees @[e00, e01, e05, e07]@.
--
-- - @e01@ is an interpreted effect, its handler sees @[e00, e02, e03]@.
--
-- - @e03@ is an interpreted effect, its handler sees @[e00, e02, e04]@.
--
-- - @e05@ is an interpreted effect, its handler sees @[e00, e01, e06]@.
--
-- - @e07@ is an interpreted effect, its handler sees @[e00, e01, e05, e08,
--   e09]@.
--
-- If an operation from @e01@ is invoked, the environment in the middle of
-- handling it might look like this:
--
-- @
-- e00 - e01 - e05 - e07
--        |     |     |
--        |    e06   e08 - e09
--        |
--       e02 - e03 - e10 (*)
--              |     |
--             e04   e11
--                    |
--                   e12
-- @
--
-- The point of execution is at (*), i.e. inside the handler of @e01@. The
-- handler needed to introduce @e10@ (which introduced @e11@, which in turn
-- introduced @e12@), so its local environment was temporarily extended with
-- it. Moreover, currently:
--
-- - Handler of @e01@ sees @[e00, e02, e03, e10]@.
--
-- - Handler of @e10@ sees @[e00, e02, e03, e11]@.
--
-- - Handler of @e11@ sees @[e00, e02, e03, e12]@.
--
data Env (es :: [Effect]) = Env
  { _forks     :: Forks
  , _globalRef :: IORef EnvRef
  , _forkIdGen :: ForkIdGen
  }

-- | Local forks of the environment.
data Forks = Forks ForkId Int (IORef EnvRef) Forks | NoFork

data EnvRef = EnvRef
  { _size      :: Int
  , _data      :: SmallMutableArray RealWorld Any
  , _relinkers :: SmallMutableArray RealWorld Any
  }

----------------------------------------
-- ForkId

newtype ForkId = ForkId { unForkId :: Int }

-- | 'ForkId' generation.
newtype ForkIdGen = ForkIdGen (IORef ForkId)

-- | Create a new thread local 'ForkId' generator.
newForkIdGen :: IO ForkIdGen
newForkIdGen = ForkIdGen <$> newIORef (ForkId 0)

-- | Clone the 'ForkId' generator for use in a different thread.
cloneForkIdGen :: ForkIdGen -> IO ForkIdGen
cloneForkIdGen (ForkIdGen ref) = fmap ForkIdGen . newIORef =<< readIORef ref

-- | Get a unique 'ForkId' from the generator.
newForkId :: ForkIdGen -> IO ForkId
newForkId (ForkIdGen ref) = do
  fid@(ForkId n) <- readIORef ref
  writeIORef ref $! ForkId (n + 1)
  pure fid

----------------------------------------
-- Relinker

newtype Relinker i (e :: Effect) where
  Relinker
    :: ((forall es. Env es -> IO (Env es)) -> i e -> IO (i e))
    -> Relinker i e

noRelinker :: Relinker i e
noRelinker = Relinker $ \_ -> pure

----------------------------------------
-- Operations

-- | Create an empty environment.
emptyEnv :: IO (Env '[])
emptyEnv = Env NoFork <$> emptyEnvRef <*> newForkIdGen

-- | Clone the environment.
cloneEnv :: Env es -> IO (Env es)
cloneEnv (Env NoFork gref0 gen0) = do
  EnvRef n es0 fs0 <- readIORef gref0
  es  <- cloneSmallMutableArray es0 0 (sizeofSmallMutableArray es0)
  fs  <- cloneSmallMutableArray fs0 0 (sizeofSmallMutableArray fs0)
  gen <- cloneForkIdGen gen0
  gref <- newIORef $ EnvRef n es fs
  cache <- newIORef IM.empty
  relinkData gref gen cache es fs n
  pure $ Env NoFork gref gen
cloneEnv (Env forks@(Forks _ baseIx lref0 _) gref0 gen0) = do
  EnvRef _ es0 fs0 <- readIORef gref0
  EnvRef n _   _   <- readIORef lref0
  let len = baseIx + n
  es  <- newSmallArray len (error "undefined field")
  fs  <- newSmallArray len (error "undefined field")
  baseN <- copyForks es fs len forks
  copySmallMutableArray es 0 es0 0 baseN
  copySmallMutableArray fs 0 fs0 0 baseN
  gen <- cloneForkIdGen gen0
  gref <- newIORef $ EnvRef len es fs
  cache <- newIORef IM.empty
  relinkData gref gen cache es fs n
  -- The forked environment is flattened and becomes the global one.
  pure $ Env NoFork gref gen
{-# NOINLINE cloneEnv #-}

----------------------------------------
-- Utils for cloning

-- | Let's say that the forked environment looks like this:
--
-- [0,1][2,3,4,5][6,7,8,9][10,11]
--
-- Then forks will look like this:
--
-- Fork (baseIx: 10, ...) [10,11]
--   (Fork (baseIx: 6, ...) [6,7,8,9,..]
--     (Fork (baseIx: 2, ...) [2,3,4,5,..]
--       NoFork))
--
-- and elements [0,1] are taken from the global environment ([0,1,..]).
--
-- We start with len: 12, we subtract baseIx: 10 and get n: 2, the number of
-- elements to copy from the fork. We copy them, then call recursively with
-- baseIx (10).
--
-- Then len: 10, we subtract baseIx: 6 and get n: 4. the number of elements to
-- copy from the fork (note that we can't use the capacity from EnvRef, because
-- it might be longer than 4 if the fork was locally extended). We copy them,
-- then call recursively with baseIx (6).
--
-- Then len: 6, we subtract baseIx: 2 and get n: 4. We copy the elements, then
-- call recursively with baseIx (2).
--
-- Now len: 2, but there are no more forks left, so we return len (2), the
-- number of elements to copy from the global environment.
copyForks
  :: SmallMutableArray RealWorld Any
  -> SmallMutableArray RealWorld Any
  -> Int
  -> Forks
  -> IO Int
copyForks es fs len = \case
  NoFork -> pure len
  Forks _ baseIx lref0 forks -> do
    EnvRef _ es0 fs0 <- readIORef lref0
    let n = len - baseIx
    copySmallMutableArray es baseIx es0 0 n
    copySmallMutableArray fs baseIx fs0 0 n
    copyForks es fs baseIx forks

type EnvRefCache = IORef (IM.IntMap (IORef EnvRef))

-- | Relink local environments hiding in the interpreters.
relinkData
  :: IORef EnvRef
  -> ForkIdGen
  -> EnvRefCache
  -> SmallMutableArray RealWorld Any
  -> SmallMutableArray RealWorld Any
  -> Int
  -> IO ()
relinkData gref gen cache es fs = \case
  0 -> pure ()
  n -> do
    let i = n - 1
    Relinker f <- fromAny <$> readSmallArray fs i
    readSmallArray es i
      >>= f relinkEnv . fromAny
      >>= writeSmallArray es i . toAny
    relinkData gref gen cache es fs i
  where
    relinkEnv :: Env es -> IO (Env es)
    relinkEnv (Env forks _ _) = Env <$> relinkForks gref gen cache forks
                                    <*> pure gref
                                    <*> pure gen

relinkForks :: IORef EnvRef -> ForkIdGen -> EnvRefCache -> Forks -> IO Forks
relinkForks gref gen cache = \case
  NoFork -> pure NoFork
  Forks fid baseIx lref0 forks -> do
    -- A specific IORef EnvRef can be held by more than one local environment
    -- and we need to replace all its occurences with the same, new value
    -- containing its clone.
    readIORef cache >>= pure . IM.lookup (unForkId fid) >>= \case
      Just lref -> Forks fid baseIx <$> pure lref
                                    <*> relinkForks gref gen cache forks
      Nothing   -> Forks fid baseIx <$> cloneEnvRef gref gen cache fid lref0
                                    <*> relinkForks gref gen cache forks

-- | Clone the local 'EnvRef' and put it in a cache.
cloneEnvRef
  :: IORef EnvRef
  -> ForkIdGen
  -> EnvRefCache
  -> ForkId
  -> IORef EnvRef
  -> IO (IORef EnvRef)
cloneEnvRef gref gen cache fid lref0 = do
  EnvRef n es0 fs0 <- readIORef lref0
  es  <- cloneSmallMutableArray es0 0 (sizeofSmallMutableArray es0)
  fs  <- cloneSmallMutableArray fs0 0 (sizeofSmallMutableArray fs0)
  ref <- newIORef $ EnvRef n es fs
  modifyIORef' cache $ IM.insert (unForkId fid) ref
  relinkData gref gen cache es fs n
  pure ref

----------------------------------------

-- | Create a local fork of the environment for interpreters.
forkEnv :: Env es -> IO (Env es)
forkEnv env@(Env NoFork gref gen) = do
  size <- sizeEnv env
  fid <- newForkId gen
  Env <$> (Forks fid size <$> emptyEnvRef <*> pure NoFork)
      <*> pure gref <*> pure gen
forkEnv (Env forks@(Forks _ baseIx lref0 olderForks) gref gen) = do
  fid <- newForkId gen
  lref <- emptyEnvRef
  (_size <$> readIORef lref0) >>= \case
    -- If the fork is empty, replace it as no data is lost.
    0 -> pure $ Env (Forks fid baseIx lref olderForks) gref gen
    _ -> pure $ Env (Forks fid baseIx lref forks)      gref gen
{-# NOINLINE forkEnv #-}

-- | Get the current size of the environment.
sizeEnv :: Env es -> IO Int
sizeEnv (Env NoFork ref _) = do
  EnvRef n _ _ <- readIORef ref
  pure n
sizeEnv (Env (Forks _ baseIx lref _) _ _) = do
  EnvRef n _ _ <- readIORef lref
  pure $ baseIx + n
{-# NOINLINE sizeEnv #-}

-- | Extract a specific data type from the environment.
getEnv :: forall e es i. e :> es => Env es -> IO (i e)
getEnv env = do
  (i, es) <- getLocation @e env
  fromAny <$> readSmallArray es i

-- | Check that the size of the environment is the same as the expected value.
checkSizeEnv :: Int -> Env es -> IO ()
checkSizeEnv k (Env NoFork ref _) = do
  EnvRef n _ _ <- readIORef ref
  when (k /= n) $ do
    error $ "k (" ++ show k ++ ") /= n (" ++ show n ++ ")"
checkSizeEnv k (Env (Forks _ baseIx lref _) _ _) = do
  EnvRef n _ _ <- readIORef lref
  when (k /= baseIx + n) $ do
    error $ "k (" ++ show k ++ ") /= baseIx + n (baseIx: "
         ++ show baseIx ++ ", n: " ++ show n ++ ")"
{-# NOINLINE checkSizeEnv #-}

----------------------------------------
-- Extending and shrinking

-- | Extend the environment with a new data type (in place).
unsafeConsEnv :: i e -> Relinker i e -> Env es -> IO (Env (e : es))
unsafeConsEnv e f (Env fork gref gen) = case fork of
  NoFork -> do
    extendEnvRef gref
    pure $ Env NoFork gref gen
  Forks _ _ lref _ -> do
    extendEnvRef lref
    pure $ Env fork gref gen
  where
    extendEnvRef :: IORef EnvRef -> IO ()
    extendEnvRef ref = do
      EnvRef n es0 fs0 <- readIORef ref
      let len0 = sizeofSmallMutableArray es0
      case n `compare` len0 of
        GT -> error $ "n (" ++ show n ++ ") > len0 (" ++ show len0 ++ ")"
        LT -> do
          e `seq` writeSmallArray es0 n (toAny e)
          f `seq` writeSmallArray fs0 n (toAny f)
          writeIORef ref $! EnvRef (n + 1) es0 fs0
        EQ -> do
          let len = doubleCapacity len0
          es <- newSmallArray len (error "undefined field")
          copySmallMutableArray es 0 es0 0 len0
          e `seq` writeSmallArray es n (toAny e)
          fs <- newSmallArray len (error "undefined field")
          copySmallMutableArray fs 0 fs0 0 len0
          f `seq` writeSmallArray fs n (toAny f)
          writeIORef ref $! EnvRef (n + 1) es fs

    doubleCapacity :: Int -> Int
    doubleCapacity n = max 1 n * 2
{-# NOINLINE unsafeConsEnv #-}

-- | Shrink the environment by one data type (in place). Makes sure the size of
-- the environment is as expected.
unsafeTailEnv :: Int -> Env (e : es) -> IO (Env es)
unsafeTailEnv len (Env fork gref gen) = case fork of
  NoFork -> do
    shrinkEnvRef len gref
    pure $ Env NoFork gref gen
  Forks _ baseIx lref _ -> do
    shrinkEnvRef (len - baseIx) lref
    pure $ Env fork gref gen
  where
    shrinkEnvRef :: Int -> IORef EnvRef -> IO ()
    shrinkEnvRef k ref = do
      EnvRef n es fs <- readIORef ref
      if k /= n - 1
        then error $ "k (" ++ show k ++ ") /= n - 1 (" ++ show (n - 1) ++ ")"
        else do
          writeSmallArray es k (error "undefined field")
          writeSmallArray fs k (error "undefined field")
          writeIORef ref $! EnvRef k es fs
{-# NOINLINE unsafeTailEnv #-}

----------------------------------------
-- Data retrieval and update

-- | Replace the data type in the environment with a new value (in place).
unsafePutEnv :: forall e es i. e :> es => i e -> Env es -> IO ()
unsafePutEnv e env = do
  (i, es) <- getLocation @e env
  e `seq` writeSmallArray es i (toAny e)

-- | Modify the data type in the environment (in place).
unsafeModifyEnv :: forall e es i. e :> es => (i e -> i e) -> Env es -> IO ()
unsafeModifyEnv f env = do
  (i, es) <- getLocation @e env
  e <- f . fromAny <$> readSmallArray es i
  e `seq` writeSmallArray es i (toAny e)

-- | Modify the data type in the environment (in place) and return a value.
unsafeStateEnv :: forall e es i a. e :> es => (i e -> (a, i e)) -> Env es -> IO a
unsafeStateEnv f env = do
  (i, es) <- getLocation @e env
  (a, e) <- f . fromAny <$> readSmallArray es i
  e `seq` writeSmallArray es i (toAny e)
  pure a

----------------------------------------
-- Internal helpers

emptyEnvRef :: IO (IORef EnvRef)
emptyEnvRef = do
  es <- newSmallArray 0 (error "undefined field")
  fs <- newSmallArray 0 (error "undefined field")
  newIORef $ EnvRef 0 es fs

-- | Determine location of the data type in the environment.
getLocation
  :: forall e es. e :> es
  => Env es
  -> IO (Int, SmallMutableArray RealWorld Any)
getLocation (Env fork ref _) = do
  EnvRef n es _ <- readIORef ref
  -- Optimized for the common access pattern:
  --
  -- - Most application code has no access to forks, in which case we look at
  --   the global EnvRef.
  --
  -- - Interpreters will most likely access the newest fork for reinterpretation
  --   of effects that are there.
  case fork of
    NoFork -> pure (ix n, es)
    Forks _ baseIx lref forks -> do
      EnvRef ln les _ <- readIORef lref
      let i = ix (baseIx + ln)
      if i >= baseIx
        then pure (i - baseIx, les)
        else go es i forks
  where
    go :: SmallMutableArray RealWorld Any
       -> Int
       -> Forks
       -> IO (Int, SmallMutableArray RealWorld Any)
    go es i = \case
      NoFork -> pure (i, es)
      Forks _ baseIx lref forks -> do
        EnvRef _ les _ <- readIORef lref
        if i >= baseIx
          then pure (i - baseIx, les)
          else go es i forks

    ix :: Int -> Int
    ix n = n - reifyIndex @e @es - 1

toAny :: a -> Any
toAny = unsafeCoerce

fromAny :: Any -> a
fromAny = unsafeCoerce
