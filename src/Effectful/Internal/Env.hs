{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | The environment for 'Effectful.Internal.Monad.Eff'.
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
import GHC.Stack
import Unsafe.Coerce
import qualified Data.Map.Strict as M

import Effectful.Internal.Effect

type role Env nominal

-- | A mutable, strict (elements are kept in weak head normal form), extensible
-- record indexed by effect data types.
--
-- Offers very good performance characteristics:
--
-- - Extending: /O(1)/ (amortized).
--
-- - Shrinking: /O(1)/.
--
-- - Indexing via '(:>)': /O(forks)/, usually /O(1)/ (amortized).
--
-- - Modification of a specific element: /O(forks)/, usually /O(1)/.
--
data Env (es :: [Effect]) = Env Forks (IORef EnvRef)

-- | A fork is uniquely determined by its base index and depth.
data ForkId = ForkId
  { baseIx :: Int
  , depth  :: Int
  } deriving (Eq, Ord, Show)

-- | Implicit 'ForkId' of the 'EnvRef' directly available from 'Env'.
globalFid :: ForkId
globalFid = ForkId 0 0

-- | Local forks of the environment.
data Forks = Forks {-# UNPACK #-} ForkId (IORef EnvRef) Forks | NoFork

data EnvRef = EnvRef
  { _capacity  :: Int
  , _data      :: SmallMutableArray RealWorld Any
  , _relinkers :: SmallMutableArray RealWorld Any
  }

newtype Relinker i (e :: Effect) where
  Relinker
    :: ((forall es. Env es -> IO (Env es)) -> i e -> IO (i e))
    -> Relinker i e

noRelinker :: Relinker i e
noRelinker = Relinker $ \_ -> pure

-- | Create an empty environment.
emptyEnv :: IO (Env '[])
emptyEnv = Env NoFork <$> emptyEnvRef

-- | Clone the environment.
cloneEnv :: HasCallStack => Env es -> IO (Env es)
cloneEnv (Env NoFork gref0) = do
  cache <- newIORef M.empty
  gref <- cloneEnvRef cache globalFid gref0
  pure $ Env NoFork gref
cloneEnv _ = error "Cloning of forked enviroment is not supported yet"
{-# NOINLINE cloneEnv #-}

----------------------------------------
-- Utils for cloning

type EnvRefCache = IORef (M.Map ForkId (IORef EnvRef))

-- | Clone the 'EnvRef' and put it in a cache.
cloneEnvRef
  :: EnvRefCache
  -> ForkId
  -> IORef EnvRef
  -> IO (IORef EnvRef)
cloneEnvRef cache fid ref0 = do
  EnvRef n es0 fs0 <- readIORef ref0
  es  <- cloneSmallMutableArray es0 0 (sizeofSmallMutableArray es0)
  fs  <- cloneSmallMutableArray fs0 0 (sizeofSmallMutableArray fs0)
  ref <- newIORef $ EnvRef n es fs
  modifyIORef' cache $ M.insert fid ref
  relinkData cache es fs n
  pure ref

-- | Relink local environments hiding in the interpreters.
relinkData
  :: EnvRefCache
  -> SmallMutableArray RealWorld Any
  -> SmallMutableArray RealWorld Any
  -> Int
  -> IO ()
relinkData cache es fs = \case
  0 -> pure ()
  n -> do
    let i = n - 1
    Relinker f <- fromAny <$> readSmallArray fs i
    readSmallArray es i
      >>= f relinkEnv . fromAny
      >>= writeSmallArray es i . toAny
    relinkData cache es fs i
  where
    relinkEnv :: Env es -> IO (Env es)
    relinkEnv (Env forks _) = do
      Just gref <- M.lookup globalFid <$> readIORef cache
      Env <$> relinkForks cache forks <*> pure gref

relinkForks :: EnvRefCache -> Forks -> IO Forks
relinkForks cache = \case
  NoFork -> pure NoFork
  Forks fid lref0 forks -> (M.lookup fid <$> readIORef cache) >>= \case
    Just lref -> Forks fid <$> pure lref
                           <*> relinkForks cache forks
    Nothing   -> Forks fid <$> cloneEnvRef cache fid lref0
                           <*> relinkForks cache forks

----------------------------------------

-- | Create a local fork of the environment for interpreters.
forkEnv :: HasCallStack => Env es -> IO (Env es)
forkEnv env@(Env NoFork gref) = do
  size <- sizeEnv env
  let fid = ForkId
        { baseIx = size
        , depth  = 1
        }
  Env <$> (Forks fid <$> emptyEnvRef <*> pure NoFork)
      <*> pure gref
forkEnv (Env forks@(Forks fid0 lref0 olderForks) gref) = do
  EnvRef n _ _ <- readIORef lref0
  -- If the fork is empty, replace it as no data is lost.
  if n == 0
    then do
      lref <- emptyEnvRef
      let fid = fid0
            { depth = depth fid0 + 1
            }
      pure $ Env (Forks fid lref olderForks) gref
    else do
      lref <- emptyEnvRef
      let fid = fid0
            { baseIx = baseIx fid0 + n
            , depth  = depth  fid0 + 1
            }
      pure $ Env (Forks fid lref forks) gref
{-# NOINLINE forkEnv #-}

-- | Get the current size of the environment.
sizeEnv :: Env es -> IO Int
sizeEnv (Env NoFork ref) = do
  EnvRef n _ _ <- readIORef ref
  pure n
sizeEnv (Env (Forks fid lref _) _) = do
  EnvRef n _ _ <- readIORef lref
  pure $ baseIx fid + n
{-# NOINLINE sizeEnv #-}

-- | Extract a specific data type from the environment.
getEnv :: forall e es i. (HasCallStack, e :> es) => Env es -> IO (i e)
getEnv env = do
  (i, es) <- getLocation @e env
  fromAny <$> readSmallArray es i

-- | Check that the size of the environment is the same as the expected value.
checkSizeEnv :: HasCallStack => Int -> Env es -> IO ()
checkSizeEnv k (Env NoFork ref) = do
  EnvRef n _ _ <- readIORef ref
  when (k /= n) $ do
    error $ "k (" ++ show k ++ ") /= n (" ++ show n ++ ")"
checkSizeEnv k (Env (Forks fid lref _) _) = do
  EnvRef n _ _ <- readIORef lref
  when (k /= baseIx fid + n) $ do
    error $ "k (" ++ show k ++ ") /= baseIx + n (baseIx: "
         ++ show (baseIx fid) ++ ", n: " ++ show n ++ ")"
{-# NOINLINE checkSizeEnv #-}

----------------------------------------
-- Extending and shrinking

-- | Extend the environment with a new data type (in place).
unsafeConsEnv :: HasCallStack => i e -> Relinker i e -> Env es -> IO (Env (e : es))
unsafeConsEnv e f (Env fork gref) = case fork of
  NoFork -> do
    extendEnvRef gref
    pure $ Env NoFork gref
  Forks base lref forks -> do
    extendEnvRef lref
    pure $ Env (Forks base lref forks) gref
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
unsafeTailEnv :: HasCallStack => Int -> Env (e : es) -> IO (Env es)
unsafeTailEnv len (Env fork gref) = case fork of
  NoFork -> do
    shrinkEnvRef len gref
    pure $ Env NoFork gref
  Forks fid lref forks -> do
    shrinkEnvRef (len - baseIx fid) lref
    pure $ Env (Forks fid lref forks) gref
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
unsafePutEnv
  :: forall e es i. (HasCallStack, e :> es)
  => i e
  -> Env es
  -> IO ()
unsafePutEnv e env = do
  (i, es) <- getLocation @e env
  e `seq` writeSmallArray es i (toAny e)

-- | Modify the data type in the environment (in place).
unsafeModifyEnv
  :: forall e es i. (HasCallStack, e :> es)
  => (i e -> i e)
  -> Env es
  -> IO ()
unsafeModifyEnv f env = do
  (i, es) <- getLocation @e env
  e <- f . fromAny <$> readSmallArray es i
  e `seq` writeSmallArray es i (toAny e)

-- | Modify the data type in the environment (in place) and return a value.
unsafeStateEnv
  :: forall e es i a. (HasCallStack, e :> es)
  => (i e -> (a, i e))
  -> Env es -> IO a
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
getLocation (Env fork ref) = do
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
    Forks fid lref forks -> do
      EnvRef ln les _ <- readIORef lref
      let i = ix (baseIx fid + ln)
      if i >= baseIx fid
        then pure (i - baseIx fid, les)
        else go es i forks
  where
    go :: SmallMutableArray RealWorld Any
       -> Int
       -> Forks
       -> IO (Int, SmallMutableArray RealWorld Any)
    go es i = \case
      NoFork -> pure (i, es)
      Forks fid lref forks -> do
        EnvRef _ les _ <- readIORef lref
        if i >= baseIx fid
          then pure (i - baseIx fid, les)
          else go es i forks

    ix :: Int -> Int
    ix n = n - reifyIndex @e @es - 1

toAny :: a -> Any
toAny = unsafeCoerce

fromAny :: Any -> a
fromAny = unsafeCoerce
