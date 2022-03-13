{-# OPTIONS_HADDOCK not-home #-}
module Effectful.Internal.Env
  ( -- * The environment
    Env(..)
  , References(..)
  , Storage(..)

    -- ** Relinker
  , Relinker(..)
  , dummyRelinker

    -- * Dispatch
  , Dispatch(..)
  , SideEffects(..)
  , DispatchOf
  , EffectRep

    -- * Operations
  , emptyEnv
  , cloneEnv
  , forkEnv
  , sizeEnv
  , checkSizeEnv
  , tailEnv

    -- ** Extending and shrinking
  , consEnv
  , unconsEnv

    -- ** Data retrieval and update
  , getEnv
  , putEnv
  , stateEnv
  , modifyEnv
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Data.IORef
import Data.Primitive.PrimArray
import Data.Primitive.SmallArray
import GHC.Stack (HasCallStack)

import Effectful.Internal.Effect
import Effectful.Internal.Utils

type role Env nominal

-- | A strict (WHNF), __thread local__, mutable, extensible record indexed by types
-- of kind 'Effect'.
--
-- Supports forking, i.e. introduction of local branches for encapsulation of
-- effects specific to effect handlers.
--
-- __Warning: the environment is a mutable data structure and cannot be simultaneously used from multiple threads under any circumstances.__
--
-- In order to pass it to a different thread, you need to perform a deep copy
-- with the 'cloneEnv' funtion.
--
-- Offers very good performance characteristics for most often performed
-- operations:
--
-- - Extending: /@O(1)@/ (amortized).
--
-- - Shrinking: /@O(1)@/.
--
-- - Indexing via '(:>)': /@O(1)@/
--
-- - Modification of a specific element: /@O(1)@/.
--
-- - Forking: /@O(n)@/, where @n@ is the size of the effect stack.
--
-- - Cloning: /@O(N + Σ(n_i))@/, where @N@ is the size of the 'Storage', while
--   @i@ ranges over handlers of dynamically dispatched effects in the 'Storage'
--   and @n_i@ is the size of the effect stack of @i@-th handler.
--
data Env (es :: [Effect]) = Env
  { envSize    :: !Int
  , envRefs    :: !(IORef References)
  , envStorage :: !(IORef Storage)
  }

-- | An array of references to effects in the 'Storage'.
data References = References
  { refSize    :: !Int
  , refIndices :: !(MutablePrimArray RealWorld Int)
  }

-- | A storage of effects.
--
-- Shared between all forks of the environment within the same thread.
data Storage = Storage
  { stSize      :: !Int
  , stEffects   :: !(SmallMutableArray RealWorld Any)
  , stRelinkers :: !(SmallMutableArray RealWorld Any)
  }

----------------------------------------
-- Relinker

-- | A function for relinking 'Env' objects stored in the handlers and/or making
-- a deep copy of the representation of the effect when cloning the environment.
newtype Relinker :: (Effect -> Type) -> Effect -> Type where
  Relinker
    :: ((forall es. Env es -> IO (Env es)) -> rep e -> IO (rep e))
    -> Relinker rep e

-- | A dummy 'Relinker'.
dummyRelinker :: Relinker rep e
dummyRelinker = Relinker $ \_ -> pure

----------------------------------------
-- Dispatch

-- | A type of dispatch. For more information consult the documentation in
-- "Effectful.Dispatch.Dynamic" and "Effectful.Dispatch.Static".
data Dispatch = Dynamic | Static SideEffects

-- | Signifies whether core operations of a statically dispatched effect perform
-- side effects. If an effect is marked as such, the
-- 'Effectful.Dispatch.Static.runStaticRep' family of functions will require the
-- 'Effectful.IOE' effect to be in context via the
-- 'Effectful.Dispatch.Static.MaybeIOE' type family.
data SideEffects = NoSideEffects | WithSideEffects

-- | Dispatch types of effects.
type family DispatchOf (e :: Effect) :: Dispatch

-- | Internal representations of effects.
type family EffectRep (d :: Dispatch) :: Effect -> Type

----------------------------------------
-- Operations

-- | Create an empty environment.
emptyEnv :: IO (Env '[])
emptyEnv = Env <$> pure 0
  <*> (newIORef . References 0 =<< newPrimArray 0)
  <*> (newIORef =<< emptyStorage)

-- | Clone the environment to use it in a different thread.
cloneEnv :: Env es -> IO (Env es)
cloneEnv (Env size mrefs0 storage0) = do
  References n refs0 <- readIORef mrefs0
  errorWhenDifferent size n
  mrefs <- newIORef . References n
    =<< cloneMutablePrimArray refs0 0 (sizeofMutablePrimArray refs0)
  Storage storageSize es0 fs0 <- readIORef storage0
  let esSize = sizeofSmallMutableArray es0
      fsSize = sizeofSmallMutableArray fs0
  when (esSize /= fsSize) $ do
    error $ "esSize (" ++ show esSize ++ ") /= fsSize (" ++ show fsSize ++ ")"
  es <- cloneSmallMutableArray es0 0 esSize
  fs <- cloneSmallMutableArray fs0 0 esSize
  storage <- newIORef $ Storage storageSize es fs
  relinkEffects (relinkEnv storage) es fs storageSize
  pure $ Env size mrefs storage
  where
    relinkEffects
      :: (forall es. Env es -> IO (Env es))
      -> SmallMutableArray RealWorld Any
      -> SmallMutableArray RealWorld Any
      -> Int
      -> IO ()
    relinkEffects relink es fs = \case
     0 -> pure ()
     n -> do
       let i = n - 1
       Relinker f <- fromAny <$> readSmallArray fs i
       readSmallArray es i
         >>= f relink . fromAny
         >>= writeSmallArray es i . toAny
       relinkEffects relink es fs i
{-# NOINLINE cloneEnv #-}

-- | Create a fork of the environment.
--
-- Forked environment can be updated independently of the original one within
-- the same thread.
forkEnv :: Env es -> IO (Env es)
forkEnv (Env size mrefs0 storage) = do
  References n refs0 <- readIORef mrefs0
  errorWhenDifferent size n
  mrefs <- newIORef . References size
    =<< cloneMutablePrimArray refs0 0 (sizeofMutablePrimArray refs0)
  pure $ Env size mrefs storage
{-# NOINLINE forkEnv #-}

-- | Check that the size of the environment is internally consistent.
checkSizeEnv :: Env es -> IO ()
checkSizeEnv (Env size mrefs _) = do
  References n _ <- readIORef mrefs
  errorWhenDifferent size n
{-# NOINLINE checkSizeEnv #-}

-- | Get the current size of the environment.
sizeEnv :: Env es -> IO Int
sizeEnv env = pure $ envSize env

-- | Access the tail of the environment.
tailEnv :: Env (e : es) -> IO (Env es)
tailEnv (Env size mrefs0 storage) = do
  References n refs0 <- readIORef mrefs0
  errorWhenDifferent size n
  mrefs <- newIORef . References (size - 1)
    =<< cloneMutablePrimArray refs0 0 (sizeofMutablePrimArray refs0)
  pure $ Env (size - 1) mrefs storage
{-# NOINLINE tailEnv #-}

----------------------------------------
-- Extending and shrinking

-- | Extend the environment with a new data type (in place).
consEnv
  :: EffectRep (DispatchOf e) e
  -- ^ The representation of the effect.
  -> Relinker (EffectRep (DispatchOf e)) e
  -> Env es
  -> IO (Env (e : es))
consEnv e f (Env size mrefs storage) = do
  References n refs0 <- readIORef mrefs
  errorWhenDifferent size n
  len0 <- getSizeofMutablePrimArray refs0
  refs <- case size `compare` len0 of
    GT -> error $ "size (" ++ show size ++ ") > len0 (" ++ show len0 ++ ")"
    LT -> pure refs0
    EQ -> resizeMutablePrimArray refs0 (doubleCapacity len0)
  mask_ $ do
    ref <- insertEffect storage e f
    writePrimArray refs size ref
    writeIORef mrefs $! References (size + 1) refs
    pure $ Env (size + 1) mrefs storage
{-# NOINLINE consEnv #-}

-- | Shrink the environment by one data type (in place).
--
-- /Note:/ after calling this function the input environment is no longer
-- usable.
unconsEnv :: Env (e : es) -> IO ()
unconsEnv (Env size mrefs storage) = do
  References n refs <- readIORef mrefs
  errorWhenDifferent size n
  ref <- readPrimArray refs (size - 1)
  mask_ $ do
    deleteEffect storage ref
    writeIORef mrefs $! References (size - 1) refs
{-# NOINLINE unconsEnv #-}

----------------------------------------
-- Data retrieval and update

-- | Extract a specific data type from the environment.
getEnv
  :: forall e es. e :> es
  => Env es -- ^ The environment.
  -> IO (EffectRep (DispatchOf e) e)
getEnv env = do
  (i, es) <- getLocation (reifyIndex @e @es) env
  fromAny <$> readSmallArray es i

-- | Replace the data type in the environment with a new value (in place).
putEnv
  :: forall e es. e :> es
  => Env es -- ^ The environment.
  -> EffectRep (DispatchOf e) e
  -> IO ()
putEnv env e = do
  (i, es) <- getLocation (reifyIndex @e @es) env
  e `seq` writeSmallArray es i (toAny e)

-- | Modify the data type in the environment (in place) and return a value.
stateEnv
  :: forall e es a. e :> es
  => Env es -- ^ The environment.
  -> (EffectRep (DispatchOf e) e -> (a, EffectRep (DispatchOf e) e))
  -> IO a
stateEnv env f = do
  (i, es) <- getLocation (reifyIndex @e @es) env
  (a, e) <- f . fromAny <$> readSmallArray es i
  e `seq` writeSmallArray es i (toAny e)
  pure a

-- | Modify the data type in the environment (in place).
modifyEnv
  :: forall e es. e :> es
  => Env es -- ^ The environment.
  -> (EffectRep (DispatchOf e) e -> EffectRep (DispatchOf e) e)
  -> IO ()
modifyEnv env f = do
  (i, es) <- getLocation (reifyIndex @e @es) env
  e <- f . fromAny <$> readSmallArray es i
  e `seq` writeSmallArray es i (toAny e)

-- | Determine location of the effect in the environment.
getLocation
  :: Int
  -> Env es
  -> IO (Int, SmallMutableArray RealWorld Any)
getLocation ix (Env size mrefs storage) = do
  refs <- refIndices <$> readIORef mrefs
  i <- readPrimArray refs (size - ix - 1)
  es <- stEffects <$> readIORef storage
  pure (i, es)

----------------------------------------
-- Internal helpers

-- | Create an empty storage.
emptyStorage :: IO Storage
emptyStorage = Storage
  <$> pure 0
  <*> newSmallArray 0 undefinedData
  <*> newSmallArray 0 undefinedData

-- | Insert an effect into the storage and return its reference.
insertEffect
  :: IORef Storage
  -> EffectRep (DispatchOf e) e
  -- ^ The representation of the effect.
  -> Relinker (EffectRep (DispatchOf e)) e
  -> IO Int
insertEffect storage e f = do
  Storage size es0 fs0 <- readIORef storage
  let len0 = sizeofSmallMutableArray es0
  case size `compare` len0 of
    GT -> error $ "size (" ++ show size ++ ") > len0 (" ++ show len0 ++ ")"
    LT -> do
      e `seq` writeSmallArray es0 size (toAny e)
      f `seq` writeSmallArray fs0 size (toAny f)
      writeIORef storage $! Storage (size + 1) es0 fs0
      pure size
    EQ -> do
      let len = doubleCapacity len0
      es <- newSmallArray len undefinedData
      fs <- newSmallArray len undefinedData
      copySmallMutableArray es 0 es0 0 size
      copySmallMutableArray fs 0 fs0 0 size
      e `seq` writeSmallArray es size (toAny e)
      f `seq` writeSmallArray fs size (toAny f)
      writeIORef storage $! Storage (size + 1) es fs
      pure size

-- | Given a reference to an effect, delete it from the storage.
--
-- /Note:/ the reference needs to point to the end of the storage. Normally it's
-- not a problem as it turns out effects are put and taken from the storage in
-- the same order across all forks, unless someone tries to do something
-- unexpected.
deleteEffect :: IORef Storage -> Int -> IO ()
deleteEffect storage ref = do
  Storage size es fs <- readIORef storage
  when (ref /= size - 1) $ do
    error $ "ref (" ++ show ref ++ ") /= size - 1 (" ++ show (size - 1) ++ ")"
  writeSmallArray es ref undefinedData
  writeSmallArray fs ref undefinedData
  writeIORef storage $! Storage (size - 1) es fs

-- | Relink the environment to use the new storage.
relinkEnv :: IORef Storage -> Env es -> IO (Env es)
relinkEnv storage (Env size mrefs0 _) = do
  References n refs0 <- readIORef mrefs0
  mrefs <- newIORef . References n
    =<< cloneMutablePrimArray refs0 0 (sizeofMutablePrimArray refs0)
  pure $ Env size mrefs storage

-- | Throw an error if array sizes do not agree.
errorWhenDifferent :: HasCallStack => Int -> Int -> IO ()
errorWhenDifferent size n
  | size /= n = error $ "size (" ++ show size ++ ") /= n (" ++ show n ++ ")"
  | otherwise = pure ()

-- | Double the capacity of an array.
doubleCapacity :: Int -> Int
doubleCapacity n = max 1 n * 2

undefinedData :: HasCallStack => a
undefinedData = error "undefined data"
