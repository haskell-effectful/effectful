{-# LANGUAGE AllowAmbiguousTypes #-}
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

    -- ** Modification of the effect stack
  , consEnv
  , unconsEnv
  , replaceEnv
  , unreplaceEnv
  , subsumeEnv
  , unsubsumeEnv
  , injectEnv

    -- ** Data retrieval and update
  , getEnv
  , putEnv
  , stateEnv
  , modifyEnv
  ) where

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
  let relinkEffects = \case
        0 -> pure ()
        k -> do
          let i = k - 1
          Relinker f <- fromAny <$> readSmallArray fs i
          readSmallArray es i
            >>= f (relinkEnv storage) . fromAny
            >>= writeSmallArray es i . toAny
          relinkEffects i
  relinkEffects storageSize
  pure $ Env size mrefs storage
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
  deleteEffect storage ref
  writeIORef mrefs $! References (size - 1) refs
{-# NOINLINE unconsEnv #-}

----------------------------------------

-- | Replace a specific effect in the stack with a new value.
replaceEnv
  :: forall e es. e :> es
  => EffectRep (DispatchOf e) e
  -- ^ The representation of the effect.
  -> Relinker (EffectRep (DispatchOf e)) e
  -> Env es
  -> IO (Env es)
replaceEnv e f (Env size mrefs0 storage) = do
  References n refs0 <- readIORef mrefs0
  errorWhenDifferent size n
  len0 <- getSizeofMutablePrimArray refs0
  when (size > len0) $ do
    error $ "size (" ++ show size ++ ") > len0 (" ++ show len0 ++ ")"
  refs <- cloneMutablePrimArray refs0 0 len0
  ref <- insertEffect storage e f
  writePrimArray refs (refIndex @e @es size) ref
  mrefs <- newIORef $ References n refs
  pure $ Env size mrefs storage
{-# NOINLINE replaceEnv #-}

-- | Remove a reference to the replaced effect.
--
-- /Note:/ after calling this function the input environment is no longer
-- usable.
unreplaceEnv :: forall e es. e :> es => Env es -> IO ()
unreplaceEnv (Env size mrefs storage) = do
  References n refs <- readIORef mrefs
  errorWhenDifferent size n
  ref <- readPrimArray refs (refIndex @e @es size)
  deleteEffect storage ref
{-# NOINLINE unreplaceEnv #-}

----------------------------------------

-- | Reference an existing effect from the top of the stack (in place).
subsumeEnv :: forall e es. e :> es => Env es -> IO (Env (e : es))
subsumeEnv (Env size mrefs storage) = do
  References n refs0 <- readIORef mrefs
  errorWhenDifferent size n
  len0 <- getSizeofMutablePrimArray refs0
  refs <- case size `compare` len0 of
    GT -> error $ "size (" ++ show size ++ ") > len0 (" ++ show len0 ++ ")"
    LT -> pure refs0
    EQ -> resizeMutablePrimArray refs0 (doubleCapacity len0)
  ref <- readPrimArray refs (refIndex @e @es size)
  writePrimArray refs size ref
  writeIORef mrefs $! References (size + 1) refs
  pure $ Env (size + 1) mrefs storage
{-# NOINLINE subsumeEnv #-}

-- | Remove a reference to an existing effect from the top of the stack.
--
-- /Note:/ after calling this function the input environment is no longer
-- usable.
unsubsumeEnv :: e :> es => Env (e : es) -> IO ()
unsubsumeEnv (Env size mrefs _) = do
  References n refs <- readIORef mrefs
  errorWhenDifferent size n
  writeIORef mrefs $! References (size - 1) refs
{-# NOINLINE unsubsumeEnv #-}

----------------------------------------

-- | Construct an environment containing a permutation (with possible
-- duplicates) of a subset of effects from the input environment.
injectEnv :: forall xs es. Subset xs es => Env es -> IO (Env xs)
injectEnv (Env size0 mrefs0 storage) = do
  References n0 refs0 <- readIORef mrefs0
  errorWhenDifferent size0 n0
  let makeRefs k acc = \case
        []       -> unsafeThawPrimArray $ primArrayFromListN k acc
        (e : es) -> do
          i <- readPrimArray refs0 (n0 - e - 1) -- refIndex
          makeRefs (k + 1) (i : acc) es
  refs <- makeRefs 0 [] (reifyIndices @xs @es)
  size <- getSizeofMutablePrimArray refs
  mrefs <- newIORef $ References size refs
  pure $ Env size mrefs storage
{-# NOINLINE injectEnv #-}

----------------------------------------
-- Data retrieval and update

-- | Extract a specific data type from the environment.
getEnv
  :: forall e es. e :> es
  => Env es -- ^ The environment.
  -> IO (EffectRep (DispatchOf e) e)
getEnv env = do
  (i, es) <- getLocation @e env
  fromAny <$> readSmallArray es i

-- | Replace the data type in the environment with a new value (in place).
putEnv
  :: forall e es. e :> es
  => Env es -- ^ The environment.
  -> EffectRep (DispatchOf e) e
  -> IO ()
putEnv env e = do
  (i, es) <- getLocation @e env
  e `seq` writeSmallArray es i (toAny e)

-- | Modify the data type in the environment (in place) and return a value.
stateEnv
  :: forall e es a. e :> es
  => Env es -- ^ The environment.
  -> (EffectRep (DispatchOf e) e -> (a, EffectRep (DispatchOf e) e))
  -> IO a
stateEnv env f = do
  (i, es) <- getLocation @e env
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
  (i, es) <- getLocation @e env
  e <- f . fromAny <$> readSmallArray es i
  e `seq` writeSmallArray es i (toAny e)

-- | Determine location of the effect in the environment.
getLocation
  :: forall e es. e :> es
  => Env es
  -> IO (Int, SmallMutableArray RealWorld Any)
getLocation (Env size mrefs storage) = do
  refs <- refIndices <$> readIORef mrefs
  i <- readPrimArray refs (refIndex @e @es size)
  es <- stEffects <$> readIORef storage
  pure (i, es)

-- | Get the index of a reference to an effect.
refIndex :: forall e es. e :> es => Int -> Int
refIndex size = size - reifyIndex @e @es - 1

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
