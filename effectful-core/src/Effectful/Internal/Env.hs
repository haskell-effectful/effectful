{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK not-home #-}
module Effectful.Internal.Env
  ( -- * The environment
    Env(..)
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
  , sizeEnv
  , tailEnv

    -- ** Modification of the effect stack
  , consEnv
  , unconsEnv
  , replaceEnv
  , unreplaceEnv
  , subsumeEnv
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
-- __Warning: the environment is a mutable data structure and cannot be simultaneously used from multiple threads under any circumstances.__
--
-- In order to pass it to a different thread, you need to perform a deep copy
-- with the 'cloneEnv' funtion.
--
-- Offers very good performance characteristics for most often performed
-- operations:
--
-- - Extending: /@O(n)@/, where @n@ is the size of the effect stack.
--
-- - Shrinking: /@O(1)@/.
--
-- - Indexing via '(:>)': /@O(1)@/
--
-- - Modification of a specific element: /@O(1)@/.
--
-- - Getting a tail: /@O(1)@/.
--
-- - Cloning: /@O(N)@/, where @N@ is the size of the 'Storage'.
--
data Env (es :: [Effect]) = Env
  { envOffset  :: !Int
  , envRefs    :: !(PrimArray Int)
  , envStorage :: !(IORef Storage)
  }

-- | A storage of effects.
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
emptyEnv = Env 0
  <$> (unsafeFreezePrimArray =<< newPrimArray 0)
  <*> (newIORef =<< emptyStorage)

-- | Clone the environment to use it in a different thread.
cloneEnv :: Env es -> IO (Env es)
cloneEnv (Env offset refs storage0) = do
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
  pure $ Env offset refs storage
{-# NOINLINE cloneEnv #-}

-- | Get the current size of the environment.
sizeEnv :: Env es -> IO Int
sizeEnv (Env offset refs _) = do
  pure $ sizeofPrimArray refs - offset

-- | Access the tail of the environment.
tailEnv :: Env (e : es) -> IO (Env es)
tailEnv (Env offset refs storage) = do
  pure $ Env (offset + 1) refs storage

----------------------------------------
-- Extending and shrinking

-- | Extend the environment with a new data type.
consEnv
  :: EffectRep (DispatchOf e) e
  -- ^ The representation of the effect.
  -> Relinker (EffectRep (DispatchOf e)) e
  -> Env es
  -> IO (Env (e : es))
consEnv e f (Env offset refs0 storage) = do
  let size = sizeofPrimArray refs0 - offset
  mrefs <- newPrimArray (size + 1)
  copyPrimArray mrefs 1 refs0 offset size
  ref <- insertEffect storage e f
  writePrimArray mrefs 0 ref
  refs <- unsafeFreezePrimArray mrefs
  pure $ Env 0 refs storage
{-# NOINLINE consEnv #-}

-- | Shrink the environment by one data type.
--
-- /Note:/ after calling this function the input environment is no longer
-- usable.
unconsEnv :: Env (e : es) -> IO ()
unconsEnv (Env _ refs storage) = do
  deleteEffect storage (indexPrimArray refs 0)
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
replaceEnv e f (Env offset refs0 storage) = do
  let size = sizeofPrimArray refs0 - offset
  mrefs <- newPrimArray size
  copyPrimArray mrefs 0 refs0 offset size
  ref <- insertEffect storage e f
  writePrimArray mrefs (reifyIndex @e @es) ref
  refs <- unsafeFreezePrimArray mrefs
  pure $ Env 0 refs storage
{-# NOINLINE replaceEnv #-}

-- | Remove a reference to the replaced effect.
--
-- /Note:/ after calling this function the input environment is no longer
-- usable.
unreplaceEnv :: forall e es. e :> es => Env es -> IO ()
unreplaceEnv (Env _ refs storage) = do
  deleteEffect storage $ indexPrimArray refs (reifyIndex @e @es)
{-# NOINLINE unreplaceEnv #-}

----------------------------------------

-- | Reference an existing effect from the top of the stack.
subsumeEnv :: forall e es. e :> es => Env es -> IO (Env (e : es))
subsumeEnv (Env offset refs0 storage) = do
  let size = sizeofPrimArray refs0 - offset
  mrefs <- newPrimArray (size + 1)
  copyPrimArray mrefs 1 refs0 offset size
  writePrimArray mrefs 0 $ indexPrimArray refs0 (offset + reifyIndex @e @es)
  refs <- unsafeFreezePrimArray mrefs
  pure $ Env 0 refs storage
{-# NOINLINE subsumeEnv #-}

----------------------------------------

-- | Construct an environment containing a permutation (with possible
-- duplicates) of a subset of effects from the input environment.
injectEnv :: forall xs es. Subset xs es => Env es -> IO (Env xs)
injectEnv (Env offset refs0 storage) = do
  let xs = reifyIndices @xs @es
  mrefs <- newPrimArray (length xs)
  let writeRefs i = \case
        []       -> pure ()
        (e : es) -> do
          writePrimArray mrefs i $ indexPrimArray refs0 (offset + e)
          writeRefs (i + 1) es
  writeRefs 0 xs
  refs <- unsafeFreezePrimArray mrefs
  pure $ Env 0 refs storage
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

-- | Replace the data type in the environment with a new value.
putEnv
  :: forall e es. e :> es
  => Env es -- ^ The environment.
  -> EffectRep (DispatchOf e) e
  -> IO ()
putEnv env e = do
  (i, es) <- getLocation @e env
  e `seq` writeSmallArray es i (toAny e)

-- | Modify the data type in the environment and return a value.
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

-- | Modify the data type in the environment.
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
getLocation (Env offset refs storage) = do
  let i = indexPrimArray refs $ offset + reifyIndex @e @es
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

-- | Given a reference to an effect from the top of the stack, delete it from
-- the storage.
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
relinkEnv storage (Env offset refs _) = pure $ Env offset refs storage

-- | Double the capacity of an array.
doubleCapacity :: Int -> Int
doubleCapacity n = max 1 n * 2

undefinedData :: HasCallStack => a
undefinedData = error "undefined data"
