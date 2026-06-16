{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK not-home #-}
module Effectful.Internal.Env
  ( -- * The environment
    Env(..)
  , Ref(..)
  , Version
  , Storage(..)

    -- ** StorageData
  , StorageData(..)
  , cloneStorage
  , replaceStorage
  , backupStorageData
  , restoreStorageData

    -- *** Utils
  , AnyRelinker
  , toAnyRelinker
  , fromAnyRelinker
  , AnyEffect
  , toAnyEffect
  , fromAnyEffect

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
import Data.IORef.Strict qualified as S
import Data.Primitive.PrimArray
import Data.Primitive.SmallArray
import Data.Primitive.Types
import Data.Proxy
import GHC.Exts ((*#), (+#))
import GHC.Stack

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
  { offset  :: !Int
  , refs    :: !(PrimArray Ref)
  , storage :: !(S.IORef Storage)
  }

-- | Reference to the effect in 'Storage'.
data Ref = Ref !Int !Version

instance Prim Ref where
  sizeOfType# _ = 2# *# sizeOfType# (Proxy @Int)
  alignmentOfType# _ = alignmentOfType# (Proxy @Int)
  indexByteArray# arr i =
    let n = 2# *# i
        ref = indexByteArray# arr n
        version = indexByteArray# arr (n +# 1#)
    in Ref ref version
  readByteArray# arr i s0 =
    let n = 2# *# i
        !(# s1, ref #) = readByteArray# arr n s0
        !(# s2, version #) = readByteArray# arr (n +# 1#) s1
    in (# s2, Ref ref version #)
  writeByteArray# arr i (Ref ref version) s0 =
    let n = 2# *# i
        s1 = writeByteArray# arr n ref s0
        s2 = writeByteArray# arr (n +# 1#) version s1
    in s2
  indexOffAddr# addr i =
    let n = 2# *# i
        ref = indexOffAddr# addr n
        version = indexOffAddr# addr (n +# 1#)
    in Ref ref version
  readOffAddr# addr i s0 =
    let n = 2# *# i
        !(# s1, ref #) = readOffAddr# addr n s0
        !(# s2, version #) = readOffAddr# addr (n +# 1#) s1
    in (# s2, Ref ref version #)
  writeOffAddr# addr i (Ref ref version) s0 =
    let n = 2# *# i
        s1 = writeOffAddr# addr n ref s0
        s2 = writeOffAddr# addr (n +# 1#) version s1
    in s2

-- | Version of the effect.
newtype Version = Version Int
  deriving newtype (Eq, Ord, Prim, Show)

-- | A storage of effects.
data Storage = Storage
  { version :: !Version
  , data_   :: {-# UNPACK #-} !StorageData
  }

----------------------------------------
-- StorageData

-- | Effect in 'Storage'.
newtype AnyEffect = AnyEffect Any

toAnyEffect :: EffectRep (DispatchOf e) e -> AnyEffect
toAnyEffect = AnyEffect . toAny

fromAnyEffect :: AnyEffect -> EffectRep (DispatchOf e) e
fromAnyEffect (AnyEffect e) = fromAny e

-- | Relinker in 'Storage'.
newtype AnyRelinker = AnyRelinker Any

toAnyRelinker :: Relinker (EffectRep (DispatchOf e)) e -> AnyRelinker
toAnyRelinker = AnyRelinker . toAny

fromAnyRelinker :: AnyRelinker -> Relinker (EffectRep (DispatchOf e)) e
fromAnyRelinker (AnyRelinker f) = fromAny f

----------------------------------------

data StorageData = StorageData
  { size      :: !Int
  , versions  :: !(MutablePrimArray RealWorld Version)
  , effects   :: !(SmallMutableArray RealWorld AnyEffect)
  , relinkers :: !(SmallMutableArray RealWorld AnyRelinker)
  }

-- | Clone the storage to use it in a different thread.
--
-- @since 2.7.0.0
cloneStorage :: HasCallStack => S.IORef Storage -> IO (S.IORef Storage)
cloneStorage storage0 = do
  Storage version storageData0 <- S.readIORef storage0
  storageData <- copyStorageData storageData0
  storage <- S.newIORef $ Storage version storageData
  relinkStorageData storageData storage
  pure storage

-- | Replace the storage of the environment.
--
-- @since 2.7.0.0
replaceStorage :: Env es -> S.IORef Storage -> IO (Env es)
replaceStorage (Env offset refs _) storage = pure $ Env offset refs storage

-- | Make a shallow copy of the 'StorageData'.
--
-- @since 2.5.0.0
copyStorageData :: HasCallStack => StorageData -> IO StorageData
copyStorageData (StorageData storageSize vs0 es0 fs0) = do
  vsSize <- getSizeofMutablePrimArray  vs0
  esSize <- getSizeofSmallMutableArray es0
  fsSize <- getSizeofSmallMutableArray fs0
  when (vsSize /= esSize) $ do
    error $ "vsSize (" ++ show vsSize ++ ") /= esSize (" ++ show esSize ++ ")"
  when (esSize /= fsSize) $ do
    error $ "esSize (" ++ show esSize ++ ") /= fsSize (" ++ show fsSize ++ ")"
  vs <- cloneMutablePrimArray  vs0 0 vsSize
  es <- cloneSmallMutableArray es0 0 esSize
  fs <- cloneSmallMutableArray fs0 0 fsSize
  pure $ StorageData storageSize vs es fs

-- | Relink effects in the storage data to the given storage.
--
-- @since 2.7.0.0
relinkStorageData :: HasCallStack => StorageData -> S.IORef Storage -> IO ()
relinkStorageData (StorageData storageSize _ es fs) storage = go storageSize
  where
    go = \case
      0 -> pure ()
      k -> do
        let i = k - 1
        Relinker relinker <- fromAnyRelinker <$> readSmallArray fs i
        readSmallArray es i
          >>= relinker (relinkEnv storage) . fromAnyEffect
          >>= writeSmallArray' es i . toAnyEffect
        go i

-- | Backup storage data of the environment.
--
-- It can be restored later with 'restoreStorageData'.
--
-- @since 2.7.0.0
backupStorageData :: HasCallStack => Env es -> IO StorageData
backupStorageData env = do
  storageData <- copyStorageData . (.data_) =<< S.readIORef env.storage
  -- Relinking to the same storage might seem weird, but relinkers need to run
  -- and make a copy of mutable data associated with statically dispatched
  -- effects if appropriate.
  relinkStorageData storageData env.storage
  pure storageData

-- | Restore a copy of the 'StorageData'.
--
-- The copy needs to be from the same 'Env' as the target. It's consumed by this
-- operation and must not be used afterwards.
--
-- @since 2.5.0.0
restoreStorageData :: HasCallStack => StorageData -> Env es -> IO ()
restoreStorageData (StorageData newSize vs1 es1 fs1) env = do
  Storage version (StorageData oldSize vs0 es0 fs0) <- S.readIORef env.storage
  when (newSize /= oldSize) $ do
    error $ "newSize (" ++ show newSize ++ ") /= oldSize (" ++ show oldSize ++ ")"
  -- Since the time the backup was made the storage might've been grown by
  -- 'insertEffect', so if necessary create new arrays matching the current
  -- capacity, as shrinking it would violate the invariant that out of date
  -- references in 'getLocation' never read out of bounds.
  vs0size <- getSizeofMutablePrimArray vs0
  vs1size <- getSizeofMutablePrimArray vs1
  vs <- if vs0size > vs1size
    then do
      vs <- newPrimArray vs0size
      copyMutablePrimArray vs 0 vs1 0 newSize
      -- Fill the unused part of the versions array with
      -- 'undefinedVersion' to maintain the invariant that slots beyond
      -- the size of the storage never contain garbage (see the note on
      -- 'undefinedVersion').
      setPrimArray vs newSize (vs0size - newSize) undefinedVersion
      pure vs
    else pure vs1
  es0size <- getSizeofSmallMutableArray es0
  es1size <- getSizeofSmallMutableArray es1
  es <- if es0size > es1size
    then do
      es <- newSmallArray es0size undefinedEffect
      copySmallMutableArray es 0 es1 0 newSize
      pure es
    else pure es1
  fs0size <- getSizeofSmallMutableArray fs0
  fs1size <- getSizeofSmallMutableArray fs1
  fs <- if fs0size > fs1size
    then do
      fs <- newSmallArray fs0size undefinedRelinker
      copySmallMutableArray fs 0 fs1 0 newSize
      pure fs
    else pure fs1
  S.writeIORef env.storage $ Storage version (StorageData newSize vs es fs)

----------------------------------------
-- Relinker

-- | A function for relinking 'Env' objects stored in the handlers and/or making
-- a deep copy of the representation of the effect when cloning the environment.
newtype Relinker :: (Effect -> Type) -> Effect -> Type where
  Relinker
    :: (HasCallStack => (forall es. Env es -> IO (Env es)) -> rep e -> IO (rep e))
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
emptyEnv :: HasCallStack => IO (Env '[])
emptyEnv = Env 0
  <$> (unsafeFreezePrimArray =<< newPrimArray 0)
  <*> (S.newIORef =<< emptyStorage)

-- | Clone the environment to use it in a different thread.
cloneEnv :: HasCallStack => Env es -> IO (Env es)
cloneEnv env = replaceStorage env =<< cloneStorage env.storage

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
  :: HasCallStack
  => EffectRep (DispatchOf e) e
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
-- The environment needs to come from 'consEnv', i.e. the intended usage is
-- @bracket (consEnv e f env) unconsEnv@.
--
-- /Note:/ after calling this function @e@ from the input environment is no
-- longer usable.
unconsEnv :: HasCallStack => Env (e : es) -> IO ()
unconsEnv (Env offset refs storage) = do
  when (offset /= 0) $ do
    error $ "offset (" ++ show offset ++ ") /= 0"
  deleteEffect storage (indexPrimArray refs 0)
{-# NOINLINE unconsEnv #-}

----------------------------------------

-- | Replace a specific effect in the stack with a new value.
--
-- /Note:/ unlike in 'putEnv' the value in not changed in place, so only the new
-- environment will see it.
replaceEnv
  :: forall e es. (HasCallStack, e :> es)
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
-- The environment needs to come from 'replaceEnv', i.e. the intended usage is
-- @bracket (replaceEnv e f env) unreplaceEnv@.
--
-- /Note:/ after calling this function the input environment is no longer
-- usable.
unreplaceEnv :: forall e es. (HasCallStack, e :> es) => Env es -> IO ()
unreplaceEnv (Env offset refs storage) = do
  when (offset /= 0) $ do
    error $ "offset (" ++ show offset ++ ") /= 0"
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
injectEnv :: forall subEs es. Subset subEs es => Env es -> IO (Env subEs)
injectEnv (Env offset refs0 storage) = do
  let subEs      = reifyIndices @subEs @es
      subEsSize  = length subEs
      prefixSize = prefixLength @es
      suffixSize = if subsetFullyKnown @subEs @es
                   then 0
                   else sizeofPrimArray refs0 - offset - prefixSize
  mrefs <- newPrimArray (subEsSize + suffixSize)
  copyPrimArray mrefs subEsSize refs0 (offset + prefixSize) suffixSize
  let writeRefs i = \case
        []       -> pure ()
        (x : xs) -> do
          writePrimArray mrefs i $ indexPrimArray refs0 (offset + x)
          writeRefs (i + 1) xs
  writeRefs 0 subEs
  refs <- unsafeFreezePrimArray mrefs
  pure $ Env 0 refs storage
{-# NOINLINE injectEnv #-}

----------------------------------------
-- Data retrieval and update

-- | Extract a specific data type from the environment.
getEnv
  :: forall e es. (HasCallStack, e :> es)
  => Env es -- ^ The environment.
  -> IO (EffectRep (DispatchOf e) e)
getEnv env = do
  (i, es) <- getLocation @e env
  fromAnyEffect <$> readSmallArray es i

-- | Replace the data type in the environment with a new value (in place).
putEnv
  :: forall e es. (HasCallStack, e :> es)
  => Env es -- ^ The environment.
  -> EffectRep (DispatchOf e) e
  -> IO ()
putEnv env e = do
  (i, es) <- getLocation @e env
  writeSmallArray' es i (toAnyEffect e)

-- | Modify the data type in the environment and return a value (in place).
stateEnv
  :: forall e es a. (HasCallStack, e :> es)
  => Env es -- ^ The environment.
  -> (EffectRep (DispatchOf e) e -> (a, EffectRep (DispatchOf e) e))
  -> IO a
stateEnv env f = do
  (i, es) <- getLocation @e env
  (a, e) <- f . fromAnyEffect <$> readSmallArray es i
  writeSmallArray' es i (toAnyEffect e)
  pure a

-- | Modify the data type in the environment (in place).
modifyEnv
  :: forall e es. (HasCallStack, e :> es)
  => Env es -- ^ The environment.
  -> (EffectRep (DispatchOf e) e -> (EffectRep (DispatchOf e) e))
  -> IO ()
modifyEnv env f = do
  (i, es) <- getLocation @e env
  e <- f . fromAnyEffect <$> readSmallArray es i
  writeSmallArray' es i (toAnyEffect e)

-- | Determine location of the effect in the environment.
getLocation
  :: forall e es. (HasCallStack, e :> es)
  => Env es
  -> IO (Int, SmallMutableArray RealWorld AnyEffect)
getLocation (Env offset refs storage) = do
  Storage _ (StorageData _ vs es _) <- S.readIORef storage
  storageVersion <- readPrimArray vs ref
  -- If version of the reference is different than version in the storage, it
  -- means that the effect in the storage is not the one that was initially
  -- referenced.
  when (version /= storageVersion) $ do
    error $ "version (" ++ show version ++ ") /= storageVersion ("
         ++ show storageVersion ++ ")\n"
         ++ "If you're attempting to run an unlifting function outside "
         ++ "of the scope of effects it captures, have a look at "
         ++ "UnliftStrategy (SeqForkUnlift)."
  pure (ref, es)
  where
    Ref ref version = indexPrimArray refs (offset + reifyIndex @e @es)

----------------------------------------
-- Internal helpers

-- | Create an empty storage.
emptyStorage :: HasCallStack => IO Storage
emptyStorage = Storage initialVersion <$> storageData
  where
    storageData = StorageData 0
      <$> newPrimArray 0
      <*> newSmallArray 0 undefinedEffect
      <*> newSmallArray 0 undefinedRelinker

-- | Insert an effect into the storage and return its reference.
insertEffect
  :: HasCallStack
  => S.IORef Storage
  -> EffectRep (DispatchOf e) e
  -- ^ The representation of the effect.
  -> Relinker (EffectRep (DispatchOf e)) e
  -> IO Ref
insertEffect storage e f = do
  Storage version (StorageData size vs0 es0 fs0) <- S.readIORef storage
  len0 <- getSizeofSmallMutableArray es0
  case size `compare` len0 of
    GT -> error $ "size (" ++ show size ++ ") > len0 (" ++ show len0 ++ ")"
    LT -> do
      writePrimArray   vs0 size version
      writeSmallArray' es0 size (toAnyEffect e)
      writeSmallArray' fs0 size (toAnyRelinker f)
      S.writeIORef storage $
        Storage (bumpVersion version) (StorageData (size + 1) vs0 es0 fs0)
      pure $ Ref size version
    EQ -> do
      let len = growCapacity len0
      vs <- newPrimArray len
      es <- newSmallArray len undefinedEffect
      fs <- newSmallArray len undefinedRelinker
      -- Fill the unused part of the versions array with 'undefinedVersion' to
      -- maintain the invariant that slots beyond the size of the storage never
      -- contain garbage (see the note on 'undefinedVersion').
      setPrimArray vs size (len - size) undefinedVersion
      copyMutablePrimArray  vs 0 vs0 0 size
      copySmallMutableArray es 0 es0 0 size
      copySmallMutableArray fs 0 fs0 0 size
      writePrimArray   vs size version
      writeSmallArray' es size (toAnyEffect e)
      writeSmallArray' fs size (toAnyRelinker f)
      S.writeIORef storage $
        Storage (bumpVersion version) (StorageData (size + 1) vs es fs)
      pure $ Ref size version

-- | Given a reference to an effect from the top of the stack, delete it from
-- the storage.
deleteEffect :: HasCallStack => S.IORef Storage -> Ref -> IO ()
deleteEffect storage (Ref ref version) = do
  Storage currentVersion (StorageData size vs es fs) <- S.readIORef storage
  when (ref /= size - 1) $ do
    error $ "ref (" ++ show ref ++ ") /= size - 1 (" ++ show (size - 1) ++ ")"
  storageVersion <- readPrimArray vs ref
  when (version /= storageVersion) $ do
    error $ "version (" ++ show version ++ ") /= storageVersion ("
         ++ show storageVersion ++ ")\n"
  writePrimArray  vs ref undefinedVersion
  writeSmallArray es ref undefinedEffect
  writeSmallArray fs ref undefinedRelinker
  S.writeIORef storage $ Storage currentVersion (StorageData (size - 1) vs es fs)

-- | Relink the environment to use the new storage.
relinkEnv :: S.IORef Storage -> Env es -> IO (Env es)
relinkEnv storage (Env offset refs _) = pure $ Env offset refs storage

-- | Version of an unused slot.
--
-- /Note:/ slots of the versions array beyond the current size of the storage
-- always contain 'undefinedVersion', so that out of date references to them
-- reliably fail the version check in 'getLocation'. This invariant is
-- maintained by 'insertEffect', 'deleteEffect' and 'restoreStorageData'.
undefinedVersion :: Version
undefinedVersion = Version 0

initialVersion :: Version
initialVersion = Version 1

bumpVersion :: Version -> Version
bumpVersion (Version n) = Version (n + 1)

undefinedEffect :: HasCallStack => AnyEffect
undefinedEffect = toAnyEffect . errorWithoutStackTrace $ unlines
  [ "Undefined effect"
  , "Created at: " ++ prettyCallStack callStack
  ]

undefinedRelinker :: HasCallStack => AnyRelinker
undefinedRelinker = toAnyRelinker $ Relinker $ \_ _ -> do
  errorWithoutStackTrace $ unlines
    [ "Undefined relinker"
    , "Created at: " ++ prettyCallStack creationCallStack
    , "Called at: " ++ prettyCallStack callStack
    ]
  where
    creationCallStack = callStack

-- | A strict version of 'writeSmallArray'.
writeSmallArray' :: SmallMutableArray RealWorld a -> Int -> a -> IO ()
writeSmallArray' arr i a = a `seq` writeSmallArray arr i a
