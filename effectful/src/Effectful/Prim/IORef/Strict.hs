-- | Lifted "Data.IORef.Strict".
--
-- @since 2.4.0.0
module Effectful.Prim.IORef.Strict
  ( -- * Effect
    Prim

    -- ** Handlers
  , runPrim

    -- * IORef
  , IORef
  , newIORef
  , readIORef
  , writeIORef
  , modifyIORef
  , atomicModifyIORef
  , atomicWriteIORef
  , mkWeakIORef
  ) where

import Data.IORef.Strict (IORef)
import Data.IORef.Strict qualified as Ref
import System.Mem.Weak (Weak)

import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import Effectful.Prim

-- | Lifted 'Data.IORef.Strict.newIORef'.
newIORef :: Prim :> es => a -> Eff es (IORef a)
newIORef = unsafeEff_ . Ref.newIORef

-- | Lifted 'Data.IORef.Strict.readIORef'.
readIORef :: Prim :> es => IORef a -> Eff es a
readIORef = unsafeEff_ . Ref.readIORef

-- | Lifted 'Data.IORef.Strict.writeIORef'.
writeIORef :: Prim :> es => IORef a -> a -> Eff es ()
writeIORef var = unsafeEff_ . Ref.writeIORef var

-- | Lifted 'Data.IORef.Strict.modifyIORef'.
modifyIORef :: Prim :> es => IORef a -> (a -> a) -> Eff es ()
modifyIORef var = unsafeEff_ . Ref.modifyIORef var

-- | Lifted 'Data.IORef.Strict.atomicModifyIORef'.
atomicModifyIORef :: Prim :> es => IORef a -> (a -> (a, b)) -> Eff es b
atomicModifyIORef var = unsafeEff_ . Ref.atomicModifyIORef var

-- | Lifted 'Data.IORef.Strict.atomicWriteIORef'.
atomicWriteIORef :: Prim :> es => IORef a -> a -> Eff es ()
atomicWriteIORef var = unsafeEff_ . Ref.atomicWriteIORef var

-- | Lifted 'Data.IORef.Strict.mkWeakIORef'.
--
-- /Note:/ the finalizer will run a cloned environment, so any changes it makes
-- to thread local data will not be visible outside of it.
mkWeakIORef
  :: (HasCallStack, Prim :> es)
  => IORef a
  -> Eff es ()
  -> Eff es (Weak (IORef a))
mkWeakIORef var f = unsafeEff $ \es -> do
  -- The finalizer can run at any point and in any thread.
  Ref.mkWeakIORef var . unEff f =<< cloneEnv es
