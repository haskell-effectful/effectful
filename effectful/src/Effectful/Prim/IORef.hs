-- | Lifted "Data.IORef".
--
-- /Note:/ it requires 'Prim' because @MutVar@ from the @primitive@ library is a
-- generalization of 'IORef'.
--
-- @since 2.4.0.0
module Effectful.Prim.IORef
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
  , modifyIORef'
  , atomicModifyIORef
  , atomicModifyIORef'
  , atomicWriteIORef
  , mkWeakIORef
  ) where

import Data.IORef (IORef)
import Data.IORef qualified as Ref
import System.Mem.Weak (Weak)

import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import Effectful.Prim

-- | Lifted 'Ref.newIORef'.
newIORef :: Prim :> es => a -> Eff es (IORef a)
newIORef = unsafeEff_ . Ref.newIORef

-- | Lifted 'Ref.readIORef'.
readIORef :: Prim :> es => IORef a -> Eff es a
readIORef = unsafeEff_ . Ref.readIORef

-- | Lifted 'Ref.writeIORef'.
writeIORef :: Prim :> es => IORef a -> a -> Eff es ()
writeIORef var = unsafeEff_ . Ref.writeIORef var

-- | Lifted 'Ref.modifyIORef'.
modifyIORef :: Prim :> es => IORef a -> (a -> a) -> Eff es ()
modifyIORef var = unsafeEff_ . Ref.modifyIORef var

-- | Lifted 'Ref.modifyIORef''.
modifyIORef' :: Prim :> es => IORef a -> (a -> a) -> Eff es ()
modifyIORef' var = unsafeEff_ . Ref.modifyIORef' var

-- | Lifted 'Ref.atomicModifyIORef'.
atomicModifyIORef :: Prim :> es => IORef a -> (a -> (a, b)) -> Eff es b
atomicModifyIORef var = unsafeEff_ . Ref.atomicModifyIORef var

-- | Lifted 'Ref.atomicModifyIORef''.
atomicModifyIORef' :: Prim :> es => IORef a -> (a -> (a, b)) -> Eff es b
atomicModifyIORef' var = unsafeEff_ . Ref.atomicModifyIORef' var

-- | Lifted 'Ref.atomicWriteIORef''.
atomicWriteIORef :: Prim :> es => IORef a -> a -> Eff es ()
atomicWriteIORef var = unsafeEff_ . Ref.atomicWriteIORef var

-- | Lifted 'Ref.mkWeakIORef'.
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
