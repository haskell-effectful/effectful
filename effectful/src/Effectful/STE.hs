module Effectful.STE
  ( STE

  , MutVar
  , runSTE
  , newMutVar
  , readMutVar
  , writeMutVar
  , atomicModifyMutVar
  , modifyMutVar

  , MutVarU
  , newMutVarU
  , readMutVarU
  , writeMutVarU
  , modifyMutVarU
  ) where

import Control.Monad.Primitive
import Data.Coerce
import Data.Primitive.Types
import qualified Data.Primitive.MutVar as P
import qualified Data.Primitive.PrimArray as P

import Effectful
import Effectful.Dispatch.Static
import Effectful.Internal.Effect

-- | Dirty trick for disambiguation of STE type parameters.
instance {-# INCOHERENT #-} s1 ~ s2 => STE s1 :> (STE s2 : es) where
  reifyIndex = 0

type role STE nominal phantom phantom
data STE s :: Effect
type instance DispatchOf (STE s) = Static NoSideEffects
data instance StaticRep (STE s) = STE

runSTE :: (forall s. Eff (STE s : es) a) -> Eff es a
runSTE m = evalStaticRep STE m

----------------------------------------
-- MutVar

type role MutVar nominal representational
newtype MutVar s a = MutVar (P.MutVar RealWorld a)
  deriving Eq

newMutVar :: STE s :> es => a -> Eff es (MutVar s a)
newMutVar = coerce . unsafeEff_ . P.newMutVar

readMutVar :: STE s :> es => MutVar s a -> Eff es a
readMutVar = unsafeEff_ . P.readMutVar . coerce

writeMutVar :: STE s :> es => MutVar s a -> a -> Eff es ()
writeMutVar (MutVar mv) a = unsafeEff_ $ a `seq` P.writeMutVar mv a

atomicModifyMutVar :: STE s :> es => MutVar s a -> (a -> (a, b)) -> Eff es b
atomicModifyMutVar (MutVar mv) = unsafeEff_ . P.atomicModifyMutVar' mv

modifyMutVar :: STE s :> es => MutVar s a -> (a -> a) -> Eff es ()
modifyMutVar (MutVar mv) = unsafeEff_ . P.modifyMutVar' mv

----------------------------------------
-- MutVarU

type role MutVarU nominal representational
newtype MutVarU s a = MutVarU (P.MutablePrimArray RealWorld a)
  deriving Eq

newMutVarU :: (Prim a, STE s :> es) => a -> Eff es (MutVarU s a)
newMutVarU a = unsafeEff_ $ do
  arr <- P.newPrimArray 1
  P.writePrimArray arr 0 a
  pure (MutVarU arr)

readMutVarU :: (Prim a, STE s :> es) => MutVarU s a -> Eff es a
readMutVarU (MutVarU arr) = unsafeEff_ $ P.readPrimArray arr 0

writeMutVarU :: (Prim a, STE s :> es) => MutVarU s a -> a -> Eff es ()
writeMutVarU (MutVarU arr) = unsafeEff_ . P.writePrimArray arr 0

modifyMutVarU :: (Prim a, STE s :> es) => MutVarU s a -> (a -> a) -> Eff es ()
modifyMutVarU (MutVarU arr) f = unsafeEff_ $ do
  P.writePrimArray arr 0 . f =<< P.readPrimArray arr 0
