-- | Lifted "Data.STRef"
module Effectful.ST.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef, modifySTRef') where

import Data.STRef (STRef)
import Data.STRef qualified as STRef

import Effectful
import Effectful.ST

-- Lifted 'STRef.newSTRef'
newSTRef :: forall s a es. (STE s :> es) => a -> Eff es (STRef s a)
newSTRef = liftST . STRef.newSTRef

-- Lifted 'STRef.readSTRef'
readSTRef :: forall s a es. (STE s :> es) => STRef s a -> Eff es a
readSTRef = liftST . STRef.readSTRef

-- Lifted 'STRef.writeSTRef'
writeSTRef :: forall s a es. (STE s :> es) => STRef s a -> a -> Eff es ()
writeSTRef ref = liftST . STRef.writeSTRef ref

-- Lifted 'STRef.modifySTRef'
modifySTRef :: forall s a es. (STE s :> es) => STRef s a -> (a -> a) -> Eff es ()
modifySTRef ref = liftST . STRef.modifySTRef ref

-- Lifted 'STRef.modifySTRef''
modifySTRef' :: forall s a es. (STE s :> es) => STRef s a -> (a -> a) -> Eff es ()
modifySTRef' ref = liftST . STRef.modifySTRef' ref
