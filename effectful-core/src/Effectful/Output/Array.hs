module Effectful.Output.Array
  ( -- * Effect
    Output

    -- ** Handlers
  , runOutput

    -- ** Operations
  , output

    -- * Re-exports
  , Array
  ) where

import Control.Monad.Primitive
import Data.Kind
import Data.Primitive.Array

import Effectful
import Effectful.Dispatch.Static
import Effectful.Internal.Utils
import Effectful.Internal.Env

data Output (o :: Type) :: Effect

type instance DispatchOf (Output o) = Static NoSideEffects
data instance StaticRep (Output o) = Output !Int !(MutableArray RealWorld o)

runOutput :: HasCallStack => Eff (Output o : es) a -> Eff es (a, Array o)
runOutput action = unsafeEff $ \es0 -> do
  arr <- newArray 0 undefinedValue
  inlineBracket
    (consEnv (Output 0 arr) relinkOutput es0)
    unconsEnv
    (\es -> (,) <$> unEff action es <*> (getArray =<< getEnv es))
  where
    getArray (Output size arr) = freezeArray arr 0 size

output :: (HasCallStack, Output o :> es) => o -> Eff es ()
output o = unsafeEff $ \es -> do
  Output size arr0 <- getEnv es
  let len0 = sizeofMutableArray arr0
  arr <- case size `compare` len0 of
    GT -> error $ "size (" ++ show size ++ ") > len0 (" ++ show len0 ++ ")"
    LT -> pure arr0
    EQ -> do
      let len = growCapacity len0
      arr <- newArray len undefinedValue
      copyMutableArray arr 0 arr0 0 size
      pure arr
  writeArray arr size $! o
  putEnv es $ Output (size + 1) arr

----------------------------------------

relinkOutput :: Relinker StaticRep (Output o)
relinkOutput = Relinker $ \_ (Output size arr0) -> do
  arr <- cloneMutableArray arr0 0 (sizeofMutableArray arr0)
  pure $ Output size arr

undefinedValue :: HasCallStack => a
undefinedValue = error "Undefined value"