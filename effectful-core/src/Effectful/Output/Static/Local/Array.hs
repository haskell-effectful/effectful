-- | Support for accumulation of values in a thread local array.
--
-- @since 2.7.0.0
module Effectful.Output.Static.Local.Array
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
import Effectful.Dispatch.Static.Primitive
import Effectful.Internal.Utils

-- | Provide access to accumulation of values of type @o@ in a thread local
-- array.
data Output (o :: Type) :: Effect

type instance DispatchOf (Output o) = Static NoSideEffects
data instance StaticRep (Output o) = Output !Int !(MutableArray RealWorld o)

-- | Run the 'Output' effect and return the final value along with the
-- accumulated array.
runOutput :: HasCallStack => Eff (Output o : es) a -> Eff es (a, Array o)
runOutput = runOutputImpl $ \(Output size arr) -> do
  freezeArray arr 0 size

-- | Append the value to the end of the array.
output
  :: (HasCallStack, Output o :> es)
  => o -- ^ The value.
  -> Eff es ()
output !o = unsafeEff $ \es -> do
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
  writeArray arr size o
  putEnv es $ Output (size + 1) arr

----------------------------------------
-- Helpers

runOutputImpl
  :: HasCallStack
  => (StaticRep (Output o) -> IO acc)
  -> Eff (Output o : es) a
  -> Eff es (a, acc)
runOutputImpl f action = unsafeEff $ \es0 -> do
  arr <- newArray 0 undefinedValue
  inlineBracket
    (consEnv (Output 0 arr) relinkOutput es0)
    unconsEnv
    (\es -> (,) <$> unEff action es <*> (f =<< getEnv es))
  where
    relinkOutput = Relinker $ \_ (Output size arr0) -> do
      arr <- cloneMutableArray arr0 0 (sizeofMutableArray arr0)
      pure $ Output size arr

undefinedValue :: HasCallStack => a
undefinedValue = error "Undefined value"
