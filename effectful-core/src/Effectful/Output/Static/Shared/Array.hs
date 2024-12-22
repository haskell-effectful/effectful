-- | Support for accumulation of values in a shared array.
--
-- @since 2.7.0.0
module Effectful.Output.Static.Shared.Array
  ( -- * Effect
    Output

    -- ** Handlers
  , runOutput

    -- ** Operations
  , output

    -- * Re-exports
  , Array
  ) where

import Control.Concurrent.MVar.Strict
import Control.Monad.Primitive
import Data.Kind
import Data.Primitive.Array

import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import Effectful.Internal.Utils

-- | Provide access to accumulation of values of type @o@ in a shared array.
data Output (o :: Type) :: Effect

data OutputData o = OutputData !Int !(MutableArray RealWorld o)

type instance DispatchOf (Output o) = Static NoSideEffects
newtype instance StaticRep (Output o) = Output (MVar' (OutputData o))

-- | Run the 'Output' effect and return the final value along with the
-- accumulated array.
runOutput :: HasCallStack => Eff (Output o : es) a -> Eff es (a, Array o)
runOutput = runOutputImpl $ \(OutputData size arr) -> do
  freezeArray arr 0 size

-- | Append the value to the end of the array.
output
  :: (HasCallStack, Output o :> es)
  => o -- ^ The value.
  -> Eff es ()
output !o = unsafeEff $ \es -> do
  Output v <- getEnv es
  modifyMVar'_ v $ \(OutputData size arr0) -> do
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
    pure $ OutputData (size + 1) arr

----------------------------------------
-- Helpers

runOutputImpl
  :: HasCallStack
  => (OutputData o -> IO acc)
  -> Eff (Output o : es) a
  -> Eff es (a, acc)
runOutputImpl f action = do
  v <- unsafeEff_ $ newMVar' . OutputData 0 =<< newArray 0 undefinedValue
  a <- evalStaticRep (Output v) action
  acc <- unsafeEff_ $ f =<< readMVar' v
  pure (a, acc)

undefinedValue :: HasCallStack => a
undefinedValue = error "Undefined value"
