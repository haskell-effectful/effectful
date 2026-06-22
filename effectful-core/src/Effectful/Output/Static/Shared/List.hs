-- | Support for accumulation of values in a shared list.
--
-- @since 2.7.0.0
module Effectful.Output.Static.Shared.List
  ( -- * Effect
    Output

    -- ** Handlers
  , runOutput

    -- ** Operations
  , output
  ) where

import Control.Concurrent.MVar.Strict
import Data.Kind

import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive

-- | Provide access to accumulation of values of type @o@ in a shared list.
data Output (o :: Type) :: Effect

type instance DispatchOf (Output o) = Static NoSideEffects
newtype instance StaticRep (Output o) = Output (MVar' [o])

-- | Run the 'Output' effect and return the final value along with the
-- accumulated list.
runOutput :: HasCallStack => Eff (Output o : es) a -> Eff es (a, [o])
runOutput action = do
  v <- unsafeEff_ $ newMVar' []
  a <- evalStaticRep (Output v) action
  (a, ) . reverse <$> unsafeEff_ (readMVar' v)

-- | Append the value to the end of the list.
output
  :: (HasCallStack, Output o :> es)
  => o -- ^ The value.
  -> Eff es ()
output !o = unsafeEff $ \es -> do
  Output v <- getEnv es
  modifyMVar'_ v $ \acc -> pure (o : acc)
