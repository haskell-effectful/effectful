module Effectful.Reader.Dynamic
  ( Reader(..)
  , runReader
  , ask
  , asks
  , local
  ) where

import Effectful.Dispatch.Dynamic
import Effectful.Monad
import qualified Effectful.Reader as R

data Reader r :: Effect where
  Ask   :: Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a

type instance EffectStyle (Reader r) = DynamicEffect

runReader :: r -> Eff (Reader r : es) a -> Eff es a
runReader r = reinterpretDynamic (R.runReader r) $ \env -> \case
  Ask       -> R.ask
  Local f m -> localSeqUnlift env $ \unlift -> R.local f (unlift m)

----------------------------------------
-- Operations

ask :: (HasCallStack, Reader r :> es) => Eff es r
ask = send Ask

asks :: (HasCallStack, Reader r :> es) => (r -> a) -> Eff es a
asks f = f <$> ask

local :: (HasCallStack, Reader r :> es ) => (r -> r) -> Eff es a -> Eff es a
local f = send . Local f
