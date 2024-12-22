module Effectful.Coroutine
  ( -- * Effect
    Coroutine(..)
  , Input
  , Output

    -- ** Handlers
  , runCoroutine
  , runInputConst
  , runOutputArray
  , runOutputList

    -- ** Operations
  , yield
  , input
  , output
  ) where

import Data.Bifunctor
import Data.Kind

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Input.Const qualified as IC
import Effectful.Output.Array qualified as OA
import Effectful.State.Static.Local qualified as S

data Coroutine (a :: Type) (b :: Type) :: Effect where
  Yield :: a -> Coroutine a b m b

type instance DispatchOf (Coroutine i o) = Dynamic

type Input i = Coroutine () i

type Output o = Coroutine o ()

----------------------------------------
-- Handlers

-- | Run the 'Coroutine' effect via a given action.
runCoroutine
  :: HasCallStack
  => (a -> Eff es b)
  -- ^ The action.
  -> Eff (Coroutine a b : es) a
  -> Eff es a
runCoroutine f = interpret_ $ \case
  Yield a -> f a

-- | Run the 'Coroutine' effect via "Effectful.Input.Const".
runInputConst
  :: HasCallStack
  => i
  -- ^ The input.
  -> Eff (Input i : es) a
  -> Eff es a
runInputConst i = reinterpret_ (IC.runInput i) $ \case
  Yield () -> IC.input

-- | Run the 'Coroutine' effect via "Effectful.Output.Array".
runOutputArray
  :: HasCallStack
  => Eff (Output o : es) a
  -- ^ .
  -> Eff es (a, OA.Array o)
runOutputArray = reinterpret_ OA.runOutput $ \case
  Yield o -> OA.output o

runOutputList
  :: HasCallStack
  => Eff (Output o : es) a
  -- ^ .
  -> Eff es (a, [o])
runOutputList = reinterpret_ setup $ \case
  Yield o -> S.modify (o :)
  where
    setup = fmap (second reverse) . S.runState []

----------------------------------------
-- Operations

-- | Yield to the handler with the given value.
yield :: forall b a es. (HasCallStack, Coroutine a b :> es) => a -> Eff es b
yield = send . Yield

-- | Request the value from the handler.
input :: (HasCallStack, Coroutine () i :> es) => Eff es i
input = send $ Yield ()

-- | Pass the value to the handler.
output :: (HasCallStack, Coroutine o () :> es) => o -> Eff es ()
output = send . Yield
