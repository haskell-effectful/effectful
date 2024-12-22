{-# LANGUAGE ImplicitParams #-}
-- | Support for access to values supplied by a monadic action.
--
-- @since 2.7.0.0
module Effectful.Input.Static.Action
  ( -- * Effect
    Input

    -- ** Handlers
  , runInput

    -- ** Operations
  , input
  , inputs
  ) where

import Data.Kind
import GHC.Stack

import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import Effectful.Internal.Utils

-- | Provide access to values of type @i@ supplied by a monadic action.
data Input (i :: Type) :: Effect

type instance DispatchOf (Input i) = Static NoSideEffects

-- | Wrapper to prevent a space leak on reconstruction of 'Input' in
-- 'relinkInput' (see https://gitlab.haskell.org/ghc/ghc/-/issues/25520).
newtype InputImpl i es where
  InputImpl :: (HasCallStack => Eff es i) -> InputImpl i es

data instance StaticRep (Input i) where
  Input
    :: !(Env inputEs)
    -> !(InputImpl i inputEs)
    -> StaticRep (Input i)

-- | Run the 'Input' effect with the given action that supplies values.
runInput
  :: forall i es a
   . HasCallStack
  => (HasCallStack => Eff es i)
  -- ^ The action for input generation.
  -> Eff (Input i : es) a
  -> Eff es a
runInput inputAction action = unsafeEff $ \es -> do
  inlineBracket
    (consEnv (Input es inputImpl) relinkInput es)
    unconsEnv
    (unEff action)
  where
    inputImpl = InputImpl $ let ?callStack = thawCallStack ?callStack in inputAction

-- | Fetch the value.
input :: (HasCallStack, Input i :> es) => Eff es i
input = unsafeEff $ \es -> do
  Input inputEs (InputImpl inputAction) <- getEnv es
  -- Corresponds to thawCallStack in runInput.
  (`unEff` inputEs) $ withFrozenCallStack inputAction

-- | Fetch the result of applying a function to the value.
--
-- @'inputs' f ≡ f '<$>' 'input'@
inputs
  :: (HasCallStack, Input i :> es)
  => (i -> a) -- ^ The function to apply to the value.
  -> Eff es a
inputs f = f <$> input

----------------------------------------
-- Helpers

relinkInput :: Relinker StaticRep (Input i)
relinkInput = Relinker $ \relink (Input inputEs inputAction) -> do
  newActionEs <- relink inputEs
  pure $ Input newActionEs inputAction
