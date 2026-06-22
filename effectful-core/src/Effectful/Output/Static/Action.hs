{-# LANGUAGE ImplicitParams #-}
-- | Support for feeding values of a particular type to a monadic action.
--
-- @since 2.7.0.0
module Effectful.Output.Static.Action
  ( -- * Effect
    Output

    -- ** Handlers
  , runOutput

    -- ** Operations
  , output
  ) where

import Data.Kind
import GHC.Stack

import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive
import Effectful.Internal.Utils

-- | Provide the ability to feed values of type @o@ to a monadic action.
data Output (o :: Type) :: Effect

type instance DispatchOf (Output o) = Static NoSideEffects

-- | Wrapper to prevent a space leak on reconstruction of 'Output' in
-- 'relinkOutput' (see https://gitlab.haskell.org/ghc/ghc/-/issues/25520).
newtype OutputImpl o es where
  OutputImpl :: (HasCallStack => o -> Eff es ()) -> OutputImpl o es

data instance StaticRep (Output o) where
  Output
    :: !(Env actionEs)
    -> !(OutputImpl o actionEs)
    -> StaticRep (Output o)

-- | Run the 'Output' effect with the given action for receiving values.
runOutput
  :: forall o es a
   . HasCallStack
  => (HasCallStack => o -> Eff es ())
  -- ^ The action for receiving values.
  -> Eff (Output o : es) a
  -> Eff es a
runOutput outputAction action = unsafeEff $ \es -> do
  inlineBracket
    (consEnv (Output es outputImpl) relinkOutput es)
    unconsEnv
    (unEff action)
  where
    outputImpl = OutputImpl $ let ?callStack = thawCallStack ?callStack in outputAction

-- | Feed the value to the underlying monadic action.
output
  :: (HasCallStack, Output o :> es)
  => o -- ^ The value.
  -> Eff es ()
output !o = unsafeEff $ \es -> do
  Output actionEs (OutputImpl outputAction) <- getEnv es
  -- Corresponds to thawCallStack in runOutput.
  (`unEff` actionEs) $ withFrozenCallStack outputAction o

----------------------------------------
-- Helpers

relinkOutput :: Relinker StaticRep (Output o)
relinkOutput = Relinker $ \relink (Output actionEs outputAction) -> do
  newActionEs <- relink actionEs
  pure $ Output newActionEs outputAction
