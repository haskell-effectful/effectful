{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RequiredTypeArguments #-}
-- | Labeled effects with visible dependent quantification.
--
-- Any effect can be assigned multiple labels so you have more than one
-- available simultaneously.
--
-- Requires GHC >= 9.10.
--
-- @since 2.4.0.0
module Effectful.Labeled.VDQ
  ( -- * Effect
    Labeled(..)

    -- ** Handlers
  , runLabeled

    -- ** Operations
  , labeled
  ) where

import Unsafe.Coerce (unsafeCoerce)

import Effectful
import Effectful.Labeled (Labeled(..))

-- | Run a 'Labeled' effect with a given effect handler.
runLabeled
  :: forall label
  -> (Eff (e : es) a -> Eff es b)
  -- ^ The effect handler.
  -> Eff (Labeled label e : es) a
  -> Eff es b
runLabeled label runE m = runE (fromLabeled label m)

-- | Bring an effect into scope without a label.
--
-- Useful for running code designed with the non-labeled effect in mind.
--
-- >>> import Effectful.Reader.Static
--
-- >>> :{
--  action
--    :: ( Labeled "a" (Reader String) :> es
--       , Labeled "b" (Reader String) :> es
--       , Reader String :> es
--       )
--    => Eff es String
--  action = do
--    a <- labeled @(Reader String) "b" $ do
--      labeled @(Reader String) "a" $ do
--        ask
--    b <- labeled @(Reader String) "b" $ do
--      ask
--    pure $ a ++ b
-- :}
--
-- >>> :{
--  runPureEff @String
--    . runLabeled "a" (runReader "a")
--    . runLabeled "b" (runReader "b")
--    . runReader "c"
--    $ action
-- :}
-- "ab"
labeled
  :: forall label
  -> Labeled label e :> es
  => Eff (e : es) a
  -- ^ The action using the effect.
  -> Eff es a
labeled label m = subsume (toLabeled label m)

----------------------------------------
-- Helpers

fromLabeled :: forall label -> Eff (Labeled label e : es) a -> Eff (e : es) a
fromLabeled _ = unsafeCoerce

toLabeled :: forall label -> Eff (e : es) a -> Eff (Labeled label e : es) a
toLabeled _ = unsafeCoerce
