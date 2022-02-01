{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AmbiguousSpec where

import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity
import Data.Monoid
import Effectful
import Effectful.Error.Dynamic
import Effectful.State.Dynamic
import Test.Hspec
import Test.ShouldNotTypecheck

class MPTC a b where
  mptc :: a -> b

instance MPTC Bool Int where
  mptc _ = 1000


uniquelyInt :: '[State Int , State String] :>> es => Eff es ()
uniquelyInt = put 10

uniquelyA :: forall a b es. (Num a, '[State a, State b] :>> es) => Eff es ()
uniquelyA = put 10

uniquelyString :: '[State Int , State String] :>> es => Eff es ()
uniquelyString = put mempty

uniquelyB :: (MPTC Bool b, '[State String, State b] :>> es) => Eff es ()
uniquelyB = put $ mptc False

-- uniquelyIO :: '[Embed IO, Embed Identity] :>> es => Eff es ()
-- uniquelyIO = embed $ liftIO $ pure ()

uniquelyState' :: ∀ es . [Error (), State ()] :>> es => Eff es ()
uniquelyState' = pure ()

-- uniquelyState :: ∀ r . (Tagged () (State ())) :> es => Eff es (Either () ())
-- uniquelyState = runError (tag @() uniquelyState')
-- uniquelyState = runError (tag @() (uniquelyState' @(State () : Error () : r)))

spec :: Spec
spec = describe "example" $ do
  it "should run uniquelyInt" $ do
    let z = runPureEff . runLocalState 0 . runLocalState "hello" $ uniquelyInt
    z `shouldBe` (((), "hello"), 10)

  it "should run uniquelyA" $ do
    let z = runPureEff . runLocalState 0 . runLocalState "hello" $ uniquelyA @Int @String
    z `shouldBe` (((), "hello"), 10)

  it "should run uniquelyB" $ do
    let z = runPureEff . runLocalState 0 . runLocalState "hello" $ uniquelyB @Int
    z `shouldBe` (((), "hello"), 1000)

  it "should run uniquelyString" $ do
    let z = runPureEff . runLocalState 0 . runLocalState "hello" $ uniquelyString
    z `shouldBe` (((), ""), 0)

  -- it "should run uniquelyIO" $ do
  --   z <- runM . runEmbedded @Identity (pure . runIdentity) $ uniquelyIO
  --   z `shouldBe` ()

