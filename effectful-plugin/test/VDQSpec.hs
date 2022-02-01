{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Effectful.Plugin #-}

module VDQSpec where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Resource
import Test.Hspec

data Select a = Select a

data DBAction whichDb m a where
  DoSelect :: Select a -> DBAction whichDb m (Maybe a)

type instance DispatchOf (DBAction which) = 'Dynamic

doSelect :: DBAction which :> es => Select a -> Eff es (Maybe a)
doSelect = send . DoSelect

runDBAction :: Eff (DBAction which : es) a -> Eff es a
runDBAction = interpret $ \_ -> \case
  DoSelect (Select a) -> pure $ Just a

spec :: Spec
spec = describe "example" $ do
  it "should compile!" $ do
    let z = runPureEff . runDBAction $ doSelect $ Select True
    z `shouldBe` Just True

