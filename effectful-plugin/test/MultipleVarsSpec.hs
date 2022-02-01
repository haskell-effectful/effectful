{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Effectful.Plugin #-}

module MultipleVarsSpec where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local

import Test.Hspec

data TaggedState k s :: Effect where
  TaggedGet :: forall k s m. TaggedState k s m s
  TaggedPut :: forall k s m. s -> TaggedState k s m ()

type instance DispatchOf (TaggedState k s) = 'Dynamic

taggedGet :: forall k s es. TaggedState k s :> es => Eff es s
taggedGet = send TaggedGet

taggedPut :: forall k s es. TaggedState k s :> es => s -> Eff es ()
taggedPut = send . TaggedPut

runTaggedState :: forall k s es a
                . s
               -> Eff (TaggedState k s : es) a
               -> Eff es (a, s)
runTaggedState s =
  reinterpret
  (runState s)
  $ \_ -> \case
    TaggedGet -> get
    TaggedPut s -> put s

test :: '[
          TaggedState Char Int
        , TaggedState Bool Int
        ] :>> es
     => Eff es ()
test = do
  taggedPut @Bool 10
  taggedPut @Char (-10)

spec :: Spec
spec = describe "Using multiple, but ununifiable instances\
               \ of the same effect" $ do
  it "should get disambiguated and compile, \
     \and actions should target the right effects." $ do
    let
      res1 =
          runPureEff
        . runTaggedState @Char 0
        . runTaggedState @Bool 7
        $ test
      res2 =
          runPureEff
        . runTaggedState @Bool 0
        . runTaggedState @Char 7
        $ test
      res3 =
          runPureEff
        . runTaggedState @Bool 0
        . runTaggedState @Char 7
        $ do
          taggedPut @Bool 10
          taggedPut @Char (-10)
    res1 `shouldBe` (((), 10), -10)
    res2 `shouldBe` (((), -10), 10)
    res3 `shouldBe` (((), -10), 10)
