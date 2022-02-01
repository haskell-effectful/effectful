{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module PluginSpec where

import GHC.Exts
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Test.Hspec
import Unsafe.Coerce



idState :: State s :> es => Eff es ()
idState = do
  s <- get
  put s

intState :: State Int :> es => Eff es ()
intState = put 10

numState :: Num a => State a :> es => Eff es ()
numState = put 10

strState :: State String :> es => Eff es ()
strState = put "hello"

oStrState :: IsString a => State a :> es => Eff es ()
oStrState = put "hello"


err :: Error e :> es => Eff es Bool
err =
  catchError
    (throwError undefined)
    (\_ _ -> pure True)


errState :: Num s => '[Error e, State s] :>> es => Eff es Bool
errState = do
  numState
  err


-- lifted :: Monad m => Embed m :> es => Eff es ()
-- lifted = embed $ pure ()


newtype MyString = MyString String
  deriving (IsString, Eq, Show)


data Janky = forall s. Janky (forall i. Eff '[State s] ())

jankyState :: Janky
jankyState = Janky $ put True

unsafeUnjank :: Janky -> Eff '[State Bool] ()
unsafeUnjank (Janky sem) = unsafeCoerce sem


spec :: Spec
spec = do
  describe "State effect" $ do
    describe "get/put" $ do
      it "should work in simple cases" $ do
        flipShouldBe ((), True) . runPureEff $ runState True idState

      it "should, when polymorphic, eliminate the first matching effect" $ do
        flipShouldBe (((), True), False)   . runPureEff $ runState False $ runState True idState

      it "should, when polymorphic, not eliminate unmatching effects" $ do
        flipShouldBe (Right @Int (), True) . runPureEff $ runState True $ runErrorNoCallStack idState

    describe "numbers" $ do
      it "should interpret against concrete Int" $ do
        flipShouldBe ((), 10) . runPureEff $ runState 0 intState

      describe "polymorphic Num constraint" $ do
        it "should interpret against Int" $ do
          flipShouldBe ((), 10 :: Int)     . runPureEff $ runState 0 numState

        it "should interpret against Float" $ do
          flipShouldBe ((), 10 :: Float)   . runPureEff $ runState 0 numState

        it "should interpret against Double" $ do
          flipShouldBe ((), 10 :: Double)  . runPureEff $ runState 0 numState

        it "should interpret against Integer" $ do
          flipShouldBe ((), 10 :: Integer) . runPureEff $ runState 0 numState

    describe "strings" $ do
      it "concrete interpret against concrete String" $ do
        flipShouldBe ((), "hello") . runPureEff $ runState "nothing" strState

      describe "polymorphic IsString constraint" $ do
        it "should interpret against String" $ do
          flipShouldBe ((), "hello" :: String)   . runPureEff $ runState "nothing" oStrState

        it "should interpret against MyString" $ do
          flipShouldBe ((), "hello" :: MyString) . runPureEff $ runState "nothing" oStrState

    describe "existential state" $ do
      it "JankyState should compile" $ do
        flipShouldBe ((), True) . runPureEff $ runState False $ unsafeUnjank jankyState


  describe "Error effect" $ do
    it "should interpret against Int" $ do
      flipShouldBe (Right @Int True)  . runPureEff $ runErrorNoCallStack err
    it "should interpret against Bool" $ do
      flipShouldBe (Right @Bool True) . runPureEff $ runErrorNoCallStack err


  describe "State/Error effect" $ do
    it "should interpret against Int/String" $ do
      flipShouldBe (Right @String True, 10 :: Int)  . runPureEff $ runState 0 $ runErrorNoCallStack errState
    it "should interpret against Float/Bool" $ do
      flipShouldBe (Right @Bool True, 10 :: Float)  . runPureEff $ runState 0 $ runErrorNoCallStack errState


  describe "Error/State effect" $ do
    it "should interpret against String/Int" $ do
      flipShouldBe (Right @String (True, 10 :: Int))  . runPureEff $ runErrorNoCallStack $ runState 0 errState
    it "should interpret against Bool/Float" $ do
      flipShouldBe (Right @Bool (True, 10 :: Float))  . runPureEff $ runErrorNoCallStack $ runState 0 errState

      
  -- describe "Embed effect" $ do
  --   it "should interpret against IO" $ do
  --     res <- runM lifted
  --     res `shouldBe` ()

  --   it "should interpret against Identity" $ do
  --     let res = runM lifted
  --     res `shouldBe` Identity ()


flipShouldBe :: (Show a, Eq a) => a -> a -> Expectation
flipShouldBe = flip shouldBe

