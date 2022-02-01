{-# OPTIONS_GHC -fdefer-type-errors -fno-warn-deferred-type-errors #-}

module LegitimateTypeErrorSpec where

import Effectful
import Effectful.Dispatch.Dynamic
import Test.Hspec
import Test.ShouldNotTypecheck

wrongEmbed :: IOE :> es => Eff es ()
wrongEmbed = liftIO putStrLn

wrongReturn :: Eff (e ': es) () -> Eff es ()
wrongReturn = reinterpret undefined



spec :: Spec
spec = do
  describe "Legitimate type errors" $ do
    it "should be caused by `embed`ing an unsaturated function" $
        shouldNotTypecheck wrongEmbed

    it "should be caused by giving a bad type to reinterpret" $
        shouldNotTypecheck wrongReturn

