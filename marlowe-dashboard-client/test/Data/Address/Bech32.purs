module Test.Data.Address.Bech32 (spec) where

import Prologue

import Arbitrary (ArbT(..))
import Data.Address.Bech32 (Bech32Address, fromString, toString)
import Effect.Class (liftEffect)
import Test.QuickCheck ((===))
import Test.QuickCheck.Laws.Data (checkEq, checkOrd)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = do
  describe "Data.Address.Bech32" do
    it "Satisfies Eq laws" do
      liftEffect $ checkEq (Proxy :: _ (ArbT Bech32Address))
    it "Satisfies Ord laws" do
      liftEffect $ checkOrd (Proxy :: _ (ArbT Bech32Address))
    describe "String injectivity" do
      it "preserves round trips" do
        quickCheck \(ArbT hrp) -> fromString (toString hrp) === Just hrp
