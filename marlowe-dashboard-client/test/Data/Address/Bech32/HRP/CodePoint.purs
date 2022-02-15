module Test.Data.Address.Bech32.HRP.CodePoint (spec) where

import Prologue

import Arbitrary (ArbT(..))
import Data.Address.Bech32.HRP.CodePoint
  ( HRPCodePoint
  , fromCodePoint
  , isValid
  , toCodePoint
  )
import Data.Newtype (un)
import Effect.Class (liftEffect)
import Test.QuickCheck ((===))
import Test.QuickCheck.Laws.Data
  ( checkBounded
  , checkBoundedEnum
  , checkEq
  , checkOrd
  )
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = do
  describe "Data.Address.Bech32.HRP.CodePoint" do
    it "Satisfies Eq laws" do
      liftEffect $ checkEq (Proxy :: _ (ArbT HRPCodePoint))
    it "Satisfies Ord laws" do
      liftEffect $ checkOrd (Proxy :: _ (ArbT HRPCodePoint))
    it "Satisfies Bounded laws" do
      liftEffect $ checkBounded (Proxy :: _ (ArbT HRPCodePoint))
    it "Satisfies BoundedEnum laws" do
      liftEffect $ checkBoundedEnum (Proxy :: _ (ArbT HRPCodePoint))
    describe "Arbitrary instance" do
      it "generates valid data" do
        quickCheck $ isValid <<< un ArbT
    describe "CodePoint injectivity" do
      it "preserves round trips" do
        quickCheck \(ArbT cp) -> fromCodePoint (toCodePoint cp) === Just cp
