module Test.Data.Address.Bech32.DataPart.CodePoint (spec) where

import Prologue

import Arbitrary (ArbT(..))
import Data.Address.Bech32.DataPart.CodePoint
  ( DataPartCodePoint
  , fromByte
  , fromCodePoint
  , toByte
  , toCodePoint
  )
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
  describe "Data.Address.Bech32.DataPart.CodePoint" do
    it "Satisfies Eq laws" do
      liftEffect $ checkEq (Proxy :: _ (ArbT DataPartCodePoint))
    it "Satisfies Ord laws" do
      liftEffect $ checkOrd (Proxy :: _ (ArbT DataPartCodePoint))
    it "Satisfies Bounded laws" do
      liftEffect $ checkBounded (Proxy :: _ (ArbT DataPartCodePoint))
    it "Satisfies BoundedEnum laws" do
      liftEffect $ checkBoundedEnum (Proxy :: _ (ArbT DataPartCodePoint))
    describe "CodePoint injectivity" do
      it "preserves round trips" do
        quickCheck \(ArbT cp) -> fromCodePoint (toCodePoint cp) === Just cp
    describe "Byte injectivity" do
      it "preserves round trips" do
        quickCheck \(ArbT cp) ->
          Tuple (fromByte (toByte cp)) (toByte cp `mod` 8) === Tuple (Just cp)
            (toByte cp `mod` 8)
