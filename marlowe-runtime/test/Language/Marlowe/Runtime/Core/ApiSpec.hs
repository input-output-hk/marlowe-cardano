module Language.Marlowe.Runtime.Core.ApiSpec
  ( spec
  ) where

import Cardano.Api (serialiseToRawBytes)
import qualified Cardano.Api as C
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Language.Marlowe.Runtime.ChainSync.Api (Address(..), ScriptHash(..))
import Language.Marlowe.Runtime.Core.Api
  ( AddressInNetwork(..)
  , MarloweScriptAddresses(..)
  , MarloweVersion
  , NetworkType(..)
  , ScriptAddressInfo(..)
  , getCurrentScriptAddresses
  , getScriptAddressSet
  , withSomeMarloweVersion
  )
import Language.Marlowe.Scripts (marloweValidatorHash, rolePayoutValidatorHash)
import Plutus.V1.Ledger.Api (ValidatorHash(..), fromBuiltin)
import Test.Hspec (Spec, describe, expectationFailure, focus, it, shouldBe)

spec :: Spec
spec = focus do
  describe "Address Sets" $ traverse_ (withSomeMarloweVersion addressSetSpec) [minBound..maxBound]

addressSetSpec :: MarloweVersion v -> Spec
addressSetSpec marloweVersion = do
  describe (show marloweVersion) do
    let currentAddresses = getCurrentScriptAddresses marloweVersion
    let addressSet = getScriptAddressSet marloweVersion
    it "Contains the current script address in its address set." do
      unless (Set.member currentAddresses addressSet) do
        expectationFailure $ unwords
          ["Expected the address set to contain"
          , show currentAddresses
          , "but it does not."
          ]
    it "Should specify the correct current script addresses" do
      let payoutScriptAddress = toAddressInNetwork rolePayoutValidatorHash
      let payoutScriptHash = fromPlutusValidatorHash rolePayoutValidatorHash
      let marloweScriptAddress = toAddressInNetwork marloweValidatorHash
      let marloweScriptHash = fromPlutusValidatorHash marloweValidatorHash
      let payoutScriptAddressInfo = ScriptAddressInfo payoutScriptAddress payoutScriptHash
      let marloweScriptAddressInfo = ScriptAddressInfo marloweScriptAddress marloweScriptHash
      let currentAddresses' = MarloweScriptAddresses marloweScriptAddressInfo payoutScriptAddressInfo
      currentAddresses `shouldBe` currentAddresses'

fromPlutusValidatorHash :: ValidatorHash -> ScriptHash
fromPlutusValidatorHash (ValidatorHash hash) = ScriptHash $ fromBuiltin hash

toAddressInNetwork :: ValidatorHash -> AddressInNetwork
toAddressInNetwork hash = AddressInNetwork $ Address . serialiseToRawBytes . toShelleyAddress hash

toShelleyAddress :: ValidatorHash -> NetworkType -> C.Address C.ShelleyAddr
toShelleyAddress (ValidatorHash hash) networkType = C.makeShelleyAddress
  case networkType of
    Mainnet -> C.Mainnet
    Testnet -> C.Testnet $ C.NetworkMagic 2 -- the magic is irrelevant
  (C.PaymentCredentialByScript $ fromJust $ C.deserialiseFromRawBytes C.AsScriptHash $ fromBuiltin hash)
  C.NoStakeAddress
