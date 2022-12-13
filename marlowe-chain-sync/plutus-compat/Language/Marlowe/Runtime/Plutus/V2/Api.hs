{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Language.Marlowe.Runtime.Plutus.V2.Api
  ( fromPlutusCurrencySymbol
  , fromPlutusScript
  , fromPlutusTokenName
  , fromPlutusTxOutRef
  , fromPlutusValidatorHash
  , fromPlutusValue
  , toAssetId
  , toPlutusAddress
  , toPlutusCurrencySymbol
  , toPlutusScript
  , toPlutusTokenName
  , toPlutusTxOutRef
  , toPlutusValidatorHash
  ) where

import qualified Cardano.Api as C
import qualified Cardano.Api.Byron as C
import qualified Cardano.Api.Shelley as C
import Cardano.Chain.Common (addrToBase58)
import Codec.Serialise (deserialise, serialise)
import Control.Monad ((<=<), (>=>))
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import qualified Data.Map as Map
import Language.Marlowe.Runtime.Cardano.Api (toCardanoAddressAny, toCardanoPlutusScript)
import Language.Marlowe.Runtime.ChainSync.Api
  ( Address
  , AssetId(..)
  , Assets(..)
  , Lovelace(..)
  , PlutusScript(..)
  , PolicyId(PolicyId)
  , ScriptHash(..)
  , TokenName(TokenName)
  , Tokens(..)
  , TxId(TxId)
  , TxIx(TxIx)
  , TxOutRef(TxOutRef)
  )
import qualified Plutus.V2.Ledger.Api as PV2
import qualified PlutusTx.AssocMap as AM

toPlutusAddress :: Address -> Maybe PV2.Address
toPlutusAddress = toCardanoAddressAny >=> \case
    C.AddressByron address -> fromCardanoAddress address
    C.AddressShelley address -> fromCardanoAddress address
  where
    fromCardanoAddress :: forall addrtype. C.Address addrtype -> Maybe PV2.Address
    fromCardanoAddress (C.ByronAddress address) =
        Just $ PV2.Address plutusCredential Nothing
        where
          plutusCredential :: PV2.Credential
          plutusCredential =
              PV2.PubKeyCredential
            . PV2.PubKeyHash
            . PV2.toBuiltin
            . addrToBase58
            $ address
    fromCardanoAddress (C.ShelleyAddress _ paymentCredential stakeAddressReference) =
        PV2.Address
            (fromCardanoPaymentCredential (C.fromShelleyPaymentCredential paymentCredential))
            <$> fromCardanoStakeAddressReference (C.fromShelleyStakeReference stakeAddressReference)

    fromCardanoPaymentCredential :: C.PaymentCredential -> PV2.Credential
    fromCardanoPaymentCredential (C.PaymentCredentialByKey paymentKeyHash) = PV2.PubKeyCredential (fromCardanoKeyHash paymentKeyHash)
    fromCardanoPaymentCredential (C.PaymentCredentialByScript scriptHash) = PV2.ScriptCredential (fromCardanoScriptHash scriptHash)

    fromCardanoStakeAddressReference :: C.StakeAddressReference -> Maybe (Maybe PV2.StakingCredential)
    fromCardanoStakeAddressReference C.NoStakeAddress = pure Nothing
    fromCardanoStakeAddressReference (C.StakeAddressByValue stakeCredential) =
        pure $ Just (PV2.StakingHash $ fromCardanoStakeCredential stakeCredential)
    fromCardanoStakeAddressReference C.StakeAddressByPointer{} = pure Nothing

    fromCardanoStakeCredential :: C.StakeCredential -> PV2.Credential
    fromCardanoStakeCredential (C.StakeCredentialByKey stakeKeyHash) = PV2.PubKeyCredential (fromCardanoKeyHash stakeKeyHash)
    fromCardanoStakeCredential (C.StakeCredentialByScript scriptHash) = PV2.ScriptCredential (fromCardanoScriptHash scriptHash)

    fromCardanoKeyHash :: C.SerialiseAsRawBytes (C.Hash keyRole) => C.Hash keyRole -> PV2.PubKeyHash
    fromCardanoKeyHash keyHash = PV2.PubKeyHash $ PV2.toBuiltin $ C.serialiseToRawBytes keyHash

    fromCardanoScriptHash :: C.ScriptHash -> PV2.ValidatorHash
    fromCardanoScriptHash scriptHash = PV2.ValidatorHash $ PV2.toBuiltin $ C.serialiseToRawBytes scriptHash

fromPlutusValidatorHash :: PV2.ValidatorHash -> ScriptHash
fromPlutusValidatorHash (PV2.ValidatorHash h) = ScriptHash . PV2.fromBuiltin $ h

toPlutusValidatorHash :: ScriptHash -> PV2.ValidatorHash
toPlutusValidatorHash (ScriptHash h) = PV2.ValidatorHash . PV2.toBuiltin $ h

fromPlutusScript :: PV2.Script -> PlutusScript
fromPlutusScript =
  PlutusScript
  . C.serialiseToRawBytes
  . (C.PlutusScriptSerialised :: ShortByteString -> C.PlutusScript C.PlutusScriptV2)
  . toShort
  . toStrict
  . serialise

toPlutusScript :: PlutusScript -> Maybe PV2.Script
toPlutusScript ps = do
  (C.PlutusScriptSerialised script :: C.PlutusScript C.PlutusScriptV2) <- toCardanoPlutusScript ps
  deserialise . fromStrict . fromShort $ script

fromPlutusTxOutRef :: PV2.TxOutRef -> TxOutRef
fromPlutusTxOutRef (PV2.TxOutRef (PV2.TxId txId) txIx) = TxOutRef (TxId . PV2.fromBuiltin $ txId) (TxIx . fromInteger $ txIx)

toPlutusTxOutRef :: TxOutRef -> PV2.TxOutRef
toPlutusTxOutRef (TxOutRef (TxId txId) (TxIx txIx)) = PV2.TxOutRef (PV2.TxId . PV2.toBuiltin $ txId) (toInteger txIx)

toPlutusTokenName :: TokenName -> PV2.TokenName
toPlutusTokenName (TokenName bs) = PV2.TokenName . PV2.toBuiltin $ bs

fromPlutusTokenName :: PV2.TokenName -> TokenName
fromPlutusTokenName (PV2.TokenName bs) = TokenName . PV2.fromBuiltin $ bs

toPlutusCurrencySymbol :: PolicyId -> PV2.CurrencySymbol
toPlutusCurrencySymbol (PolicyId bs) = PV2.CurrencySymbol . PV2.toBuiltin $ bs

fromPlutusCurrencySymbol :: PV2.CurrencySymbol -> PolicyId
fromPlutusCurrencySymbol (PV2.CurrencySymbol bs) = PolicyId . PV2.fromBuiltin $ bs

fromPlutusValue :: PV2.Value -> Assets
fromPlutusValue = Assets <$> valueToLovelace <*> valueToTokens

valueToLovelace :: PV2.Value -> Lovelace
valueToLovelace = Lovelace . maybe 0 fromIntegral . (AM.lookup "" <=< AM.lookup "") . PV2.getValue

valueToTokens :: PV2.Value -> Tokens
valueToTokens = Tokens
  . Map.fromList
  . fmap
      ( bimap (uncurry toAssetId) fromIntegral
      . assocLeft
      )
  . (traverse AM.toList <=< AM.toList)
  . AM.delete ""
  . PV2.getValue
  where
  assocLeft (a, (b, c)) = ((a, b), c)

toAssetId :: PV2.CurrencySymbol -> PV2.TokenName -> AssetId
toAssetId cs role =
  let
    policyId = fromPlutusCurrencySymbol cs
    tokenName = fromPlutusTokenName role
   in AssetId policyId tokenName
