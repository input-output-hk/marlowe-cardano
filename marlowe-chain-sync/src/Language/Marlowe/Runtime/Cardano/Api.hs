{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Utilities for converting to and from Cardano API types.

module Language.Marlowe.Runtime.Cardano.Api
  where

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import qualified Cardano.Ledger.BaseTypes as L
import Cardano.Ledger.Credential (Ptr(Ptr))
import Data.Bifunctor (Bifunctor(bimap))
import Data.ByteString.Short (fromShort, toShort)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import qualified Data.Map as Map
import Language.Marlowe.Runtime.Cardano.Feature
import Language.Marlowe.Runtime.ChainSync.Api
import Witherable (mapMaybe)

type family UnwrapIdentity a where
  UnwrapIdentity (Identity a) = a
  UnwrapIdentity a = a

class HasCardanoType a f | a -> f where
  type CardanoType a :: Type
  toCardano :: a -> UnwrapIdentity (f (CardanoType a))
  fromCardano :: CardanoType a -> a

class CardanoFeature (FeatureType a) => HasFeatureDependentCardanoType a f | a -> f where
  type FeatureDependentCardanoType a :: Type -> Type
  type FeatureType a :: Type -> Type
  toCardanoFeatureDependent
    :: FeatureType a era
    -> a
    -> UnwrapIdentity (f (FeatureDependentCardanoType a era))
  fromCardanoFeatureDependent
    :: FeatureType a era
    -> FeatureDependentCardanoType a era
    -> a

instance HasCardanoType BlockHeader Identity where
  type CardanoType BlockHeader = C.BlockHeader
  toCardano BlockHeader{..} = C.BlockHeader (toCardano slotNo) (toCardano headerHash) (toCardano blockNo)
  fromCardano (C.BlockHeader slotNo headerHash blockNo) =
    BlockHeader (fromCardano slotNo) (fromCardano headerHash) (fromCardano blockNo)

instance HasCardanoType SlotNo Identity where
  type CardanoType SlotNo = C.SlotNo
  toCardano = C.SlotNo . fromIntegral
  fromCardano (C.SlotNo slotNo) = fromIntegral slotNo

instance HasCardanoType BlockNo Identity where
  type CardanoType BlockNo = C.BlockNo
  toCardano = C.BlockNo . fromIntegral
  fromCardano (C.BlockNo blockNo) = fromIntegral blockNo

instance HasCardanoType BlockHeaderHash Identity where
  type CardanoType BlockHeaderHash = C.Hash C.BlockHeader
  toCardano = C.HeaderHash . toShort . unBlockHeaderHash
  fromCardano (C.HeaderHash hash) = BlockHeaderHash $ fromShort hash

instance HasCardanoType ScriptHash Maybe where
  type CardanoType ScriptHash = C.ScriptHash
  toCardano = C.deserialiseFromRawBytes C.AsScriptHash . unScriptHash
  fromCardano = ScriptHash . C.serialiseToRawBytes

instance HasCardanoType DatumHash Maybe where
  type CardanoType DatumHash = C.Hash C.ScriptData
  toCardano = C.deserialiseFromRawBytes (C.AsHash C.AsScriptData) . unDatumHash
  fromCardano = DatumHash . C.serialiseToRawBytes

instance HasCardanoType Datum Identity where
  type CardanoType Datum = C.ScriptData
  toCardano = \case
    Constr i ds -> C.ScriptDataConstructor i $ toCardano <$> ds
    Map ds -> C.ScriptDataMap $ bimap toCardano toCardano <$> ds
    List ds -> C.ScriptDataList $ toCardano <$> ds
    I i -> C.ScriptDataNumber i
    B bs -> C.ScriptDataBytes bs
  fromCardano = \case
    C.ScriptDataConstructor i ds -> Constr i $ fromCardano <$> ds
    C.ScriptDataMap ds -> Map $ bimap fromCardano fromCardano <$> ds
    C.ScriptDataList ds -> List $ fromCardano <$> ds
    C.ScriptDataNumber i -> I i
    C.ScriptDataBytes bs -> B bs

instance HasCardanoType Credential Maybe where
  type CardanoType Credential = C.PaymentCredential
  toCardano = \case
    PaymentKeyCredential pkh -> C.PaymentCredentialByKey <$> toCardano pkh
    ScriptCredential sh -> C.PaymentCredentialByScript <$> toCardano sh
  fromCardano = \case
    C.PaymentCredentialByKey pkh -> PaymentKeyCredential $ fromCardano pkh
    C.PaymentCredentialByScript sh -> ScriptCredential $ fromCardano sh

instance HasCardanoType StakeCredential Maybe where
  type CardanoType StakeCredential = C.StakeCredential
  toCardano = \case
    StakeKeyCredential skh -> C.StakeCredentialByKey <$> toCardano skh
    StakeScriptCredential sh -> C.StakeCredentialByScript <$> toCardano sh
  fromCardano = \case
    C.StakeCredentialByKey skh -> StakeKeyCredential $ fromCardano skh
    C.StakeCredentialByScript sh -> StakeScriptCredential $ fromCardano sh

instance HasCardanoType (Maybe StakeReference) Maybe where
  type CardanoType (Maybe StakeReference) = C.StakeAddressReference
  toCardano = \case
    Nothing -> Just C.NoStakeAddress
    Just (StakeCredential cred) -> C.StakeAddressByValue <$> toCardano cred
    Just (StakePointer slotNo txIx certIx) -> Just
      $ C.StakeAddressByPointer
      $ C.StakeAddressPointer
      $ Ptr (toCardano slotNo) (L.TxIx $ fromIntegral txIx) (toCardano certIx)
  fromCardano = \case
    C.NoStakeAddress -> Nothing
    C.StakeAddressByValue credential -> Just $ StakeCredential $ fromCardano credential
    C.StakeAddressByPointer (C.StakeAddressPointer (Ptr slotNo (L.TxIx txIx) certIx)) -> Just $ StakePointer
      (fromCardano slotNo)
      (fromIntegral txIx)
      (fromCardano certIx)

instance HasCardanoType PaymentKeyHash Maybe where
  type CardanoType PaymentKeyHash = C.Hash C.PaymentKey
  toCardano = C.deserialiseFromRawBytes (C.AsHash C.AsPaymentKey) . unPaymentKeyHash
  fromCardano = PaymentKeyHash . C.serialiseToRawBytes

instance HasCardanoType StakeKeyHash Maybe where
  type CardanoType StakeKeyHash = C.Hash C.StakeKey
  toCardano = C.deserialiseFromRawBytes (C.AsHash C.AsStakeKey) . unStakeKeyHash
  fromCardano = StakeKeyHash . C.serialiseToRawBytes

instance HasCardanoType PolicyId Maybe where
  type CardanoType PolicyId = C.PolicyId
  toCardano = C.deserialiseFromRawBytes C.AsPolicyId . unPolicyId
  fromCardano = PolicyId . C.serialiseToRawBytes

instance HasCardanoType TokenName Identity where
  type CardanoType TokenName = C.AssetName
  toCardano = C.AssetName . unTokenName
  fromCardano (C.AssetName name) = TokenName name

instance HasCardanoType Quantity Identity where
  type CardanoType Quantity = C.Quantity
  toCardano = C.Quantity . fromIntegral . unQuantity
  fromCardano (C.Quantity q) = Quantity $ fromIntegral q

instance HasCardanoType TxId Maybe where
  type CardanoType TxId = C.TxId
  toCardano = C.deserialiseFromRawBytes C.AsTxId . unTxId
  fromCardano = TxId . C.serialiseToRawBytes

instance HasCardanoType TxIx Identity where
  type CardanoType TxIx = C.TxIx
  toCardano = C.TxIx . fromIntegral . unTxIx
  fromCardano (C.TxIx txIx) = TxIx $ fromIntegral txIx

instance HasCardanoType CertIx Identity where
  type CardanoType CertIx = L.CertIx
  toCardano = L.CertIx . unCertIx
  fromCardano (L.CertIx certIx) = CertIx certIx

instance HasCardanoType TxOutRef Maybe where
  type CardanoType TxOutRef = C.TxIn
  toCardano TxOutRef{..} = C.TxIn <$> toCardano txId <*> pure (toCardano txIx)
  fromCardano (C.TxIn txId txIx) = TxOutRef (fromCardano txId) (fromCardano txIx)

instance HasFeatureDependentCardanoType Address Maybe where
  type FeatureDependentCardanoType Address = C.AddressInEra
  type FeatureType Address = C.CardanoEra
  toCardanoFeatureDependent era = withCardanoEra era
    $ C.deserialiseFromRawBytes (C.AsAddressInEra $ cardanoEraToAsType era) . unAddress
  fromCardanoFeatureDependent era = withCardanoEra era
    $ Address . C.serialiseToRawBytes

instance HasCardanoType Lovelace Identity where
  type CardanoType Lovelace = C.Lovelace
  toCardano = C.Lovelace . fromIntegral . unLovelace
  fromCardano (C.Lovelace l) = Lovelace $ fromIntegral l

instance HasCardanoType Tokens Maybe where
  type CardanoType Tokens = C.Value
  toCardano = fmap C.valueFromList . traverse toCardano' . Map.toList . unTokens
    where
      toCardano' :: (AssetId, Quantity) -> Maybe (C.AssetId, C.Quantity)
      toCardano' (AssetId{..}, quantity) = (,)
        <$> (C.AssetId <$> toCardano policyId <*> pure (toCardano tokenName))
        <*> pure (toCardano quantity)
  fromCardano = Tokens . Map.fromList . mapMaybe fromCardano' . C.valueToList
    where
      fromCardano' :: (C.AssetId, C.Quantity) -> Maybe (AssetId, Quantity)
      fromCardano' (C.AdaAssetId, _) = Nothing
      fromCardano' (C.AssetId policy name, q) =
        Just (AssetId (fromCardano policy) (fromCardano name), fromCardano q)

instance HasCardanoType Assets Maybe where
  type CardanoType Assets = C.Value
  toCardano Assets{..} = fold <$> sequence
    [ Just $ C.valueFromList [(C.AdaAssetId, C.lovelaceToQuantity $ toCardano ada)]
    , toCardano tokens
    ]
  fromCardano value = Assets
    { ada = maybe 0 fromCardano $ C.valueToLovelace value
    , tokens = fromCardano value
    }

instance HasFeatureDependentCardanoType Assets Maybe where
  type FeatureDependentCardanoType Assets = C.TxOutValue
  type FeatureType Assets = C.MultiAssetSupportedInEra
  toCardanoFeatureDependent era assets = C.TxOutValue era <$> toCardano assets
  fromCardanoFeatureDependent era = \case
    C.TxOutValue _ value -> fromCardano value
    C.TxOutAdaOnly era' _ -> case (era, era') of

instance HasFeatureDependentCardanoType (Maybe DatumHash, Maybe Datum) Maybe where
  type FeatureDependentCardanoType (Maybe DatumHash, Maybe Datum) = C.TxOutDatum C.CtxTx
  type FeatureType (Maybe DatumHash, Maybe Datum) = C.CardanoEra
  toCardanoFeatureDependent era = case featureInCardanoEra era of
    Nothing -> const $ Just C.TxOutDatumNone
    Just scriptDataSupported -> \case
      (Nothing, Nothing) -> Just C.TxOutDatumNone
      (Just hash, Nothing) -> C.TxOutDatumHash scriptDataSupported <$> toCardano hash
      (_, Just datum) -> Just $ C.TxOutDatumInTx scriptDataSupported $ toCardano datum
  fromCardanoFeatureDependent _ = \case
    C.TxOutDatumNone -> (Nothing, Nothing)
    C.TxOutDatumHash _ hash -> (Just $ fromCardano hash, Nothing)
    C.TxOutDatumInTx _ datum -> (Nothing, Just $ fromCardano datum)
    C.TxOutDatumInline _ datum -> (Nothing, Just $ fromCardano datum)

instance HasFeatureDependentCardanoType TransactionOutput Maybe where
  type FeatureDependentCardanoType TransactionOutput = C.TxOut C.CtxTx
  type FeatureType TransactionOutput = C.MultiAssetSupportedInEra
  toCardanoFeatureDependent era TransactionOutput{..} = C.TxOut
    <$> toCardanoFeatureDependent (cardanoEraOfFeature era) address
    <*> toCardanoFeatureDependent era assets
    <*> toCardanoFeatureDependent (cardanoEraOfFeature era) (datumHash, datum)
    <*> pure C.ReferenceScriptNone

  fromCardanoFeatureDependent era (C.TxOut address value txOutDatum _) =
    TransactionOutput
      (fromCardanoFeatureDependent (cardanoEraOfFeature era) address)
      (fromCardanoFeatureDependent era value)
      hash
      datum
    where
      (hash, datum) = fromCardanoFeatureDependent (cardanoEraOfFeature era) txOutDatum

cardanoEraToAsType :: C.CardanoEra era -> C.AsType era
cardanoEraToAsType = \case
  C.ByronEra -> C.AsByronEra
  C.ShelleyEra -> C.AsShelleyEra
  C.AllegraEra -> C.AsAllegraEra
  C.MaryEra -> C.AsMaryEra
  C.AlonzoEra -> C.AsAlonzoEra
  C.BabbageEra -> C.AsBabbageEra
