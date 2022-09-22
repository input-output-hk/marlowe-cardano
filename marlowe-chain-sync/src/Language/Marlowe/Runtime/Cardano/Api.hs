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
import Data.Bifunctor (Bifunctor(bimap))
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

class CardanoFeature (CardanoFeatureType a) => HasCardanoTypeWithEra a f | a -> f where
  type CardanoTypeWithEra a :: Type -> Type
  type CardanoFeatureType a :: Type -> Type
  toCardanoInEra
    :: CardanoFeatureType a era
    -> a
    -> UnwrapIdentity (f (CardanoTypeWithEra a era))
  fromCardanoInEra
    :: CardanoFeatureType a era
    -> CardanoTypeWithEra a era
    -> a

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

instance HasCardanoType PaymentKeyHash Maybe where
  type CardanoType PaymentKeyHash = C.Hash C.PaymentKey
  toCardano = C.deserialiseFromRawBytes (C.AsHash C.AsPaymentKey) . unPaymentKeyHash
  fromCardano = PaymentKeyHash . C.serialiseToRawBytes

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

instance HasCardanoType TxOutRef Maybe where
  type CardanoType TxOutRef = C.TxIn
  toCardano TxOutRef{..} = C.TxIn <$> toCardano txId <*> pure (toCardano txIx)
  fromCardano (C.TxIn txId txIx) = TxOutRef (fromCardano txId) (fromCardano txIx)

instance HasCardanoTypeWithEra Address Maybe where
  type CardanoTypeWithEra Address = C.AddressInEra
  type CardanoFeatureType Address = C.CardanoEra
  toCardanoInEra era = withCardanoEra era
    $ C.deserialiseFromRawBytes (C.AsAddressInEra $ cardanoEraToAsType era) . unAddress
  fromCardanoInEra era = withCardanoEra era
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

instance HasCardanoTypeWithEra Assets Maybe where
  type CardanoTypeWithEra Assets = C.TxOutValue
  type CardanoFeatureType Assets = C.MultiAssetSupportedInEra
  toCardanoInEra era assets = C.TxOutValue era <$> toCardano assets
  fromCardanoInEra era = \case
    C.TxOutValue _ value -> fromCardano value
    C.TxOutAdaOnly era' _ -> case (era, era') of

instance HasCardanoTypeWithEra (Maybe DatumHash, Maybe Datum) Maybe where
  type CardanoTypeWithEra (Maybe DatumHash, Maybe Datum) = C.TxOutDatum C.CtxTx
  type CardanoFeatureType (Maybe DatumHash, Maybe Datum) = C.CardanoEra
  toCardanoInEra era = case featureInCardanoEra era of
    Nothing -> const $ Just C.TxOutDatumNone
    Just scriptDataSupported -> \case
      (Nothing, Nothing) -> Just C.TxOutDatumNone
      (Just hash, Nothing) -> C.TxOutDatumHash scriptDataSupported <$> toCardano hash
      (_, Just datum) -> Just $ C.TxOutDatumInTx scriptDataSupported $ toCardano datum
  fromCardanoInEra _ = \case
    C.TxOutDatumNone -> (Nothing, Nothing)
    C.TxOutDatumHash _ hash -> (Just $ fromCardano hash, Nothing)
    C.TxOutDatumInTx _ datum -> (Nothing, Just $ fromCardano datum)
    C.TxOutDatumInline _ datum -> (Nothing, Just $ fromCardano datum)

instance HasCardanoTypeWithEra TransactionOutput Maybe where
  type CardanoTypeWithEra TransactionOutput = C.TxOut C.CtxTx
  type CardanoFeatureType TransactionOutput = C.MultiAssetSupportedInEra
  toCardanoInEra era TransactionOutput{..} = C.TxOut
    <$> toCardanoInEra (cardanoEraOfFeature era) address
    <*> toCardanoInEra era assets
    <*> toCardanoInEra (cardanoEraOfFeature era) (datumHash, datum)
    <*> pure C.ReferenceScriptNone

  fromCardanoInEra era (C.TxOut address value txOutDatum _) =
    TransactionOutput
      (fromCardanoInEra (cardanoEraOfFeature era) address)
      (fromCardanoInEra era value)
      hash
      datum
    where
      (hash, datum) = fromCardanoInEra (cardanoEraOfFeature era) txOutDatum

cardanoEraToAsType :: C.CardanoEra era -> C.AsType era
cardanoEraToAsType = \case
  C.ByronEra -> C.AsByronEra
  C.ShelleyEra -> C.AsShelleyEra
  C.AllegraEra -> C.AsAllegraEra
  C.MaryEra -> C.AsMaryEra
  C.AlonzoEra -> C.AsAlonzoEra
  C.BabbageEra -> C.AsBabbageEra
