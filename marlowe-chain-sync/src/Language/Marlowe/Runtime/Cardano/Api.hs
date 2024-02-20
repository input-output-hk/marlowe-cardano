{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Utilities for converting to and from Cardano API types.
module Language.Marlowe.Runtime.Cardano.Api where

import Cardano.Api (getScriptData, unsafeHashableScriptData)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import qualified Cardano.Ledger.BaseTypes as L
import Cardano.Ledger.Credential (Ptr (Ptr))
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString.Short (fromShort, toShort)
import Data.Data (Proxy (Proxy))
import Data.Foldable (fold)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShow)
import GHC.Base (Alternative ((<|>)))
import Language.Marlowe.Runtime.Cardano.Feature
import Language.Marlowe.Runtime.ChainSync.Api

toCardanoBlockHeader :: BlockHeader -> C.BlockHeader
toCardanoBlockHeader BlockHeader{..} =
  C.BlockHeader
    (toCardanoSlotNo slotNo)
    (toCardanoBlockHeaderHash headerHash)
    (toCardanoBlockNo blockNo)

fromCardanoBlockHeader :: C.BlockHeader -> BlockHeader
fromCardanoBlockHeader (C.BlockHeader slotNo headerHash blockNo) =
  BlockHeader
    (fromCardanoSlotNo slotNo)
    (fromCardanoBlockHeaderHash headerHash)
    (fromCardanoBlockNo blockNo)

toCardanoSlotNo :: SlotNo -> C.SlotNo
toCardanoSlotNo = C.SlotNo . fromIntegral

fromCardanoSlotNo :: C.SlotNo -> SlotNo
fromCardanoSlotNo (C.SlotNo slotNo) = fromIntegral slotNo

toCardanoBlockNo :: BlockNo -> C.BlockNo
toCardanoBlockNo = C.BlockNo . fromIntegral

fromCardanoBlockNo :: C.BlockNo -> BlockNo
fromCardanoBlockNo (C.BlockNo slotNo) = fromIntegral slotNo

toCardanoBlockHeaderHash :: BlockHeaderHash -> C.Hash C.BlockHeader
toCardanoBlockHeaderHash = C.HeaderHash . toShort . unBlockHeaderHash

fromCardanoBlockHeaderHash :: C.Hash C.BlockHeader -> BlockHeaderHash
fromCardanoBlockHeaderHash (C.HeaderHash hash) = BlockHeaderHash $ fromShort hash

toCardanoScriptHash :: ScriptHash -> Maybe C.ScriptHash
toCardanoScriptHash = hush . C.deserialiseFromRawBytes C.AsScriptHash . unScriptHash

toCardanoPlutusScript :: forall lang. (C.HasTypeProxy lang) => PlutusScript -> Maybe (C.PlutusScript lang)
toCardanoPlutusScript = hush . C.deserialiseFromRawBytes (C.proxyToAsType (Proxy :: Proxy (C.PlutusScript lang))) . unPlutusScript

fromCardanoPlutusScript :: forall lang. (C.HasTypeProxy lang) => C.PlutusScript lang -> PlutusScript
fromCardanoPlutusScript = PlutusScript . C.serialiseToRawBytes

plutusScriptHash :: PlutusScript -> Maybe ScriptHash
plutusScriptHash ps = hashPlutusScript C.PlutusScriptV2 <|> hashPlutusScript C.PlutusScriptV1
  where
    hashPlutusScript :: (C.HasTypeProxy lang) => C.PlutusScriptVersion lang -> Maybe ScriptHash
    hashPlutusScript pv = do
      script <- C.PlutusScript pv <$> toCardanoPlutusScript ps
      pure . fromCardanoScriptHash . C.hashScript $ script

toCardanoDatumHash :: DatumHash -> Maybe (C.Hash C.ScriptData)
toCardanoDatumHash = hush . C.deserialiseFromRawBytes (C.AsHash C.AsScriptData) . unDatumHash

fromCardanoDatumHash :: C.Hash C.ScriptData -> DatumHash
fromCardanoDatumHash = DatumHash . C.serialiseToRawBytes

toCardanoScriptData :: Datum -> C.ScriptData
toCardanoScriptData = \case
  Constr i ds -> C.ScriptDataConstructor i $ toCardanoScriptData <$> ds
  Map ds -> C.ScriptDataMap $ bimap toCardanoScriptData toCardanoScriptData <$> ds
  List ds -> C.ScriptDataList $ toCardanoScriptData <$> ds
  I i -> C.ScriptDataNumber i
  B bs -> C.ScriptDataBytes bs

fromCardanoScriptData :: C.ScriptData -> Datum
fromCardanoScriptData = \case
  C.ScriptDataConstructor i ds -> Constr i $ fromCardanoScriptData <$> ds
  C.ScriptDataMap ds -> Map $ bimap fromCardanoScriptData fromCardanoScriptData <$> ds
  C.ScriptDataList ds -> List $ fromCardanoScriptData <$> ds
  C.ScriptDataNumber i -> I i
  C.ScriptDataBytes bs -> B bs

toCardanoPaymentCredential :: Credential -> Maybe C.PaymentCredential
toCardanoPaymentCredential = \case
  PaymentKeyCredential pkh -> C.PaymentCredentialByKey <$> toCardanoPaymentKeyHash pkh
  ScriptCredential sh -> C.PaymentCredentialByScript <$> toCardanoScriptHash sh

fromCardanoPaymentCredential :: C.PaymentCredential -> Credential
fromCardanoPaymentCredential = \case
  C.PaymentCredentialByKey pkh -> PaymentKeyCredential $ fromCardanoPaymentKeyHash pkh
  C.PaymentCredentialByScript sh -> ScriptCredential $ fromCardanoScriptHash sh

toCardanoStakeCredential :: StakeCredential -> Maybe C.StakeCredential
toCardanoStakeCredential = \case
  StakeKeyCredential pkh -> C.StakeCredentialByKey <$> toCardanoStakeKeyHash pkh
  StakeScriptCredential sh -> C.StakeCredentialByScript <$> toCardanoScriptHash sh

toCardanoStakeAddressReference :: Maybe StakeReference -> Maybe C.StakeAddressReference
toCardanoStakeAddressReference = \case
  Nothing -> Just C.NoStakeAddress
  Just (StakeCredential cred) -> C.StakeAddressByValue <$> toCardanoStakeCredential cred
  Just (StakePointer slotNo (TxIx txIx) certIx) ->
    Just $
      C.StakeAddressByPointer $
        C.StakeAddressPointer $
          Ptr (toCardanoSlotNo slotNo) (L.TxIx $ fromIntegral txIx) (toCardanoCertIx certIx)

fromCardanoStakeAddressReference :: C.StakeAddressReference -> Maybe StakeReference
fromCardanoStakeAddressReference = \case
  C.NoStakeAddress -> Nothing
  C.StakeAddressByValue credential -> Just $ StakeCredential $ fromCardanoStakeCredential credential
  C.StakeAddressByPointer (C.StakeAddressPointer (Ptr slotNo (L.TxIx txIx) certIx)) -> do
    Just $
      StakePointer
        (fromCardanoSlotNo slotNo)
        (TxIx $ fromIntegral txIx)
        (fromCardanoCertIx certIx)

toCardanoPaymentKeyHash :: PaymentKeyHash -> Maybe (C.Hash C.PaymentKey)
toCardanoPaymentKeyHash = hush . C.deserialiseFromRawBytes (C.AsHash C.AsPaymentKey) . unPaymentKeyHash

toCardanoStakeKeyHash :: StakeKeyHash -> Maybe (C.Hash C.StakeKey)
toCardanoStakeKeyHash = hush . C.deserialiseFromRawBytes (C.AsHash C.AsStakeKey) . unStakeKeyHash

toCardanoPolicyId :: PolicyId -> Maybe C.PolicyId
toCardanoPolicyId = hush . C.deserialiseFromRawBytes C.AsPolicyId . unPolicyId

fromCardanoPolicyId :: C.PolicyId -> PolicyId
fromCardanoPolicyId = PolicyId . C.serialiseToRawBytes

toCardanoAssetName :: TokenName -> C.AssetName
toCardanoAssetName = C.AssetName . unTokenName

fromCardanoAssetName :: C.AssetName -> TokenName
fromCardanoAssetName (C.AssetName name) = TokenName name

toCardanoQuantity :: Quantity -> C.Quantity
toCardanoQuantity = C.Quantity . unQuantity

fromCardanoQuantity :: C.Quantity -> Quantity
fromCardanoQuantity (C.Quantity q) = Quantity q

toCardanoTxId :: TxId -> Maybe C.TxId
toCardanoTxId = hush . C.deserialiseFromRawBytes C.AsTxId . unTxId

fromCardanoTxId :: C.TxId -> TxId
fromCardanoTxId = TxId . C.serialiseToRawBytes

toCardanoTxIx :: TxIx -> C.TxIx
toCardanoTxIx = C.TxIx . fromIntegral . unTxIx

fromCardanoTxIx :: C.TxIx -> TxIx
fromCardanoTxIx (C.TxIx txIx) = TxIx $ fromIntegral txIx

toCardanoCertIx :: CertIx -> L.CertIx
toCardanoCertIx = L.CertIx . unCertIx

fromCardanoCertIx :: L.CertIx -> CertIx
fromCardanoCertIx (L.CertIx certIx) = CertIx certIx

toCardanoTxIn :: TxOutRef -> Maybe C.TxIn
toCardanoTxIn TxOutRef{..} = C.TxIn <$> toCardanoTxId txId <*> pure (toCardanoTxIx txIx)

fromCardanoTxIn :: C.TxIn -> TxOutRef
fromCardanoTxIn (C.TxIn txId txIx) = TxOutRef (fromCardanoTxId txId) (fromCardanoTxIx txIx)

toCardanoAddressAny :: Address -> Maybe C.AddressAny
toCardanoAddressAny = hush . C.deserialiseFromRawBytes C.AsAddressAny . unAddress

fromCardanoAddressAny :: C.AddressAny -> Address
fromCardanoAddressAny = Address . C.serialiseToRawBytes

toCardanoAddressInEra :: C.CardanoEra era -> Address -> Maybe (C.AddressInEra era)
toCardanoAddressInEra era =
  withCardanoEra era $
    hush . C.deserialiseFromRawBytes (C.AsAddressInEra $ cardanoEraToAsType era) . unAddress

fromCardanoAddressInEra :: C.CardanoEra era -> C.AddressInEra era -> Address
fromCardanoAddressInEra era =
  withCardanoEra era $
    Address . C.serialiseToRawBytes

toCardanoLovelace :: Lovelace -> C.Lovelace
toCardanoLovelace = C.Lovelace . unLovelace

fromCardanoLovelace :: C.Lovelace -> Lovelace
fromCardanoLovelace (C.Lovelace l) = Lovelace l

tokensToCardanoValue :: Tokens -> Maybe C.Value
tokensToCardanoValue = fmap C.valueFromList . traverse toCardanoValue' . Map.toList . unTokens
  where
    toCardanoValue' :: (AssetId, Quantity) -> Maybe (C.AssetId, C.Quantity)
    toCardanoValue' (AssetId{..}, quantity) =
      (,)
        <$> (C.AssetId <$> toCardanoPolicyId policyId <*> pure (toCardanoAssetName tokenName))
        <*> pure (toCardanoQuantity quantity)

tokensFromCardanoValue :: C.Value -> Tokens
tokensFromCardanoValue = Tokens . Map.fromList . mapMaybe fromCardanoValue' . C.valueToList
  where
    fromCardanoValue' :: (C.AssetId, C.Quantity) -> Maybe (AssetId, Quantity)
    fromCardanoValue' (C.AdaAssetId, _) = Nothing
    fromCardanoValue' (C.AssetId policy name, q) =
      Just (AssetId (fromCardanoPolicyId policy) (fromCardanoAssetName name), fromCardanoQuantity q)

assetsToCardanoValue :: TxOutAssets -> Maybe C.Value
assetsToCardanoValue (TxOutAssetsContent (Assets{..})) =
  fold
    <$> sequence
      [ Just $ C.valueFromList [(C.AdaAssetId, C.lovelaceToQuantity $ toCardanoLovelace ada)]
      , tokensToCardanoValue tokens
      ]

assetsFromCardanoValue :: C.Value -> Assets
assetsFromCardanoValue value =
  Assets
    { ada = fromCardanoLovelace $ C.selectLovelace value
    , tokens = tokensFromCardanoValue value
    }

toCardanoTxOutValue :: C.MaryEraOnwards era -> TxOutAssets -> Maybe (C.TxOutValue era)
toCardanoTxOutValue C.MaryEraOnwardsMary assets =
  C.TxOutValueShelleyBased C.ShelleyBasedEraMary . C.toLedgerValue C.MaryEraOnwardsMary <$> assetsToCardanoValue assets
toCardanoTxOutValue C.MaryEraOnwardsAlonzo assets =
  C.TxOutValueShelleyBased C.ShelleyBasedEraAlonzo . C.toLedgerValue C.MaryEraOnwardsAlonzo
    <$> assetsToCardanoValue assets
toCardanoTxOutValue C.MaryEraOnwardsBabbage assets =
  C.TxOutValueShelleyBased C.ShelleyBasedEraBabbage . C.toLedgerValue C.MaryEraOnwardsBabbage
    <$> assetsToCardanoValue assets
toCardanoTxOutValue C.MaryEraOnwardsConway assets =
  C.TxOutValueShelleyBased C.ShelleyBasedEraConway . C.toLedgerValue C.MaryEraOnwardsConway
    <$> assetsToCardanoValue assets

fromCardanoTxOutValue :: C.TxOutValue era -> Assets
fromCardanoTxOutValue = \case
  C.TxOutValueByron value -> assetsFromCardanoValue $ C.lovelaceToValue value
  C.TxOutValueShelleyBased era value -> assetsFromCardanoValue $ C.fromLedgerValue era value

toCardanoTxOutDatum
  :: C.CardanoEra era
  -> Maybe DatumHash
  -> Maybe Datum
  -> Maybe (C.TxOutDatum C.CtxTx era)
toCardanoTxOutDatum =
  curry . C.inEonForEra (const $ Just C.TxOutDatumNone) \scriptDataSupported -> \case
    (Nothing, Nothing) -> Just C.TxOutDatumNone
    (Just hash, Nothing) -> C.TxOutDatumHash scriptDataSupported <$> toCardanoDatumHash hash
    (_, Just datum) -> Just $ C.TxOutDatumInTx scriptDataSupported $ unsafeHashableScriptData $ toCardanoScriptData datum

toCardanoTxOutDatum'
  :: C.CardanoEra era
  -> Maybe DatumHash
  -> Maybe (C.TxOutDatum ctx era)
toCardanoTxOutDatum' = C.inEonForEra (const $ Just C.TxOutDatumNone) \scriptDataSupported -> \case
  Nothing -> Just C.TxOutDatumNone
  Just hash -> C.TxOutDatumHash scriptDataSupported <$> toCardanoDatumHash hash

fromCardanoTxOutDatum :: C.TxOutDatum C.CtxTx era -> (Maybe DatumHash, Maybe Datum)
fromCardanoTxOutDatum = \case
  C.TxOutDatumNone -> (Nothing, Nothing)
  C.TxOutDatumHash _ hash -> (Just $ fromCardanoDatumHash hash, Nothing)
  C.TxOutDatumInTx _ datum -> (Nothing, Just $ fromCardanoScriptData $ getScriptData datum)
  C.TxOutDatumInline _ datum -> (Nothing, Just $ fromCardanoScriptData $ getScriptData datum)

toCardanoTxOut :: C.MaryEraOnwards era -> TransactionOutput -> Maybe (C.TxOut C.CtxTx era)
toCardanoTxOut era TransactionOutput{..} = do
  printIfNone $
    C.TxOut
      <$> toCardanoAddressInEra (C.maryEraOnwardsToCardanoEra era) address
      <*> toCardanoTxOutValue era assets
      <*> toCardanoTxOutDatum (C.maryEraOnwardsToCardanoEra era) datumHash datum
      <*> pure C.ReferenceScriptNone
  where
    printIfNone = \case
      Nothing -> traceShow TransactionOutput{..} Nothing
      Just a -> Just a

toCardanoTxOut'
  :: C.MaryEraOnwards era
  -> TransactionOutput
  -> Maybe C.ScriptInAnyLang
  -> Maybe (C.TxOut ctx era)
toCardanoTxOut' era TransactionOutput{..} mScript = do
  ref <- case (mScript, era) of
    (Nothing, _) -> Just C.ReferenceScriptNone
    (_, C.MaryEraOnwardsMary) -> Nothing
    (_, C.MaryEraOnwardsAlonzo) -> Nothing
    (Just script, C.MaryEraOnwardsBabbage) -> Just $ C.ReferenceScript C.BabbageEraOnwardsBabbage script
    (Just script, C.MaryEraOnwardsConway) -> Just $ C.ReferenceScript C.BabbageEraOnwardsConway script
  C.TxOut
    <$> toCardanoAddressInEra (C.maryEraOnwardsToCardanoEra era) address
    <*> toCardanoTxOutValue era assets
    <*> toCardanoTxOutDatum' (C.maryEraOnwardsToCardanoEra era) datumHash
    <*> pure ref

fromCardanoTxOut
  :: C.CardanoEra era
  -> C.TxOut C.CtxTx era
  -> Maybe TransactionOutput
fromCardanoTxOut era (C.TxOut address value txOutDatum _) = do
  txOutAssets <- mkTxOutAssets (fromCardanoTxOutValue value)
  pure $
    TransactionOutput
      (fromCardanoAddressInEra era address)
      txOutAssets
      hash
      datum
  where
    (hash, datum) = fromCardanoTxOutDatum txOutDatum

cardanoEraToAsType :: C.CardanoEra era -> C.AsType era
cardanoEraToAsType = \case
  C.ByronEra -> C.AsByronEra
  C.ShelleyEra -> C.AsShelleyEra
  C.AllegraEra -> C.AsAllegraEra
  C.MaryEra -> C.AsMaryEra
  C.AlonzoEra -> C.AsAlonzoEra
  C.BabbageEra -> C.AsBabbageEra
  C.ConwayEra -> C.AsConwayEra
