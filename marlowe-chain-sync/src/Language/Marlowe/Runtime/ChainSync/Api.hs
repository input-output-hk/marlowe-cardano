{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.ChainSync.Api where

import Cardano.Api (
  AnyCardanoEra (..),
  AsType (..),
  CardanoEra (..),
  CardanoMode,
  ConsensusMode (..),
  EraHistory (..),
  NetworkId (..),
  NetworkMagic (..),
  ScriptDataSupportedInEra (..),
  SerialiseAsRawBytes (..),
  Tx,
  deserialiseFromBech32,
  deserialiseFromCBOR,
  metadataValueToJsonNoSchema,
  serialiseToBech32,
  serialiseToCBOR,
 )
import qualified Cardano.Api as C
import qualified Cardano.Api as Cardano
import Cardano.Api.Shelley (ProtocolParameters)
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.BaseTypes as Base
import Cardano.Ledger.Credential (ptrCertIx, ptrSlotNo, ptrTxIx)
import Cardano.Ledger.Slot (EpochSize)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad (guard, join, (<=<), (>=>))
import Data.Aeson (
  FromJSON (..),
  FromJSONKey (..),
  FromJSONKeyFunction (FromJSONKeyText, FromJSONKeyTextParser),
  ToJSON,
  ToJSONKey,
  toJSON,
 )
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseFail, toJSONKeyText)
import Data.Bifunctor (bimap)
import Data.Binary (Binary (..), get, getWord8, put, putWord8)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16, encodeBase16)
import qualified Data.ByteString.Char8 as BS
import Data.Function (on)
import Data.Functor (($>))
import Data.Hashable (Hashable)
import Data.Kind (Type)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import qualified Data.SOP.Counting as Counting
import Data.Set (Set)
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime (..), diffTimeToPicoseconds, picosecondsToDiffTime)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDateValid, toOrdinalDate)
import Data.Traversable (for)
import Data.Type.Equality (TestEquality (..), type (:~:) (..))
import qualified Data.Vector as Vector
import Data.Word (Word16, Word32, Word64)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Language.Marlowe.Runtime.Cardano.Feature (hush)
import qualified Network.Protocol.ChainSeek.Types as ChainSeek
import Network.Protocol.Codec.Spec
import Network.Protocol.Handshake.Types (HasSignature (..))
import qualified Network.Protocol.Job.Types as Job
import Network.Protocol.Peer.Monad (ClientT, ServerT)
import qualified Network.Protocol.Query.Types as Query
import Ouroboros.Consensus.BlockchainTime (RelativeTime, SlotLength, SystemStart (..))
import Ouroboros.Consensus.HardFork.History (
  Bound,
  EraEnd,
  EraParams,
  EraSummary,
  Interpreter,
  SafeZone,
  Summary (Summary),
  mkInterpreter,
 )
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCostModelParams)
import qualified PlutusLedgerApi.V1 as Plutus
import Text.Read (readMaybe)
import Unsafe.Coerce (unsafeCoerce)

-- | Extends a type with a "Genesis" member.
data WithGenesis a = Genesis | At a
  deriving stock (Show, Eq, Ord, Functor, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

instance (HasSignature a) => HasSignature (WithGenesis a) where
  signature _ = T.intercalate " " ["WithGenesis", signature $ Proxy @a]

-- | A point in the chain, identified by a slot number, block header hash, and
-- block number.
type ChainPoint = WithGenesis BlockHeader

-- | A block header, consisting of a slot number, a hash, and a block number.
data BlockHeader = BlockHeader
  { slotNo :: SlotNo
  -- ^ The slot number when this block was produced.
  , headerHash :: BlockHeaderHash
  -- ^ The hash of this block's header.
  , blockNo :: BlockNo
  -- ^ The ordinal number of this block.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

instance HasSignature BlockHeader where
  signature _ = "BlockHeader"

isAfter :: SlotNo -> BlockHeader -> Bool
isAfter s BlockHeader{..} = slotNo > s

-- | A transaction body
data Transaction = Transaction
  { txId :: TxId
  -- ^ The hash of this transaction.
  , validityRange :: ValidityRange
  -- ^ The range of slots during which this transaction is valid.
  , metadata :: TransactionMetadata
  -- ^ The metadata of this transaction
  , inputs :: Set TransactionInput
  -- ^ The inputs consumed by the transaction
  , outputs :: [TransactionOutput]
  -- ^ The outputs produced by the transaction.
  , mintedTokens :: Tokens
  -- ^ Tokens minted by the transaction.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | A validity range for a transaction
data ValidityRange
  = -- | The transaction is always valid.
    Unbounded
  | -- | The transaction is only valid after a specific slot.
    MinBound SlotNo
  | -- | The transaction is only valid before a specific slot.
    MaxBound SlotNo
  | -- | The transaction is only valid between two slots.
    MinMaxBound SlotNo SlotNo
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- Encodes `transaction_metadatum`:
-- https://github.com/input-output-hk/cardano-ledger/blob/node/1.35.3/eras/shelley/test-suite/cddl-files/shelley.cddl#L203
data Metadata
  = MetadataMap [(Metadata, Metadata)]
  | MetadataList [Metadata]
  | MetadataNumber Integer
  | MetadataBytes ByteString
  | MetadataText Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

parseMetadataMap :: forall k v. (Ord k) => (Metadata -> Maybe k) -> (Metadata -> Maybe v) -> Metadata -> Maybe (Map k v)
parseMetadataMap activeKeyPattern activeValuePattern = \case
  MetadataMap entries ->
    Map.fromList <$> for entries \case
      (activeKeyPattern -> Just key, activeValuePattern -> Just value) -> Just (key, value)
      _ -> Nothing
  _ -> Nothing

parseMetadataList :: (Metadata -> Maybe a) -> Metadata -> Maybe [a]
parseMetadataList activeItemPattern = \case
  MetadataList xs -> for xs \case
    (activeItemPattern -> Just x) -> Just x
    _ -> Nothing
  _ -> Nothing

parseMetadataNumber :: Metadata -> Maybe Integer
parseMetadataNumber = \case MetadataNumber n -> Just n; _ -> Nothing

parseMetadataBytes :: Metadata -> Maybe ByteString
parseMetadataBytes = \case MetadataBytes bs -> Just bs; _ -> Nothing

parseMetadataText :: Metadata -> Maybe Text
parseMetadataText = \case MetadataText bs -> Just bs; _ -> Nothing

-- Using the generic implementation produces an infinite list.
instance Variations Metadata where
  variations =
    join $
      NE.fromList
        [ pure $ MetadataMap []
        , pure $ MetadataMap [NE.head variations]
        , pure $ MetadataList []
        , pure $ MetadataList [NE.head variations]
        , MetadataNumber <$> variations
        , MetadataBytes <$> variations
        , MetadataText <$> variations
        ]

instance ToJSON Metadata where
  toJSON = metadataValueToJsonNoSchema . toCardanoMetadata

toCardanoMetadata :: Metadata -> C.TxMetadataValue
toCardanoMetadata = \case
  MetadataMap ms -> C.TxMetaMap $ bimap toCardanoMetadata toCardanoMetadata <$> ms
  MetadataList ds -> C.TxMetaList $ toCardanoMetadata <$> ds
  MetadataNumber i -> C.TxMetaNumber i
  MetadataBytes bs -> C.TxMetaBytes bs
  MetadataText bs -> C.TxMetaText bs

fromCardanoMetadata :: C.TxMetadataValue -> Metadata
fromCardanoMetadata = \case
  C.TxMetaMap ms -> MetadataMap $ bimap fromCardanoMetadata fromCardanoMetadata <$> ms
  C.TxMetaList ds -> MetadataList $ fromCardanoMetadata <$> ds
  C.TxMetaNumber i -> MetadataNumber i
  C.TxMetaBytes bs -> MetadataBytes bs
  C.TxMetaText bs -> MetadataText bs

-- Handle convenient `JSON` based encoding for a subset of `Metadata`
-- type domain.
-- It is not an implementation of a possible `fromJSON` method.
fromJSONEncodedMetadata :: A.Value -> Maybe Metadata
fromJSONEncodedMetadata = \case
  A.Number n ->
    guard (fromInteger (floor n) == n) $> MetadataNumber (floor n)
  A.String t -> Just $ MetadataText t
  A.Array (Vector.toList -> elems) -> MetadataList <$> traverse fromJSONEncodedMetadata elems
  A.Object (Map.toList . KeyMap.toMapText -> props) ->
    MetadataMap <$> for props \(key, value) -> do
      value' <- fromJSONEncodedMetadata value
      pure (MetadataText key, value')
  A.Bool _ -> Nothing
  A.Null -> Nothing

toJSONEncodedMetadata :: Metadata -> Maybe A.Value
toJSONEncodedMetadata = \case
  MetadataMap ms ->
    A.Object . KeyMap.fromList <$> for ms \case
      (MetadataText k, v) -> do
        let k' = Key.fromText k
        v' <- toJSONEncodedMetadata v
        pure (k', v')
      _ -> Nothing
  MetadataList ds ->
    A.Array . Vector.fromList <$> for ds \e -> do
      toJSONEncodedMetadata e
  MetadataNumber i -> Just $ A.Number $ fromInteger i
  MetadataBytes _ -> Nothing
  MetadataText bs -> Just $ A.String bs

-- Encodes `transaction_metadata`:
-- https://github.com/input-output-hk/cardano-ledger/blob/node/1.35.3/eras/shelley/test-suite/cddl-files/shelley.cddl#L212
newtype TransactionMetadata = TransactionMetadata {unTransactionMetadata :: Map Word64 Metadata}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Semigroup, Monoid, Binary, ToJSON, Variations)

fromJSONEncodedTransactionMetadata :: A.Value -> Maybe TransactionMetadata
fromJSONEncodedTransactionMetadata = \case
  A.Object (Map.toList . KeyMap.toMapText -> props) ->
    TransactionMetadata . Map.fromList <$> for props \(key, value) -> do
      label <- fmap fromInteger $ readMaybe . T.unpack $ key
      value' <- fromJSONEncodedMetadata value
      pure (label, value')
  _ -> Nothing

toCardanoTxMetadata :: TransactionMetadata -> C.TxMetadata
toCardanoTxMetadata (TransactionMetadata metadata) = C.TxMetadata $ toCardanoMetadata <$> metadata

fromCardanoTxMetadata :: C.TxMetadata -> TransactionMetadata
fromCardanoTxMetadata (C.TxMetadata metadata) = TransactionMetadata $ fromCardanoMetadata <$> metadata

-- | An input of a transaction.
data TransactionInput = TransactionInput
  { txId :: TxId
  -- ^ The txId of the TransactionOutput this input consumes.
  , txIx :: TxIx
  -- ^ The txIx of the TransactionOutput this input consumes.
  , address :: Address
  -- ^ The address of the TransactionOutput this input consumes.
  , datumBytes :: Maybe Datum
  -- ^ The script datum for this input
  , redeemer :: Maybe Redeemer
  -- ^ The script redeemer datum for this input (if one was provided).
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | An output of a transaction.
data TransactionOutput = TransactionOutput
  { address :: Address
  -- ^ The address that receives the assets of this output.
  , assets :: Assets
  -- ^ The assets this output produces.
  , datumHash :: Maybe DatumHash
  -- ^ The hash of the script datum associated with this output.
  , datum :: Maybe Datum
  -- ^ The script datum associated with this output.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | A script datum that is used to spend the output of a script tx.
newtype Redeemer = Redeemer {unRedeemer :: Datum}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary, ToJSON, Variations)

-- | A datum as a sum-of-products.
data Datum
  = Constr Integer [Datum]
  | Map [(Datum, Datum)]
  | List [Datum]
  | I Integer
  | B ByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- Using the generic implementation produces an infinite list.
instance Variations Datum where
  variations =
    join $
      NE.fromList
        [ pure $ Constr 1 []
        , pure $ Constr 1 [NE.head variations]
        , pure $ Map []
        , pure $ Map [NE.head variations]
        , pure $ List []
        , pure $ List [NE.head variations]
        , I <$> variations
        , B <$> variations
        ]

instance ToJSON Datum where
  toJSON = \case
    B bs -> toJSON (encodeBase16 bs)
    I i -> toJSON i
    List elems ->
      A.object
        [ ("tag", "list")
        , ("value", toJSON . map toJSON $ elems)
        ]
    Constr i attrs ->
      A.object
        [ ("tag", "constr")
        , ("idx", toJSON i)
        , ("value", toJSON . map toJSON $ attrs)
        ]
    Map props ->
      A.object
        [ ("tag", "map")
        , ("value", toJSON . map toJSON $ props)
        ]

fromDatum :: (Plutus.FromData a) => Datum -> Maybe a
fromDatum = Plutus.fromData . toPlutusData

toDatum :: (Plutus.ToData a) => a -> Datum
toDatum = fromPlutusData . Plutus.toData

fromRedeemer :: (Plutus.FromData a) => Redeemer -> Maybe a
fromRedeemer = fromDatum . unRedeemer

toRedeemer :: (Plutus.ToData a) => a -> Redeemer
toRedeemer = Redeemer . toDatum

-- | Convert from PlutusLedgerApi.V1.Data to Datum
fromPlutusData :: Plutus.Data -> Datum
fromPlutusData (Plutus.Constr i ds) = Constr i $ fromPlutusData <$> ds
fromPlutusData (Plutus.Map m) = Map $ bimap fromPlutusData fromPlutusData <$> m
fromPlutusData (Plutus.List ds) = List $ fromPlutusData <$> ds
fromPlutusData (Plutus.I i) = I i
fromPlutusData (Plutus.B b) = B b

-- | Convert to PlutusLedgerApi.V1.Data to Datum
toPlutusData :: Datum -> Plutus.Data
toPlutusData (Constr i ds) = Plutus.Constr i $ toPlutusData <$> ds
toPlutusData (Map m) = Plutus.Map $ bimap toPlutusData toPlutusData <$> m
toPlutusData (List ds) = Plutus.List $ toPlutusData <$> ds
toPlutusData (I i) = Plutus.I i
toPlutusData (B b) = Plutus.B b

-- | A collection of assets transferred by a transaction output.
data Assets = Assets
  { ada :: Lovelace
  -- ^ The ADA sent by the tx output.
  , tokens :: Tokens
  -- ^ Additional tokens sent by the tx output.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | Let's make the instance explicit so we can assume "some" semantics.
instance Ord Assets where
  compare (Assets a1 t1) (Assets a2 t2) = compare (a1, t1) (a2, t2)

instance Semigroup Assets where
  a <> b =
    Assets
      { ada = on (+) ada a b
      , tokens = on (<>) tokens a b
      }

instance Monoid Assets where
  mempty = Assets 0 mempty

-- | A collection of token quantities by their asset ID.
newtype Tokens = Tokens {unTokens :: Map AssetId Quantity}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary, ToJSON, Variations)

instance Semigroup Tokens where
  (<>) = fmap Tokens . on (Map.unionWith (+)) unTokens

instance Monoid Tokens where
  mempty = Tokens mempty

-- | A newtype wrapper for parsing base 16 strings as byte strings.
newtype Base16 = Base16 {unBase16 :: ByteString}

instance Show Base16 where
  show = show . encodeBase16 . unBase16

instance IsString Base16 where
  fromString = either (error . T.unpack) Base16 . decodeBase16 . encodeUtf8 . T.pack

instance FromJSON Base16 where
  parseJSON = either (parseFail . T.unpack) (pure . Base16) . decodeBase16 . encodeUtf8 <=< parseJSON

instance FromJSONKey Base16 where
  fromJSONKey = FromJSONKeyTextParser $ either (parseFail . T.unpack) (pure . Base16) . decodeBase16 . encodeUtf8

instance ToJSON Base16 where
  toJSON = toJSON . encodeBase16 . unBase16

instance ToJSONKey Base16 where
  toJSONKey = toJSONKeyText $ encodeBase16 . unBase16

newtype DatumHash = DatumHash {unDatumHash :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations, Hashable)
  deriving (IsString, Show, ToJSON) via Base16

newtype TxId = TxId {unTxId :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations, Hashable)
  deriving (IsString, Show, ToJSON, ToJSONKey) via Base16

newtype TxIx = TxIx {unTxIx :: Word16}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary, ToJSON, ToJSONKey, Variations, Hashable)

newtype CertIx = CertIx {unCertIx :: Word64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary, Variations, Hashable)

data TxOutRef = TxOutRef
  { txId :: TxId
  , txIx :: TxIx
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, ToJSONKey, Variations, Hashable)

instance IsString TxOutRef where
  fromString = fromJust . parseTxOutRef . T.pack

parseTxOutRef :: Text -> Maybe TxOutRef
parseTxOutRef val = case T.splitOn "#" val of
  [txId, txIx] ->
    TxOutRef
      <$> (TxId <$> either (const Nothing) Just (decodeBase16 . encodeUtf8 $ txId))
      <*> (TxIx <$> readMaybe (T.unpack txIx))
  _ -> Nothing

renderTxOutRef :: TxOutRef -> Text
renderTxOutRef TxOutRef{..} =
  mconcat
    [ encodeBase16 $ unTxId txId
    , "#"
    , T.pack $ show $ unTxIx txIx
    ]

newtype SlotNo = SlotNo {unSlotNo :: Word64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary, ToJSON, Variations, Hashable)

newtype BlockNo = BlockNo {unBlockNo :: Word64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary, ToJSON, Variations, Hashable)

newtype BlockHeaderHash = BlockHeaderHash {unBlockHeaderHash :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations, Hashable)
  deriving (IsString, Show, ToJSON) via Base16

data AssetId = AssetId
  { policyId :: PolicyId
  , tokenName :: TokenName
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, ToJSONKey, FromJSON, Variations, Hashable)

newtype PolicyId = PolicyId {unPolicyId :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations, Hashable)
  deriving (IsString, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey) via Base16

newtype TokenName = TokenName {unTokenName :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, IsString, Binary, Variations, Hashable)

instance ToJSONKey TokenName where
  toJSONKey = toJSONKeyText $ T.pack . BS.unpack . unTokenName

instance ToJSON TokenName where
  toJSON = Aeson.String . T.pack . BS.unpack . unTokenName

instance FromJSON TokenName where
  parseJSON = Aeson.withText "TokenName" (pure . TokenName . BS.pack . T.unpack)

instance FromJSONKey TokenName where
  fromJSONKey = FromJSONKeyText (TokenName . BS.pack . T.unpack)

newtype Quantity = Quantity {unQuantity :: Word64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary, ToJSON, Variations)

newtype Lovelace = Lovelace {unLovelace :: Word64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary, ToJSON, Variations)

newtype Address = Address {unAddress :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations, Hashable)
  deriving (IsString, Show, ToJSON, FromJSON) via Base16

toBech32 :: Address -> Maybe Text
toBech32 =
  toCardanoAddressAny >=> \case
    Cardano.AddressShelley address ->
      Just $ serialiseToBech32 address
    _ -> Nothing

fromBech32 :: Text -> Maybe Address
fromBech32 =
  fmap (Address . serialiseToRawBytes)
    . either (const Nothing) Just
    . deserialiseFromBech32 (AsAddress AsShelleyAddr)

fromCardanoShelleyAddress :: Cardano.Address Cardano.ShelleyAddr -> Address
fromCardanoShelleyAddress = Address . serialiseToRawBytes

fromCardanoAddressAny :: Cardano.AddressAny -> Maybe Address
fromCardanoAddressAny = \case
  Cardano.AddressShelley shelleyAddr -> Just $ fromCardanoShelleyAddress shelleyAddr
  _ -> Nothing

toCardanoAddressAny :: Address -> Maybe Cardano.AddressAny
toCardanoAddressAny = hush . Cardano.deserialiseFromRawBytes Cardano.AsAddressAny . unAddress

paymentCredential :: Address -> Maybe Credential
paymentCredential =
  toCardanoAddressAny >=> \case
    Cardano.AddressShelley (Cardano.ShelleyAddress _ credential _) ->
      Just $ fromCardanoPaymentCredential $ Cardano.fromShelleyPaymentCredential credential
    _ -> Nothing

stakeReference :: Address -> Maybe StakeReference
stakeReference =
  toCardanoAddressAny >=> \case
    Cardano.AddressShelley (Cardano.ShelleyAddress _ _ ref) ->
      fromCardanoStakeAddressReference $ Cardano.fromShelleyStakeReference ref
    _ -> Nothing

data Credential
  = PaymentKeyCredential PaymentKeyHash
  | ScriptCredential ScriptHash
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations, Hashable)

data StakeCredential
  = StakeKeyCredential StakeKeyHash
  | StakeScriptCredential ScriptHash
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations, Hashable)

fromCardanoPaymentCredential :: Cardano.PaymentCredential -> Credential
fromCardanoPaymentCredential = \case
  Cardano.PaymentCredentialByKey pkh -> PaymentKeyCredential $ fromCardanoPaymentKeyHash pkh
  Cardano.PaymentCredentialByScript scriptHash -> ScriptCredential $ fromCardanoScriptHash scriptHash

newtype PaymentKeyHash = PaymentKeyHash {unPaymentKeyHash :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations, Hashable)
  deriving (IsString, Show, ToJSON) via Base16

newtype StakeKeyHash = StakeKeyHash {unStakeKeyHash :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations, Hashable)
  deriving (IsString, Show, ToJSON) via Base16

fromCardanoPaymentKeyHash :: Cardano.Hash Cardano.PaymentKey -> PaymentKeyHash
fromCardanoPaymentKeyHash = PaymentKeyHash . Cardano.serialiseToRawBytes

fromCardanoStakeKeyHash :: Cardano.Hash Cardano.StakeKey -> StakeKeyHash
fromCardanoStakeKeyHash = StakeKeyHash . Cardano.serialiseToRawBytes

newtype ScriptHash = ScriptHash {unScriptHash :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable)
  deriving (IsString, Show, ToJSON) via Base16
  deriving anyclass (Binary, Variations)

policyIdToScriptHash :: PolicyId -> ScriptHash
policyIdToScriptHash (PolicyId h) = ScriptHash h

fromCardanoScriptHash :: Cardano.ScriptHash -> ScriptHash
fromCardanoScriptHash = ScriptHash . Cardano.serialiseToRawBytes

newtype PlutusScript = PlutusScript {unPlutusScript :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving (IsString, Show, ToJSON) via Base16
  deriving anyclass (Binary, Variations)

data StakeReference
  = StakeCredential StakeCredential
  | StakePointer SlotNo TxIx CertIx
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Variations, Hashable)

fromCardanoStakeAddressReference :: Cardano.StakeAddressReference -> Maybe StakeReference
fromCardanoStakeAddressReference = \case
  Cardano.NoStakeAddress -> Nothing
  Cardano.StakeAddressByValue credential -> Just $ StakeCredential $ fromCardanoStakeCredential credential
  Cardano.StakeAddressByPointer (Cardano.StakeAddressPointer ptr) ->
    Just $
      StakePointer
        (SlotNo $ Cardano.unSlotNo $ ptrSlotNo ptr)
        (let Base.TxIx txIx = ptrTxIx ptr in TxIx $ fromIntegral txIx)
        (let Base.CertIx certIx = ptrCertIx ptr in CertIx certIx)

fromCardanoStakeAddressPointer :: Cardano.StakeAddressPointer -> Word64
fromCardanoStakeAddressPointer = error "not implemented"

fromCardanoStakeCredential :: Cardano.StakeCredential -> StakeCredential
fromCardanoStakeCredential = \case
  Cardano.StakeCredentialByKey skh -> StakeKeyCredential $ fromCardanoStakeKeyHash skh
  Cardano.StakeCredentialByScript scriptHash -> StakeScriptCredential $ fromCardanoScriptHash scriptHash

-- | Reasons a 'FindTx' request can be rejected.
data TxError
  = TxNotFound
  | TxInPast BlockHeader
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | Reasons a 'FindTxsTo' request can be rejected.
data FindTxsToError
  = NoAddresses
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | Reasons a 'FindConsumingTx' request can be rejected.
data UTxOError
  = UTxONotFound
  | UTxOSpent TxId
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | Reasons an 'Intersect' request can be rejected.
data IntersectError = IntersectionNotFound
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

data ChainSyncMove
  = AdvanceBlocks
  | Intersect
  | FindConsumingTxs
  | FindTx
  | FindTxsFor
  | AdvanceToTip
  deriving stock (Show, Read, Eq, Ord, Bounded, Enum, Generic)
  deriving anyclass (Binary, Variations)

instance HasSignature ChainSyncMove where
  signature _ = "ChainSyncMove"

instance ChainSeek.TagKind ChainSyncMove where
  data Tag t where
    TagAdvanceBlocks :: ChainSeek.Tag 'AdvanceBlocks
    TagIntersect :: ChainSeek.Tag 'Intersect
    TagFindConsumingTxs :: ChainSeek.Tag 'FindConsumingTxs
    TagFindTx :: ChainSeek.Tag 'FindTx
    TagFindTxsFor :: ChainSeek.Tag 'FindTxsFor
    TagAdvanceToTip :: ChainSeek.Tag 'AdvanceToTip

  data Move t where
    MoveAdvanceBlocks :: Natural -> ChainSeek.Move 'AdvanceBlocks
    MoveIntersect :: [BlockHeader] -> ChainSeek.Move 'Intersect
    MoveFindConsumingTxs :: Set TxOutRef -> ChainSeek.Move 'FindConsumingTxs
    MoveFindTx :: TxId -> Bool -> ChainSeek.Move 'FindTx
    MoveFindTxsFor :: NESet Credential -> ChainSeek.Move 'FindTxsFor
    MoveAdvanceToTip :: ChainSeek.Move 'AdvanceToTip

  data SeekResult t where
    ResAdvanceBlocks :: ChainSeek.SeekResult 'AdvanceBlocks
    ResIntersect :: ChainSeek.SeekResult 'Intersect
    ResFindConsumingTxs :: Map TxOutRef Transaction -> ChainSeek.SeekResult 'FindConsumingTxs
    ResFindTx :: Transaction -> ChainSeek.SeekResult 'FindTx
    ResFindTxsFor :: Set Transaction -> ChainSeek.SeekResult 'FindTxsFor
    ResAdvanceToTip :: ChainSeek.SeekResult 'AdvanceToTip

  data SeekError t where
    ErrIntersect :: IntersectError -> ChainSeek.SeekError 'Intersect
    ErrFindConsumingTxs :: Map TxOutRef UTxOError -> ChainSeek.SeekError 'FindConsumingTxs
    ErrFindTx :: TxError -> ChainSeek.SeekError 'FindTx

  withSingTag = \case
    TagAdvanceBlocks -> id
    TagIntersect -> id
    TagFindConsumingTxs -> id
    TagFindTx -> id
    TagFindTxsFor -> id
    TagAdvanceToTip -> id

  moveTag = \case
    MoveAdvanceBlocks{} -> TagAdvanceBlocks
    MoveIntersect{} -> TagIntersect
    MoveFindConsumingTxs{} -> TagFindConsumingTxs
    MoveFindTx{} -> TagFindTx
    MoveFindTxsFor{} -> TagFindTxsFor
    MoveAdvanceToTip{} -> TagAdvanceToTip

  fromTag = \case
    TagAdvanceBlocks -> AdvanceBlocks
    TagIntersect -> Intersect
    TagFindConsumingTxs -> FindConsumingTxs
    TagFindTx -> FindTx
    TagFindTxsFor -> FindTxsFor
    TagAdvanceToTip -> AdvanceToTip

  toTag = \case
    AdvanceBlocks -> ChainSeek.SomeTag TagAdvanceBlocks
    Intersect -> ChainSeek.SomeTag TagIntersect
    FindConsumingTxs -> ChainSeek.SomeTag TagFindConsumingTxs
    FindTx -> ChainSeek.SomeTag TagFindTx
    FindTxsFor -> ChainSeek.SomeTag TagFindTxsFor
    AdvanceToTip -> ChainSeek.SomeTag TagAdvanceToTip

instance ChainSeek.SingTag 'AdvanceBlocks where singTag = TagAdvanceBlocks
instance ChainSeek.SingTag 'Intersect where singTag = TagIntersect
instance ChainSeek.SingTag 'FindConsumingTxs where singTag = TagFindConsumingTxs
instance ChainSeek.SingTag 'FindTx where singTag = TagFindTx
instance ChainSeek.SingTag 'FindTxsFor where singTag = TagFindTxsFor
instance ChainSeek.SingTag 'AdvanceToTip where singTag = TagAdvanceToTip

deriving instance Show (ChainSeek.Tag (t :: ChainSyncMove))
deriving instance Eq (ChainSeek.Tag (t :: ChainSyncMove))
deriving instance Ord (ChainSeek.Tag (t :: ChainSyncMove))
deriving instance Show (ChainSeek.Move (t :: ChainSyncMove))
deriving instance Eq (ChainSeek.Move (t :: ChainSyncMove))
deriving instance Ord (ChainSeek.Move (t :: ChainSyncMove))
deriving instance Show (ChainSeek.SeekResult (t :: ChainSyncMove))
deriving instance Eq (ChainSeek.SeekResult (t :: ChainSyncMove))
deriving instance Ord (ChainSeek.SeekResult (t :: ChainSyncMove))
deriving instance Show (ChainSeek.SeekError (t :: ChainSyncMove))
deriving instance Eq (ChainSeek.SeekError (t :: ChainSyncMove))
deriving instance Ord (ChainSeek.SeekError (t :: ChainSyncMove))

instance TestEquality (ChainSeek.Tag :: ChainSyncMove -> Type) where
  testEquality = \case
    TagAdvanceBlocks -> \case
      TagAdvanceBlocks -> Just Refl
      _ -> Nothing
    TagIntersect -> \case
      TagIntersect -> Just Refl
      _ -> Nothing
    TagFindConsumingTxs -> \case
      TagFindConsumingTxs -> Just Refl
      _ -> Nothing
    TagFindTx -> \case
      TagFindTx -> Just Refl
      _ -> Nothing
    TagFindTxsFor -> \case
      TagFindTxsFor -> Just Refl
      _ -> Nothing
    TagAdvanceToTip -> \case
      TagAdvanceToTip -> Just Refl
      _ -> Nothing

instance ChainSeek.EqTagKind ChainSyncMove
instance ChainSeek.ShowTagKind ChainSyncMove

instance ChainSeek.VariationsTagKind ChainSyncMove where
  moveVariations = \case
    TagAdvanceBlocks -> MoveAdvanceBlocks <$> variations
    TagIntersect -> MoveIntersect <$> variations
    TagFindConsumingTxs -> MoveFindConsumingTxs <$> variations
    TagFindTx -> MoveFindTx <$> variations `varyAp` variations
    TagFindTxsFor -> MoveFindTxsFor <$> variations
    TagAdvanceToTip -> pure MoveAdvanceToTip
  errorVariations = \case
    TagAdvanceBlocks -> []
    TagIntersect -> ErrIntersect <$> NE.toList variations
    TagFindConsumingTxs -> ErrFindConsumingTxs <$> NE.toList variations
    TagFindTx -> ErrFindTx <$> NE.toList variations
    TagFindTxsFor -> []
    TagAdvanceToTip -> []
  resultVariations = \case
    TagAdvanceBlocks -> pure ResAdvanceBlocks
    TagIntersect -> pure ResIntersect
    TagFindConsumingTxs -> ResFindConsumingTxs <$> variations
    TagFindTx -> ResFindTx <$> variations
    TagFindTxsFor -> ResFindTxsFor <$> variations
    TagAdvanceToTip -> pure ResAdvanceToTip

type RuntimeChainSeek = ChainSeek.ChainSeek ChainSyncMove ChainPoint ChainPoint

type RuntimeChainSeekClientT = ClientT RuntimeChainSeek

type RuntimeChainSeekServerT = ServerT RuntimeChainSeek

instance ChainSeek.BinaryTagKind ChainSyncMove where
  putMove = \case
    MoveAdvanceBlocks blocks -> put blocks
    MoveIntersect points -> put points
    MoveFindTx txId wait -> put txId *> put wait
    MoveFindConsumingTxs utxos -> put utxos
    MoveAdvanceToTip -> mempty
    MoveFindTxsFor credentials -> put $ NESet.toList credentials

  getMove = \case
    TagAdvanceBlocks -> MoveAdvanceBlocks <$> get
    TagIntersect -> MoveIntersect <$> get
    TagFindTx -> MoveFindTx <$> get <*> get
    TagFindConsumingTxs -> MoveFindConsumingTxs <$> get
    TagAdvanceToTip -> pure MoveAdvanceToTip
    TagFindTxsFor -> MoveFindTxsFor . NESet.fromList <$> get

  putSeekResult = \case
    ResAdvanceBlocks -> mempty
    ResFindTx tx -> put tx
    ResIntersect -> mempty
    ResFindConsumingTxs txs -> put txs
    ResAdvanceToTip -> mempty
    ResFindTxsFor txs -> put txs

  getSeekResult = \case
    TagAdvanceBlocks -> pure ResAdvanceBlocks
    TagFindTx -> ResFindTx <$> get
    TagIntersect -> pure ResIntersect
    TagFindConsumingTxs -> ResFindConsumingTxs <$> get
    TagAdvanceToTip -> pure ResAdvanceToTip
    TagFindTxsFor -> ResFindTxsFor <$> get

  putSeekError = \case
    ErrFindTx err -> put err
    ErrIntersect err -> put err
    ErrFindConsumingTxs err -> put err

  getSeekError = \case
    TagAdvanceBlocks -> fail "advance blocks has no error"
    TagFindTx -> ErrFindTx <$> get
    TagIntersect -> ErrIntersect <$> get
    TagFindConsumingTxs -> ErrFindConsumingTxs <$> get
    TagFindTxsFor -> fail "find txs for has no error"
    TagAdvanceToTip -> fail "advance to tip has no error"

instance Binary UTCTime where
  put UTCTime{..} = do
    let (year, dayOfYear) = toOrdinalDate utctDay
    put year
    put dayOfYear
    put $ diffTimeToPicoseconds utctDayTime

  get = do
    year <- get
    dayOfYear <- get
    utctDayTime <- picosecondsToDiffTime <$> get
    utctDay <- case fromOrdinalDateValid year dayOfYear of
      Nothing -> fail "Invalid ISO 8601 ordinal date"
      Just a -> pure a
    pure UTCTime{..}

data GetUTxOsQuery
  = GetUTxOsAtAddresses (Set Address)
  | GetUTxOsForTxOutRefs (Set TxOutRef)
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Variations, Binary)

-- Semigroup and Monoid seem to be safe - we cover here a subset of a partial function.
newtype UTxOs = UTxOs {unUTxOs :: Map TxOutRef TransactionOutput}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Semigroup, Monoid, Variations)
  deriving anyclass (Binary, ToJSON)

lookupUTxO :: TxOutRef -> UTxOs -> Maybe TransactionOutput
lookupUTxO txOutRef (UTxOs utxos) = Map.lookup txOutRef utxos

toUTxOsList :: UTxOs -> [UTxO]
toUTxOsList (UTxOs (Map.toList -> utxos)) = fmap (uncurry UTxO) utxos

data UTxO = UTxO
  { txOutRef :: TxOutRef
  , transactionOutput :: TransactionOutput
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Variations)

toUTxOTuple :: UTxO -> (TxOutRef, TransactionOutput)
toUTxOTuple (UTxO txOutRef transactionOutput) = (txOutRef, transactionOutput)

data ChainSyncQuery
  = GetSecurityParameter
  | GetNetworkId
  | GetProtocolParameters
  | GetSystemStart
  | GetEraHistory
  | GetUTxOs
  | GetNodeTip
  | GetTip
  | GetEra
  deriving stock (Show, Read, Eq, Ord, Bounded, Enum, Generic)
  deriving anyclass (Binary, Variations)

instance HasSignature ChainSyncQuery where
  signature _ = "ChainSyncQuery"

instance Query.TagKind ChainSyncQuery where
  data Tag t where
    TagGetSecurityParameter :: Query.Tag 'GetSecurityParameter
    TagGetNetworkId :: Query.Tag 'GetNetworkId
    TagGetProtocolParameters :: Query.Tag 'GetProtocolParameters
    TagGetSystemStart :: Query.Tag 'GetSystemStart
    TagGetEraHistory :: Query.Tag 'GetEraHistory
    TagGetUTxOs :: Query.Tag 'GetUTxOs
    TagGetNodeTip :: Query.Tag 'GetNodeTip
    TagGetTip :: Query.Tag 'GetTip
    TagGetEra :: Query.Tag 'GetEra

  data Request t where
    ReqGetSecurityParameter :: Query.Request 'GetSecurityParameter
    ReqGetNetworkId :: Query.Request 'GetNetworkId
    ReqGetProtocolParameters :: Query.Request 'GetProtocolParameters
    ReqGetSystemStart :: Query.Request 'GetSystemStart
    ReqGetEraHistory :: Query.Request 'GetEraHistory
    ReqGetUTxOs :: GetUTxOsQuery -> Query.Request 'GetUTxOs
    ReqGetNodeTip :: Query.Request 'GetNodeTip
    ReqGetTip :: Query.Request 'GetTip
    ReqGetEra :: Query.Request 'GetEra

  data Response t where
    ResGetSecurityParameter :: Int -> Query.Response 'GetSecurityParameter
    ResGetNetworkId :: NetworkId -> Query.Response 'GetNetworkId
    ResGetProtocolParameters :: ProtocolParameters -> Query.Response 'GetProtocolParameters
    ResGetSystemStart :: SystemStart -> Query.Response 'GetSystemStart
    ResGetEraHistory :: EraHistory CardanoMode -> Query.Response 'GetEraHistory
    ResGetUTxOs :: UTxOs -> Query.Response 'GetUTxOs
    ResGetNodeTip :: ChainPoint -> Query.Response 'GetNodeTip
    ResGetTip :: ChainPoint -> Query.Response 'GetTip
    ResGetEra :: AnyCardanoEra -> Query.Response 'GetEra

  withSingTag = \case
    TagGetSecurityParameter -> id
    TagGetNetworkId -> id
    TagGetProtocolParameters -> id
    TagGetSystemStart -> id
    TagGetEraHistory -> id
    TagGetUTxOs -> id
    TagGetNodeTip -> id
    TagGetTip -> id
    TagGetEra -> id

  requestTag = \case
    ReqGetSecurityParameter -> TagGetSecurityParameter
    ReqGetNetworkId -> TagGetNetworkId
    ReqGetProtocolParameters -> TagGetProtocolParameters
    ReqGetSystemStart -> TagGetSystemStart
    ReqGetEraHistory -> TagGetEraHistory
    ReqGetUTxOs{} -> TagGetUTxOs
    ReqGetNodeTip -> TagGetNodeTip
    ReqGetTip -> TagGetTip
    ReqGetEra -> TagGetEra

  fromTag = \case
    TagGetSecurityParameter -> GetSecurityParameter
    TagGetNetworkId -> GetNetworkId
    TagGetProtocolParameters -> GetProtocolParameters
    TagGetSystemStart -> GetSystemStart
    TagGetEraHistory -> GetEraHistory
    TagGetUTxOs -> GetUTxOs
    TagGetNodeTip -> GetNodeTip
    TagGetTip -> GetTip
    TagGetEra -> GetEra

  toTag = \case
    GetSecurityParameter -> Query.SomeTag TagGetSecurityParameter
    GetNetworkId -> Query.SomeTag TagGetNetworkId
    GetProtocolParameters -> Query.SomeTag TagGetProtocolParameters
    GetSystemStart -> Query.SomeTag TagGetSystemStart
    GetEraHistory -> Query.SomeTag TagGetEraHistory
    GetUTxOs -> Query.SomeTag TagGetUTxOs
    GetNodeTip -> Query.SomeTag TagGetNodeTip
    GetTip -> Query.SomeTag TagGetTip
    GetEra -> Query.SomeTag TagGetEra

instance Query.SingTag 'GetSecurityParameter where singTag = TagGetSecurityParameter
instance Query.SingTag 'GetNetworkId where singTag = TagGetNetworkId
instance Query.SingTag 'GetProtocolParameters where singTag = TagGetProtocolParameters
instance Query.SingTag 'GetSystemStart where singTag = TagGetSystemStart
instance Query.SingTag 'GetEraHistory where singTag = TagGetEraHistory
instance Query.SingTag 'GetUTxOs where singTag = TagGetUTxOs
instance Query.SingTag 'GetNodeTip where singTag = TagGetNodeTip
instance Query.SingTag 'GetTip where singTag = TagGetTip
instance Query.SingTag 'GetEra where singTag = TagGetEra

deriving instance Show (Query.Tag (t :: ChainSyncQuery))
deriving instance Eq (Query.Tag (t :: ChainSyncQuery))
deriving instance Ord (Query.Tag (t :: ChainSyncQuery))
deriving instance Show (Query.Request (t :: ChainSyncQuery))
deriving instance Eq (Query.Request (t :: ChainSyncQuery))
deriving instance Ord (Query.Request (t :: ChainSyncQuery))
deriving instance Show (Query.Response (t :: ChainSyncQuery))
deriving instance Eq (Query.Response (t :: ChainSyncQuery))

instance TestEquality (Query.Tag :: ChainSyncQuery -> Type) where
  testEquality TagGetSecurityParameter TagGetSecurityParameter = Just Refl
  testEquality TagGetSecurityParameter _ = Nothing
  testEquality TagGetNetworkId TagGetNetworkId = Just Refl
  testEquality TagGetNetworkId _ = Nothing
  testEquality TagGetProtocolParameters TagGetProtocolParameters = Just Refl
  testEquality TagGetProtocolParameters _ = Nothing
  testEquality TagGetEraHistory TagGetEraHistory = Just Refl
  testEquality TagGetEraHistory _ = Nothing
  testEquality TagGetSystemStart TagGetSystemStart = Just Refl
  testEquality TagGetSystemStart _ = Nothing
  testEquality TagGetUTxOs TagGetUTxOs = Just Refl
  testEquality TagGetUTxOs _ = Nothing
  testEquality TagGetNodeTip TagGetNodeTip = Just Refl
  testEquality TagGetNodeTip _ = Nothing
  testEquality TagGetTip TagGetTip = Just Refl
  testEquality TagGetTip _ = Nothing
  testEquality TagGetEra TagGetEra = Just Refl
  testEquality TagGetEra _ = Nothing

deriving instance Show (EraHistory CardanoMode)
deriving instance Eq (EraHistory CardanoMode)
deriving instance Eq (ConsensusMode a)

instance Ord AnyCardanoEra where
  compare = on compare fromEnum

instance Binary AnyCardanoEra where
  put = putWord8 . fromIntegral . fromEnum
  get = do
    tag <- getWord8
    case tag of
      0 -> pure $ AnyCardanoEra ByronEra
      1 -> pure $ AnyCardanoEra ShelleyEra
      2 -> pure $ AnyCardanoEra AllegraEra
      3 -> pure $ AnyCardanoEra MaryEra
      4 -> pure $ AnyCardanoEra AlonzoEra
      5 -> pure $ AnyCardanoEra BabbageEra
      6 -> pure $ AnyCardanoEra ConwayEra
      _ -> fail $ "Invalid era tag: " <> show tag

instance Query.VariationsTagKind ChainSyncQuery where
  requestVariations = \case
    TagGetSecurityParameter -> pure ReqGetSecurityParameter
    TagGetNetworkId -> pure ReqGetNetworkId
    TagGetProtocolParameters -> pure ReqGetProtocolParameters
    TagGetSystemStart -> pure ReqGetSystemStart
    TagGetEraHistory -> pure ReqGetEraHistory
    TagGetUTxOs -> ReqGetUTxOs <$> variations
    TagGetNodeTip -> pure ReqGetNodeTip
    TagGetTip -> pure ReqGetTip
    TagGetEra -> pure ReqGetEra

  responseVariations = \case
    TagGetSecurityParameter -> ResGetSecurityParameter <$> variations
    TagGetNetworkId -> ResGetNetworkId <$> Mainnet NE.:| [Testnet $ NetworkMagic 0]
    TagGetProtocolParameters -> ResGetProtocolParameters <$> variations
    TagGetSystemStart -> ResGetSystemStart . SystemStart <$> variations
    TagGetEraHistory -> ResGetEraHistory <$> variations
    TagGetUTxOs -> ResGetUTxOs <$> variations
    TagGetNodeTip -> ResGetNodeTip <$> variations
    TagGetTip -> ResGetTip <$> variations
    TagGetEra -> ResGetEra <$> variations

instance Query.BinaryTagKind ChainSyncQuery where
  putRequest = \case
    ReqGetSecurityParameter -> mempty
    ReqGetNetworkId -> mempty
    ReqGetProtocolParameters -> mempty
    ReqGetSystemStart -> mempty
    ReqGetEraHistory -> mempty
    ReqGetUTxOs q -> put q
    ReqGetNodeTip -> mempty
    ReqGetTip -> mempty
    ReqGetEra -> mempty

  getRequest = \case
    TagGetSecurityParameter -> pure ReqGetSecurityParameter
    TagGetNetworkId -> pure ReqGetNetworkId
    TagGetProtocolParameters -> pure ReqGetProtocolParameters
    TagGetSystemStart -> pure ReqGetSystemStart
    TagGetEraHistory -> pure ReqGetEraHistory
    TagGetUTxOs -> ReqGetUTxOs <$> get
    TagGetNodeTip -> pure ReqGetNodeTip
    TagGetTip -> pure ReqGetTip
    TagGetEra -> pure ReqGetEra

  putResponse = \case
    ResGetSecurityParameter x -> put x
    ResGetNetworkId Mainnet -> put $ Nothing @Word32
    ResGetNetworkId (Testnet (NetworkMagic x)) -> put $ Just x
    ResGetProtocolParameters x -> put $ Aeson.encode x
    ResGetSystemStart (SystemStart x) -> put x
    ResGetEraHistory (EraHistory _ x) -> put $ serialise x
    ResGetUTxOs x -> put x
    ResGetNodeTip x -> put x
    ResGetTip x -> put x
    ResGetEra x -> put x

  getResponse = \case
    TagGetSecurityParameter -> ResGetSecurityParameter <$> get
    TagGetNetworkId -> ResGetNetworkId . maybe Mainnet (Testnet . NetworkMagic) <$> get
    TagGetProtocolParameters ->
      ResGetProtocolParameters <$> do
        bytes <- get
        case Aeson.decode bytes of
          Nothing -> fail "failed to decode protocol parameters JSON"
          Just params -> pure params
    TagGetSystemStart -> ResGetSystemStart . SystemStart <$> get
    TagGetEraHistory ->
      ResGetEraHistory <$> do
        bytes <- get
        case deserialiseOrFail bytes of
          Left err -> fail $ show err
          Right interpreter -> pure $ EraHistory CardanoMode interpreter
    TagGetUTxOs -> ResGetUTxOs <$> get
    TagGetNodeTip -> ResGetNodeTip <$> get
    TagGetTip -> ResGetTip <$> get
    TagGetEra -> ResGetEra <$> get

instance Query.ShowTagKind ChainSyncQuery
instance Query.EqTagKind ChainSyncQuery

unInterpreter :: Interpreter xs -> Summary xs
unInterpreter = unsafeCoerce

data ChainSyncCommand = SubmitTx
  deriving stock (Show, Read, Eq, Ord, Bounded, Enum, Generic)
  deriving anyclass (Binary, Variations)

instance Job.TagKind ChainSyncCommand where
  data Tag t where
    TagSubmitTx :: Job.Tag 'SubmitTx

  data Command t where
    CmdSubmitTx :: ScriptDataSupportedInEra era -> Tx era -> Job.Command 'SubmitTx

  data JobError t where
    ErrSubmitTx :: String -> Job.JobError 'SubmitTx

  data JobResult t where
    ResSubmitTx :: Job.JobResult 'SubmitTx

  data JobId t
  data Status t

  withSingTag = \case
    TagSubmitTx -> id

  commandTag = \case
    CmdSubmitTx{} -> TagSubmitTx

  jobIdTag = \case {}

  fromTag = \case
    TagSubmitTx -> SubmitTx

  toTag = \case
    SubmitTx -> Job.SomeTag TagSubmitTx

instance Job.SingTag 'SubmitTx where singTag = TagSubmitTx

deriving instance Show (Job.Tag (t :: ChainSyncCommand))
deriving instance Eq (Job.Tag (t :: ChainSyncCommand))
deriving instance Ord (Job.Tag (t :: ChainSyncCommand))
deriving instance Show (Job.Command (t :: ChainSyncCommand))
instance Eq (Job.Command (t :: ChainSyncCommand)) where
  CmdSubmitTx era tx == CmdSubmitTx era' tx' = case (era, era') of
    (ScriptDataInAlonzoEra, ScriptDataInAlonzoEra) -> tx == tx'
    (ScriptDataInAlonzoEra, _) -> False
    (ScriptDataInBabbageEra, ScriptDataInBabbageEra) -> tx == tx'
    (ScriptDataInBabbageEra, _) -> False
    (ScriptDataInConwayEra, ScriptDataInConwayEra) -> tx == tx'
    (ScriptDataInConwayEra, _) -> False
deriving instance Show (Job.JobId (t :: ChainSyncCommand))
deriving instance Eq (Job.JobId (t :: ChainSyncCommand))
deriving instance Ord (Job.JobId (t :: ChainSyncCommand))
deriving instance Show (Job.JobResult (t :: ChainSyncCommand))
deriving instance Eq (Job.JobResult (t :: ChainSyncCommand))
deriving instance Ord (Job.JobResult (t :: ChainSyncCommand))
deriving instance Show (Job.JobError (t :: ChainSyncCommand))
deriving instance Eq (Job.JobError (t :: ChainSyncCommand))
deriving instance Ord (Job.JobError (t :: ChainSyncCommand))
deriving instance Show (Job.Status (t :: ChainSyncCommand))
deriving instance Eq (Job.Status (t :: ChainSyncCommand))
deriving instance Ord (Job.Status (t :: ChainSyncCommand))

instance TestEquality (Job.Tag :: ChainSyncCommand -> Type) where
  testEquality TagSubmitTx TagSubmitTx = Just Refl

instance Job.EqTagKind ChainSyncCommand
instance Job.ShowTagKind ChainSyncCommand

instance HasSignature ChainSyncCommand where
  signature _ = "ChainSyncCommand"

instance Job.BinaryTagKind ChainSyncCommand where
  putCommand = \case
    CmdSubmitTx ScriptDataInAlonzoEra tx -> do
      putWord8 0x01
      put $ serialiseToCBOR tx
    CmdSubmitTx ScriptDataInBabbageEra tx -> do
      putWord8 0x02
      put $ serialiseToCBOR tx
    CmdSubmitTx ScriptDataInConwayEra tx -> do
      putWord8 0x03
      put $ serialiseToCBOR tx
  getCommand = \case
    TagSubmitTx ->
      getWord8 >>= \case
        0x01 ->
          CmdSubmitTx ScriptDataInAlonzoEra <$> do
            bytes <- get
            case deserialiseFromCBOR (AsTx AsAlonzo) bytes of
              Left err -> fail $ show err
              Right tx -> pure tx
        0x02 ->
          CmdSubmitTx ScriptDataInBabbageEra <$> do
            bytes <- get
            case deserialiseFromCBOR (AsTx AsBabbage) bytes of
              Left err -> fail $ show err
              Right tx -> pure tx
        0x03 ->
          CmdSubmitTx ScriptDataInConwayEra <$> do
            bytes <- get
            case deserialiseFromCBOR (AsTx AsConway) bytes of
              Left err -> fail $ show err
              Right tx -> pure tx
        tag -> fail $ "invalid era tag " <> show tag
  putJobResult = \case
    ResSubmitTx -> mempty
  getJobResult = \case
    TagSubmitTx -> pure ResSubmitTx
  putJobError = \case
    ErrSubmitTx err -> put err
  getJobError = \case
    TagSubmitTx -> ErrSubmitTx <$> get
  putStatus = \case {}
  getStatus = \case
    TagSubmitTx -> fail "submit tx has no status"
  putJobId = \case {}
  getJobId = \case
    TagSubmitTx -> fail "submit tx has no job ID"

-- * Orphan instances

instance Variations AnyCardanoEra where
  variations = NE.fromList [minBound .. maxBound]

instance Variations ProtocolParameters

instance Variations C.PraosNonce where
  variations = C.makePraosNonce <$> variations

instance Variations C.Lovelace where
  variations = C.Lovelace <$> variations

instance Variations C.EpochNo

instance Variations C.AnyPlutusScriptVersion where
  variations =
    NE.fromList
      [ C.AnyPlutusScriptVersion C.PlutusScriptV1
      , C.AnyPlutusScriptVersion C.PlutusScriptV2
      ]

instance Variations C.CostModel where
  variations = pure $ C.CostModel $ Map.elems $ fromJust defaultCostModelParams

instance Variations C.ExecutionUnitPrices where
  variations = C.ExecutionUnitPrices <$> variations `varyAp` variations

instance Variations C.ExecutionUnits where
  variations = C.ExecutionUnits <$> variations `varyAp` variations

instance Variations (C.EraHistory CardanoMode) where
  variations = C.EraHistory C.CardanoMode <$> variations

instance (Variations (Counting.NonEmpty xs EraSummary)) => Variations (Interpreter xs) where
  variations = mkInterpreter <$> variations

instance (Variations (Counting.NonEmpty xs EraSummary)) => Variations (Summary xs) where
  variations = Summary <$> variations

instance {-# OVERLAPPING #-} (Variations a) => Variations (Counting.NonEmpty '[x] a) where
  variations = Counting.NonEmptyOne <$> variations

instance {-# OVERLAPPING #-} (Variations a, Variations (Counting.NonEmpty xs a)) => Variations (Counting.NonEmpty (x ': xs) a) where
  variations = Counting.NonEmptyCons <$> variations `varyAp` pure (NE.head variations)

instance Variations EraSummary

instance Variations EraParams

instance Variations SafeZone

instance Variations EpochSize

instance Variations SlotLength

instance Variations EraEnd

instance Variations Bound

instance Variations Cardano.SlotNo

instance Variations RelativeTime
