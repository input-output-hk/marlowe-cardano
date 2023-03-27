{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Marlowe.Runtime.ChainSync.Api
  where

import Cardano.Api
  ( AsType(..)
  , CardanoMode
  , ConsensusMode(..)
  , EraHistory(..)
  , NetworkId(..)
  , NetworkMagic(..)
  , ScriptDataSupportedInEra(..)
  , SerialiseAsRawBytes(..)
  , Tx
  , deserialiseFromBech32
  , deserialiseFromCBOR
  , metadataValueToJsonNoSchema
  , serialiseToBech32
  , serialiseToCBOR
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
import Data.Aeson
  ( FromJSON(..)
  , FromJSONKey(..)
  , FromJSONKeyFunction(FromJSONKeyText, FromJSONKeyTextParser)
  , ToJSON
  , ToJSONKey
  , Value(..)
  , object
  , toJSON
  , (.=)
  )
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseFail, toJSONKeyText)
import Data.Bifunctor (bimap)
import Data.Binary (Binary(..), Get, Put, get, getWord8, put, putWord8)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16, encodeBase16)
import qualified Data.ByteString.Char8 as BS
import Data.Function (on)
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Proxy (Proxy(..))
import Data.Set (Set)
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime(..), diffTimeToPicoseconds, picosecondsToDiffTime)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDateValid, toOrdinalDate)
import Data.Traversable (for)
import Data.Type.Equality (type (:~:)(Refl))
import qualified Data.Vector as Vector
import Data.Void (Void, absurd)
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Network.Protocol.ChainSeek.Client
import Network.Protocol.ChainSeek.Server
import Network.Protocol.ChainSeek.Types
import qualified Network.Protocol.ChainSeek.Types as ChainSeek
import Network.Protocol.Codec.Spec
import Network.Protocol.Handshake.Types (HasSignature(..))
import Network.Protocol.Job.Types (CommandToJSON)
import qualified Network.Protocol.Job.Types as Job
import qualified Network.Protocol.Query.Types as Query
import Ouroboros.Consensus.BlockchainTime (RelativeTime, SlotLength, SystemStart(..))
import Ouroboros.Consensus.HardFork.History
  (Bound, EraEnd, EraParams, EraSummary, Interpreter, SafeZone, Summary(Summary), mkInterpreter)
import qualified Ouroboros.Consensus.Util.Counting as Counting
import qualified Plutus.V1.Ledger.Api as Plutus
import Text.Read (readMaybe)

-- | Extends a type with a "Genesis" member.
data WithGenesis a = Genesis | At a
  deriving stock (Show, Eq, Ord, Functor, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

instance HasSignature a => HasSignature (WithGenesis a) where
  signature _ = T.intercalate " " ["WithGenesis", signature $ Proxy @a]

-- | A point in the chain, identified by a slot number, block header hash, and
-- block number.
type ChainPoint = WithGenesis BlockHeader

-- | A block header, consisting of a slot number, a hash, and a block number.
data BlockHeader = BlockHeader
  { slotNo     :: SlotNo          -- ^ The slot number when this block was produced.
  , headerHash :: BlockHeaderHash -- ^ The hash of this block's header.
  , blockNo    :: BlockNo         -- ^ The ordinal number of this block.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

instance HasSignature BlockHeader where
  signature _ = "BlockHeader"

isAfter :: SlotNo -> BlockHeader -> Bool
isAfter s BlockHeader{..} = slotNo > s

-- | A transaction body
data Transaction = Transaction
  { txId          :: TxId                   -- ^ The hash of this transaction.
  , validityRange :: ValidityRange          -- ^ The range of slots during which this transaction is valid.
  , metadata      :: TransactionMetadata -- ^ The metadata of this transaction
  , inputs        :: Set TransactionInput -- ^ The inputs consumed by the transaction
  , outputs       :: [TransactionOutput]    -- ^ The outputs produced by the transaction.
  , mintedTokens  :: Tokens                 -- ^ Tokens minted by the transaction.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | A validity range for a transaction
data ValidityRange
  = Unbounded                 -- ^ The transaction is always valid.
  | MinBound SlotNo           -- ^ The transaction is only valid after a specific slot.
  | MaxBound SlotNo           -- ^ The transaction is only valid before a specific slot.
  | MinMaxBound SlotNo SlotNo -- ^ The transaction is only valid between two slots.
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

parseMetadataMap :: forall k v. Ord k => (Metadata -> Maybe k) -> (Metadata -> Maybe v) -> Metadata -> Maybe (Map k v)
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
  variations = join $ NE.fromList
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
  A.Object (Map.toList . KeyMap.toMapText -> props) -> MetadataMap <$> for props \(key, value) -> do
    value' <- fromJSONEncodedMetadata value
    pure (MetadataText key, value')
  A.Bool _ -> Nothing
  A.Null -> Nothing

-- Encodes `transaction_metadata`:
-- https://github.com/input-output-hk/cardano-ledger/blob/node/1.35.3/eras/shelley/test-suite/cddl-files/shelley.cddl#L212
newtype TransactionMetadata = TransactionMetadata { unTransactionMetadata :: Map Word64 Metadata }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Semigroup, Monoid, Binary, ToJSON, Variations)

fromJSONEncodedTransactionMetadata :: A.Value -> Maybe TransactionMetadata
fromJSONEncodedTransactionMetadata = \case
  A.Object (Map.toList . KeyMap.toMapText -> props) -> TransactionMetadata . Map.fromList <$> for props \(key, value) -> do
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
  { txId       :: TxId             -- ^ The txId of the TransactionOutput this input consumes.
  , txIx       :: TxIx             -- ^ The txIx of the TransactionOutput this input consumes.
  , address    :: Address          -- ^ The address of the TransactionOutput this input consumes.
  , datumBytes :: Maybe Datum    -- ^ The script datum for this input
  , redeemer   :: Maybe Redeemer -- ^ The script redeemer datum for this input (if one was provided).
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | An output of a transaction.
data TransactionOutput = TransactionOutput
  { address   :: Address           -- ^ The address that receives the assets of this output.
  , assets    :: Assets            -- ^ The assets this output produces.
  , datumHash :: Maybe DatumHash -- ^ The hash of the script datum associated with this output.
  , datum     :: Maybe Datum     -- ^ The script datum associated with this output.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | A script datum that is used to spend the output of a script tx.
newtype Redeemer = Redeemer { unRedeemer :: Datum }
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
  variations = join $ NE.fromList
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
    List elems -> A.object
      [ ("tag", "list")
      , ("value", toJSON . map toJSON $ elems)
      ]
    Constr i attrs -> A.object
      [ ("tag", "constr")
      , ("idx", toJSON i)
      , ("value", toJSON . map toJSON $ attrs)
      ]
    Map props -> A.object
      [ ("tag", "map")
      , ("value", toJSON . map toJSON $ props)
      ]

fromDatum :: Plutus.FromData a => Datum -> Maybe a
fromDatum = Plutus.fromData . toPlutusData

toDatum :: Plutus.ToData a => a -> Datum
toDatum = fromPlutusData . Plutus.toData

fromRedeemer :: Plutus.FromData a => Redeemer -> Maybe a
fromRedeemer = fromDatum . unRedeemer

toRedeemer :: Plutus.ToData a => a -> Redeemer
toRedeemer = Redeemer . toDatum

-- | Convert from Plutus.V1.Ledger.Api.Data to Datum
fromPlutusData :: Plutus.Data -> Datum
fromPlutusData (Plutus.Constr i ds) = Constr i $ fromPlutusData <$> ds
fromPlutusData (Plutus.Map m)       = Map $ bimap fromPlutusData fromPlutusData <$> m
fromPlutusData (Plutus.List ds)     = List $ fromPlutusData <$> ds
fromPlutusData (Plutus.I i)         = I i
fromPlutusData (Plutus.B b)         = B b

-- | Convert to Plutus.V1.Ledger.Api.Data to Datum
toPlutusData :: Datum -> Plutus.Data
toPlutusData (Constr i ds) = Plutus.Constr i $ toPlutusData <$> ds
toPlutusData (Map m)       = Plutus.Map $ bimap toPlutusData toPlutusData <$> m
toPlutusData (List ds)     = Plutus.List $ toPlutusData <$> ds
toPlutusData (I i)         = Plutus.I i
toPlutusData (B b)         = Plutus.B b

-- | A collection of assets transferred by a transaction output.
data Assets = Assets
  { ada    :: Lovelace -- ^ The ADA sent by the tx output.
  , tokens :: Tokens   -- ^ Additional tokens sent by the tx output.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | Let's make the instance explicit so we can assume "some" semantics.
instance Ord Assets where
  compare (Assets a1 t1) (Assets a2 t2) = compare (a1, t1) (a2, t2)

instance Semigroup Assets where
  a <> b = Assets
    { ada = on (+) ada a b
    , tokens = on (<>) tokens a b
    }

instance Monoid Assets where
  mempty = Assets 0 mempty

-- | A collection of token quantities by their asset ID.
newtype Tokens = Tokens { unTokens :: Map AssetId Quantity }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary, ToJSON, Variations)

instance Semigroup Tokens where
  (<>) = fmap Tokens . on (Map.unionWith (+)) unTokens

instance Monoid Tokens where
  mempty = Tokens mempty

-- | A newtype wrapper for parsing base 16 strings as byte strings.
newtype Base16 = Base16 { unBase16 :: ByteString }

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

newtype DatumHash = DatumHash { unDatumHash :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations)
  deriving (IsString, Show, ToJSON) via Base16

newtype TxId = TxId { unTxId :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations)
  deriving (IsString, Show, ToJSON, ToJSONKey) via Base16

newtype TxIx = TxIx { unTxIx :: Word16 }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary, ToJSON, Variations)

newtype CertIx = CertIx { unCertIx :: Word64 }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary, Variations)

data TxOutRef = TxOutRef
  { txId :: TxId
  , txIx :: TxIx
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, ToJSONKey, Variations)

instance IsString TxOutRef where
  fromString = fromJust . parseTxOutRef . T.pack

parseTxOutRef :: Text -> Maybe TxOutRef
parseTxOutRef val = case T.splitOn "#" val of
  [txId, txIx] -> TxOutRef
    <$> (TxId <$> either (const Nothing) Just (decodeBase16 . encodeUtf8 $ txId))
    <*> (TxIx <$> readMaybe (T.unpack txIx))
  _ -> Nothing

renderTxOutRef :: TxOutRef -> Text
renderTxOutRef TxOutRef{..} = mconcat
  [ encodeBase16 $ unTxId txId
  , "#"
  , T.pack $ show $ unTxIx txIx
  ]

newtype SlotNo = SlotNo { unSlotNo :: Word64 }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary, ToJSON, Variations)

newtype BlockNo = BlockNo { unBlockNo :: Word64 }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary, ToJSON, Variations)

newtype BlockHeaderHash = BlockHeaderHash { unBlockHeaderHash :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations)
  deriving (IsString, Show, ToJSON) via Base16

data AssetId = AssetId
  { policyId  :: PolicyId
  , tokenName :: TokenName
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, ToJSONKey, Variations)

newtype PolicyId = PolicyId { unPolicyId :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations)
  deriving (IsString, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey) via Base16

newtype TokenName = TokenName { unTokenName :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, IsString, Binary, Variations)

instance ToJSONKey TokenName where
  toJSONKey = toJSONKeyText $ T.pack . BS.unpack . unTokenName

instance ToJSON TokenName where
  toJSON = Aeson.String . T.pack . BS.unpack . unTokenName

instance FromJSON TokenName where
  parseJSON = Aeson.withText "TokenName" (pure . TokenName . BS.pack . T.unpack)

instance FromJSONKey TokenName where
  fromJSONKey = FromJSONKeyText (TokenName . BS.pack . T.unpack)

newtype Quantity = Quantity { unQuantity :: Word64 }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary, ToJSON, Variations)

newtype Lovelace = Lovelace { unLovelace :: Word64 }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary, ToJSON, Variations)

newtype Address = Address { unAddress :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations)
  deriving (IsString, Show, ToJSON, FromJSON) via Base16

toBech32 :: Address -> Maybe Text
toBech32 = toCardanoAddressAny >=> \case
  Cardano.AddressShelley address ->
    Just $ serialiseToBech32 address
  _ -> Nothing

fromBech32 :: Text -> Maybe Address
fromBech32 = fmap (Address . serialiseToRawBytes)
  . either (const Nothing) Just
  . deserialiseFromBech32 (AsAddress AsShelleyAddr)

fromCardanoShelleyAddress :: Cardano.Address Cardano.ShelleyAddr -> Address
fromCardanoShelleyAddress = Address . serialiseToRawBytes

fromCardanoAddressAny :: Cardano.AddressAny -> Maybe Address
fromCardanoAddressAny = \case
  Cardano.AddressShelley shelleyAddr -> Just $ fromCardanoShelleyAddress shelleyAddr
  _ -> Nothing

toCardanoAddressAny :: Address -> Maybe Cardano.AddressAny
toCardanoAddressAny = Cardano.deserialiseFromRawBytes Cardano.AsAddressAny . unAddress

paymentCredential :: Address -> Maybe Credential
paymentCredential = toCardanoAddressAny >=> \case
  Cardano.AddressShelley (Cardano.ShelleyAddress _ credential _) ->
    Just $ fromCardanoPaymentCredential $ Cardano.fromShelleyPaymentCredential credential
  _ -> Nothing

stakeReference :: Address -> Maybe StakeReference
stakeReference = toCardanoAddressAny >=> \case
  Cardano.AddressShelley (Cardano.ShelleyAddress _ _ ref) ->
    fromCardanoStakeAddressReference $ Cardano.fromShelleyStakeReference ref
  _ -> Nothing

data Credential
  = PaymentKeyCredential PaymentKeyHash
  | ScriptCredential ScriptHash
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

data StakeCredential
  = StakeKeyCredential StakeKeyHash
  | StakeScriptCredential ScriptHash
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

fromCardanoPaymentCredential :: Cardano.PaymentCredential -> Credential
fromCardanoPaymentCredential = \case
  Cardano.PaymentCredentialByKey pkh           -> PaymentKeyCredential $ fromCardanoPaymentKeyHash pkh
  Cardano.PaymentCredentialByScript scriptHash -> ScriptCredential $ fromCardanoScriptHash scriptHash

newtype PaymentKeyHash = PaymentKeyHash { unPaymentKeyHash :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations)
  deriving (IsString, Show, ToJSON) via Base16

newtype StakeKeyHash = StakeKeyHash { unStakeKeyHash :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations)
  deriving (IsString, Show, ToJSON) via Base16

fromCardanoPaymentKeyHash :: Cardano.Hash Cardano.PaymentKey -> PaymentKeyHash
fromCardanoPaymentKeyHash = PaymentKeyHash . Cardano.serialiseToRawBytes

fromCardanoStakeKeyHash :: Cardano.Hash Cardano.StakeKey -> StakeKeyHash
fromCardanoStakeKeyHash = StakeKeyHash . Cardano.serialiseToRawBytes

newtype ScriptHash = ScriptHash { unScriptHash :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving (IsString, Show, ToJSON) via Base16
  deriving anyclass (Binary, Variations)

policyIdToScriptHash :: PolicyId -> ScriptHash
policyIdToScriptHash (PolicyId h) = ScriptHash h

fromCardanoScriptHash :: Cardano.ScriptHash -> ScriptHash
fromCardanoScriptHash = ScriptHash . Cardano.serialiseToRawBytes

newtype PlutusScript = PlutusScript { unPlutusScript :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving (IsString, Show, ToJSON) via Base16
  deriving anyclass (Binary, Variations)

data StakeReference
  = StakeCredential StakeCredential
  | StakePointer SlotNo TxIx CertIx
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Variations)

fromCardanoStakeAddressReference :: Cardano.StakeAddressReference -> Maybe StakeReference
fromCardanoStakeAddressReference = \case
  Cardano.NoStakeAddress -> Nothing
  Cardano.StakeAddressByValue credential -> Just $ StakeCredential $ fromCardanoStakeCredential credential
  Cardano.StakeAddressByPointer (Cardano.StakeAddressPointer ptr) -> Just $ StakePointer
    (SlotNo $ Cardano.unSlotNo $ ptrSlotNo ptr)
    (let Base.TxIx txIx = ptrTxIx ptr in TxIx $ fromIntegral txIx)
    (let Base.CertIx certIx = ptrCertIx ptr in CertIx certIx)

fromCardanoStakeAddressPointer :: Cardano.StakeAddressPointer -> Word64
fromCardanoStakeAddressPointer = error "not implemented"

fromCardanoStakeCredential :: Cardano.StakeCredential -> StakeCredential
fromCardanoStakeCredential = \case
  Cardano.StakeCredentialByKey skh           -> StakeKeyCredential $ fromCardanoStakeKeyHash skh
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

-- | The 'query' type for the Marlowe Chain Sync.
data Move err result where

  -- | Advance a fixed number of blocks without collecting any results..
  AdvanceBlocks :: Natural -> Move Void ()

  -- | Jump to the latest intersection from a list of known block headers
  -- without collecting any results.
  Intersect :: [BlockHeader] -> Move IntersectError ()

  -- | Advance to the block when a tx out is consumed and collect the tx that
  -- consumes the tx out.
  FindConsumingTxs :: Set TxOutRef -> Move (Map TxOutRef UTxOError) (Map TxOutRef Transaction)

  -- | Advance to the block containing a transaction. The boolean flag
  -- indicates whether or not to wait for the transaction to be produced.
  FindTx :: TxId -> Bool -> Move TxError Transaction

  -- | Advance to the block containing transactions that send or consume outputs
  -- to any addresses with the requested credentials.
  FindTxsFor :: NESet Credential -> Move Void (Set Transaction)

  -- | Advances to the tip block. Waits if already at the tip.
  AdvanceToTip :: Move Void ()

instance HasSignature Move where
  signature _ = "Move"

deriving instance Show (Move err result)
deriving instance Eq (Move err result)
deriving instance Ord (Move err result)

instance ChainSeek.QueryVariations Move where
  tags = NE.fromList
    [ ChainSeek.SomeTag TagAdvanceBlocks
    , ChainSeek.SomeTag TagIntersect
    , ChainSeek.SomeTag TagFindConsumingTxs
    , ChainSeek.SomeTag TagFindTx
    , ChainSeek.SomeTag TagFindTxsFor
    , ChainSeek.SomeTag TagAdvanceToTip
    ]
  queryVariations = \case
    TagAdvanceBlocks -> AdvanceBlocks <$> variations
    TagIntersect -> Intersect <$> variations
    TagFindConsumingTxs -> FindConsumingTxs <$> variations
    TagFindTx -> FindTx <$> variations `varyAp` variations
    TagFindTxsFor -> FindTxsFor <$> variations
    TagAdvanceToTip -> pure AdvanceToTip
  errVariations = \case
    TagAdvanceBlocks -> []
    TagIntersect -> NE.toList variations
    TagFindConsumingTxs -> NE.toList variations
    TagFindTx -> NE.toList variations
    TagFindTxsFor -> []
    TagAdvanceToTip -> []
  resultVariations = \case
    TagAdvanceBlocks -> variations
    TagIntersect -> variations
    TagFindConsumingTxs -> variations
    TagFindTx -> variations
    TagFindTxsFor -> variations
    TagAdvanceToTip -> variations

instance QueryToJSON Move where
  queryToJSON = \case
    AdvanceBlocks offset -> object [ "advanceBlocks" .= toJSON offset ]
    Intersect blocks -> object [ "intersect" .= toJSON blocks ]
    FindConsumingTxs refs -> object [ "findConsumingTxs" .= toJSON refs ]
    FindTx txId wait -> object
      [ "findTx" .= object
        [ "txId" .= toJSON txId
        , "wait" .= toJSON wait
        ]
      ]
    AdvanceToTip -> String "advanceToTip"
    FindTxsFor c -> object [ "findTxsFor" .= toJSON c ]
  errToJSON = \case
    TagAdvanceBlocks -> toJSON
    TagIntersect -> toJSON
    TagFindConsumingTxs -> toJSON
    TagFindTx -> toJSON
    TagFindTxsFor -> toJSON
    TagAdvanceToTip -> toJSON
  resultToJSON = \case
    TagAdvanceBlocks -> toJSON
    TagIntersect -> toJSON
    TagFindConsumingTxs -> toJSON
    TagFindTx -> toJSON
    TagFindTxsFor -> toJSON
    TagAdvanceToTip -> toJSON

type RuntimeChainSeek = ChainSeek Move ChainPoint ChainPoint

type RuntimeChainSeekClient = ChainSeekClient Move ChainPoint ChainPoint

type RuntimeChainSeekServer = ChainSeekServer Move ChainPoint ChainPoint

instance Query Move where
  data Tag Move err result where
    TagAdvanceBlocks :: Tag Move Void ()
    TagIntersect :: Tag Move IntersectError ()
    TagFindTx :: Tag Move TxError Transaction
    TagFindConsumingTxs :: Tag Move (Map TxOutRef UTxOError) (Map TxOutRef Transaction)
    TagFindTxsFor :: Tag Move Void (Set Transaction)
    TagAdvanceToTip :: Tag Move Void ()

  tagFromQuery = \case
    AdvanceBlocks _    -> TagAdvanceBlocks
    Intersect _        -> TagIntersect
    FindTx _ _         -> TagFindTx
    FindConsumingTxs _ -> TagFindConsumingTxs
    FindTxsFor _        -> TagFindTxsFor
    AdvanceToTip       -> TagAdvanceToTip

  tagEq = curry \case
    (TagAdvanceBlocks, TagAdvanceBlocks)     -> Just (Refl, Refl)
    -- Please don't refactor this to use a single catch-all wildcard pattern.
    -- The idea of doing it this way is to cause an incomplete pattern match
    -- warning when a new 'Tag' constructor is added.
    (TagAdvanceBlocks, _)                    -> Nothing
    (TagIntersect, TagIntersect)             -> Just (Refl, Refl)
    (TagIntersect, _)                        -> Nothing
    (TagFindTx, TagFindTx)                   -> Just (Refl, Refl)
    (TagFindTx, _)                           -> Nothing
    (TagFindConsumingTxs, TagFindConsumingTxs) -> Just (Refl, Refl)
    (TagFindConsumingTxs, _)                  -> Nothing
    (TagFindTxsFor, TagFindTxsFor)                   -> Just (Refl, Refl)
    (TagFindTxsFor, _)                           -> Nothing
    (TagAdvanceToTip, TagAdvanceToTip)                   -> Just (Refl, Refl)
    (TagAdvanceToTip, _)                           -> Nothing

  putTag = \case
    TagAdvanceBlocks -> putWord8 0x01
    TagIntersect -> putWord8 0x02
    TagFindTx -> putWord8 0x03
    TagFindConsumingTxs -> putWord8 0x04
    TagAdvanceToTip -> putWord8 0x05
    TagFindTxsFor -> putWord8 0x06

  putQuery = \case
    AdvanceBlocks blocks -> put blocks
    Intersect points -> put points
    FindTx txId wait -> put txId *> put wait
    FindConsumingTxs utxos -> put utxos
    AdvanceToTip -> mempty
    FindTxsFor credentials -> put $ NESet.toList credentials

  getTag = do
    tag <- getWord8
    case tag of
      0x01 -> pure $ SomeTag TagAdvanceBlocks
      0x02 -> pure $ SomeTag TagIntersect
      0x03 -> pure $ SomeTag TagFindTx
      0x04 -> pure $ SomeTag TagFindConsumingTxs
      0x05 -> pure $ SomeTag TagAdvanceToTip
      0x06 -> pure $ SomeTag TagFindTxsFor
      _ -> fail $ "Invalid move tag " <> show tag

  getQuery = \case
    TagAdvanceBlocks    -> AdvanceBlocks <$> get
    TagIntersect        -> Intersect <$> get
    TagFindTx           -> FindTx <$> get <*> get
    TagFindConsumingTxs -> FindConsumingTxs <$> get
    TagAdvanceToTip     -> pure AdvanceToTip
    TagFindTxsFor        -> FindTxsFor . NESet.fromList <$> get

  putResult = \case
    TagAdvanceBlocks -> mempty
    TagFindTx -> put
    TagIntersect -> mempty
    TagFindConsumingTxs -> put
    TagAdvanceToTip -> mempty
    TagFindTxsFor -> put

  getResult = \case
    TagAdvanceBlocks -> pure ()
    TagFindTx -> get
    TagIntersect -> pure ()
    TagFindConsumingTxs -> get
    TagAdvanceToTip -> pure ()
    TagFindTxsFor -> get

  putErr = \case
    TagAdvanceBlocks -> put
    TagFindTx -> put
    TagIntersect -> put
    TagFindConsumingTxs -> put
    TagFindTxsFor -> put
    TagAdvanceToTip -> put

  getErr = \case
    TagAdvanceBlocks -> get
    TagFindTx -> get
    TagIntersect -> get
    TagFindConsumingTxs -> get
    TagFindTxsFor -> get
    TagAdvanceToTip -> get

putUTCTime :: UTCTime -> Put
putUTCTime UTCTime{..} = do
  let (year, dayOfYear) = toOrdinalDate utctDay
  put year
  put dayOfYear
  put $ diffTimeToPicoseconds utctDayTime

getUTCTime :: Get UTCTime
getUTCTime  = do
  year <- get
  dayOfYear <- get
  utctDayTime <- picosecondsToDiffTime <$> get
  utctDay <- case fromOrdinalDateValid year dayOfYear of
    Nothing -> fail "Invalid ISO 8601 ordinal date"
    Just a  -> pure a
  pure UTCTime{..}

data GetUTxOsQuery
  = GetUTxOsAtAddresses (Set Address)
  | GetUTxOsForTxOutRefs (Set TxOutRef)
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass Variations

-- Semigroup and Monoid seem to be safe - we cover here a subset of a partial function.
newtype UTxOs = UTxOs { unUTxOs :: Map TxOutRef TransactionOutput }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Semigroup, Monoid, Variations)
  deriving anyclass (Binary, ToJSON)

lookupUTxO :: TxOutRef -> UTxOs -> Maybe TransactionOutput
lookupUTxO txOutRef (UTxOs utxos) = Map.lookup txOutRef utxos

toUTxOsList :: UTxOs -> [UTxO]
toUTxOsList (UTxOs (Map.toList -> utxos)) = fmap (uncurry UTxO) utxos

data UTxO = UTxO
  { txOutRef :: TxOutRef
  , transactionOutput ::  TransactionOutput
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Variations)

toUTxOTuple :: UTxO -> (TxOutRef, TransactionOutput)
toUTxOTuple (UTxO txOutRef transactionOutput) = (txOutRef, transactionOutput)

data ChainSyncQuery delimiter err result where
  GetSecurityParameter :: ChainSyncQuery Void () Int
  GetNetworkId :: ChainSyncQuery Void () NetworkId
  GetProtocolParameters :: ChainSyncQuery Void () ProtocolParameters
  GetSystemStart :: ChainSyncQuery Void () SystemStart
  GetEraHistory :: ChainSyncQuery Void () (EraHistory CardanoMode)
  GetUTxOs :: GetUTxOsQuery -> ChainSyncQuery Void () UTxOs

instance HasSignature ChainSyncQuery where
  signature _ = "ChainSyncQuery"

instance Query.QueryToJSON ChainSyncQuery where
  queryToJSON = \case
    GetSecurityParameter -> String "get-security-parameter"
    GetNetworkId -> String "get-network-id"
    GetProtocolParameters -> String "get-protocol-parameters"
    GetSystemStart -> String "get-system-start"
    GetEraHistory -> String "get-era-history"
    GetUTxOs subQuery -> object
      [ "get-utxos" .= case subQuery of
          GetUTxOsAtAddresses addresses -> object
            [ "at-address" .= addresses
            ]
          GetUTxOsForTxOutRefs txOutRefs -> object
            [ "for-tx-out-refs" .= txOutRefs
            ]
      ]
  errToJSON = \case
    TagGetSecurityParameter -> toJSON
    TagGetNetworkId -> toJSON
    TagGetProtocolParameters -> toJSON
    TagGetSystemStart -> toJSON
    TagGetEraHistory -> toJSON
    TagGetUTxOs -> toJSON
  resultToJSON = \case
    TagGetSecurityParameter -> toJSON
    TagGetNetworkId -> \case
      Mainnet -> String "mainnet"
      Testnet (NetworkMagic n) -> object ["testnet" .= n]
    TagGetProtocolParameters -> toJSON
    TagGetSystemStart -> toJSON
    TagGetEraHistory -> const $ String "<era-history>"
    TagGetUTxOs -> toJSON
  delimiterToJSON = \case
    TagGetSecurityParameter -> toJSON
    TagGetNetworkId -> toJSON
    TagGetProtocolParameters -> toJSON
    TagGetSystemStart -> toJSON
    TagGetEraHistory -> toJSON
    TagGetUTxOs -> toJSON

instance Query.QueryVariations ChainSyncQuery where
  tags = NE.fromList
    [ Query.SomeTag TagGetSecurityParameter
    , Query.SomeTag TagGetNetworkId
    , Query.SomeTag TagGetProtocolParameters
    , Query.SomeTag TagGetSystemStart
    , Query.SomeTag TagGetEraHistory
    , Query.SomeTag TagGetUTxOs
    ]
  queryVariations = \case
    TagGetSecurityParameter -> pure GetSecurityParameter
    TagGetNetworkId -> pure GetNetworkId
    TagGetProtocolParameters -> pure GetProtocolParameters
    TagGetSystemStart -> pure GetSystemStart
    TagGetEraHistory -> pure GetEraHistory
    TagGetUTxOs -> GetUTxOs <$> variations
  errVariations = \case
    TagGetSecurityParameter -> NE.toList variations
    TagGetNetworkId -> NE.toList variations
    TagGetProtocolParameters -> NE.toList variations
    TagGetSystemStart -> NE.toList variations
    TagGetEraHistory -> NE.toList variations
    TagGetUTxOs -> NE.toList variations
  delimiterVariations = \case
    TagGetSecurityParameter -> []
    TagGetNetworkId -> []
    TagGetProtocolParameters -> []
    TagGetSystemStart -> []
    TagGetEraHistory -> []
    TagGetUTxOs -> []
  resultsVariations = \case
    TagGetSecurityParameter -> variations
    TagGetNetworkId -> Mainnet NE.:| [Testnet $ NetworkMagic 0]
    TagGetProtocolParameters -> variations
    TagGetSystemStart -> SystemStart <$> variations
    TagGetEraHistory -> variations
    TagGetUTxOs -> variations

instance Query.IsQuery ChainSyncQuery where
  data Tag ChainSyncQuery delimiter err result where
    TagGetSecurityParameter :: Query.Tag ChainSyncQuery Void () Int
    TagGetNetworkId :: Query.Tag ChainSyncQuery Void () NetworkId
    TagGetProtocolParameters :: Query.Tag ChainSyncQuery Void () ProtocolParameters
    TagGetSystemStart :: Query.Tag ChainSyncQuery Void () SystemStart
    TagGetEraHistory :: Query.Tag ChainSyncQuery Void () (EraHistory CardanoMode)
    TagGetUTxOs :: Query.Tag ChainSyncQuery Void () UTxOs
  tagEq TagGetSecurityParameter TagGetSecurityParameter = Just (Refl, Refl, Refl)
  tagEq TagGetSecurityParameter _                       = Nothing
  tagEq TagGetNetworkId TagGetNetworkId = Just (Refl, Refl, Refl)
  tagEq TagGetNetworkId _                       = Nothing
  tagEq TagGetProtocolParameters TagGetProtocolParameters = Just (Refl, Refl, Refl)
  tagEq TagGetProtocolParameters _                       = Nothing
  tagEq TagGetEraHistory TagGetEraHistory = Just (Refl, Refl, Refl)
  tagEq TagGetEraHistory _                       = Nothing
  tagEq TagGetSystemStart TagGetSystemStart = Just (Refl, Refl, Refl)
  tagEq TagGetSystemStart _                       = Nothing
  tagEq TagGetUTxOs TagGetUTxOs = Just (Refl, Refl, Refl)
  tagEq TagGetUTxOs _ = Nothing
  putTag = \case
    TagGetSecurityParameter -> putWord8 0x01
    TagGetNetworkId -> putWord8 0x02
    TagGetProtocolParameters -> putWord8 0x03
    TagGetSystemStart -> putWord8 0x04
    TagGetEraHistory -> putWord8 0x05
    TagGetUTxOs -> putWord8 0x06
  getTag = do
    word <- getWord8
    case word of
      0x01 -> pure $ Query.SomeTag TagGetSecurityParameter
      0x02 -> pure $ Query.SomeTag TagGetNetworkId
      0x03 -> pure $ Query.SomeTag TagGetProtocolParameters
      0x04 -> pure $ Query.SomeTag TagGetSystemStart
      0x05 -> pure $ Query.SomeTag TagGetEraHistory
      0x06 -> pure $ Query.SomeTag TagGetUTxOs
      _    -> fail "Invalid ChainSyncQuery tag"
  putQuery = \case
    GetSecurityParameter -> mempty
    GetNetworkId -> mempty
    GetProtocolParameters -> mempty
    GetSystemStart -> mempty
    GetEraHistory -> mempty
    GetUTxOs (GetUTxOsAtAddresses addresses) -> do
      putWord8 0x01
      put addresses
    GetUTxOs (GetUTxOsForTxOutRefs txOutRefs) -> do
      putWord8 0x02
      put txOutRefs
  getQuery = \case
    TagGetSecurityParameter -> pure GetSecurityParameter
    TagGetNetworkId -> pure GetNetworkId
    TagGetProtocolParameters -> pure GetProtocolParameters
    TagGetSystemStart -> pure GetSystemStart
    TagGetEraHistory -> pure GetEraHistory
    TagGetUTxOs -> do
      word <- getWord8
      GetUTxOs <$> case word of
        0x01 -> do
          GetUTxOsAtAddresses <$> get
        0x02 -> do
          GetUTxOsForTxOutRefs <$> get
        _    -> fail "Invalid GetUTxOsQuery tag"
  putDelimiter = \case
    TagGetSecurityParameter -> absurd
    TagGetNetworkId -> absurd
    TagGetProtocolParameters -> absurd
    TagGetSystemStart -> absurd
    TagGetEraHistory -> absurd
    TagGetUTxOs -> absurd
  getDelimiter = \case
    TagGetSecurityParameter -> fail "no delimiter defined"
    TagGetNetworkId -> fail "no delimiter defined"
    TagGetProtocolParameters -> fail "no delimiter defined"
    TagGetSystemStart -> fail "no delimiter defined"
    TagGetEraHistory -> fail "no delimiter defined"
    TagGetUTxOs -> fail "no delimiter defined"
  putErr = \case
    TagGetSecurityParameter -> put
    TagGetNetworkId -> put
    TagGetProtocolParameters -> put
    TagGetSystemStart -> put
    TagGetEraHistory -> put
    TagGetUTxOs -> put
  getErr = \case
    TagGetSecurityParameter -> get
    TagGetNetworkId -> get
    TagGetProtocolParameters -> get
    TagGetSystemStart -> get
    TagGetEraHistory -> get
    TagGetUTxOs -> get
  putResult = \case
    TagGetSecurityParameter -> put
    TagGetNetworkId -> put . \case
      Mainnet -> Nothing
      Testnet (NetworkMagic magic) -> Just magic
    TagGetProtocolParameters -> put . Aeson.encode
    TagGetEraHistory -> \case
      EraHistory _ interpreter -> put $ serialise interpreter
    TagGetSystemStart -> \case
      SystemStart start -> putUTCTime start
    TagGetUTxOs -> put
  getResult = \case
    TagGetSecurityParameter -> get
    TagGetNetworkId -> maybe Mainnet (Testnet . NetworkMagic) <$> get
    TagGetProtocolParameters -> do
      bytes <- get
      case Aeson.decode bytes of
        Nothing -> fail "failed to decode protocol parameters JSON"
        Just params -> pure params
    TagGetEraHistory -> do
      bytes <- get
      case deserialiseOrFail bytes of
        Left err -> fail $ show err
        Right interpreter -> pure $ EraHistory CardanoMode interpreter
    TagGetSystemStart -> SystemStart <$> getUTCTime
    TagGetUTxOs -> get
  tagFromQuery = \case
    GetSecurityParameter -> TagGetSecurityParameter
    GetNetworkId -> TagGetNetworkId
    GetProtocolParameters -> TagGetProtocolParameters
    GetEraHistory -> TagGetEraHistory
    GetSystemStart -> TagGetSystemStart
    GetUTxOs _ -> TagGetUTxOs

data ChainSyncCommand status err result where
  SubmitTx :: ScriptDataSupportedInEra era -> Tx era -> ChainSyncCommand Void String ()

instance HasSignature ChainSyncCommand where
  signature _ = "ChainSyncCommand"

instance CommandToJSON ChainSyncCommand where
  commandToJSON = \case
    SubmitTx _ tx -> object [ "submit-tx" .= show tx ]
  jobIdToJSON = \case
  errToJSON = \case
    TagSubmitTx _ -> toJSON
  resultToJSON = \case
    TagSubmitTx _ -> toJSON
  statusToJSON = \case
    TagSubmitTx _ -> toJSON

instance Job.Command ChainSyncCommand where
  data Tag ChainSyncCommand status err result where
    TagSubmitTx :: ScriptDataSupportedInEra era -> Job.Tag ChainSyncCommand Void String ()

  data JobId ChainSyncCommand status err result where

  tagFromCommand = \case
    SubmitTx era _ -> TagSubmitTx era
  tagFromJobId = \case
  tagEq (TagSubmitTx era1) (TagSubmitTx era2) = do
    Refl <- eraEq era1 era2
    pure (Refl, Refl, Refl)
  putTag = \case
    TagSubmitTx era -> do
      putWord8 0x01
      case era of
        ScriptDataInAlonzoEra -> putWord8 0x01
        ScriptDataInBabbageEra -> putWord8 0x02
  getTag = getWord8 >>= \case
    0x01 -> getWord8 >>= \case
      0x01 -> pure $ Job.SomeTag $ TagSubmitTx ScriptDataInAlonzoEra
      0x02 -> pure $ Job.SomeTag $ TagSubmitTx ScriptDataInBabbageEra
      tag ->  fail $ "invalid era tag " <> show tag
    tag -> fail $ "invalid command tag " <> show tag
  putJobId = \case
  getJobId = \case
    TagSubmitTx _ -> fail "SubmitTx does not support job IDs"
  putCommand = \case
    SubmitTx ScriptDataInAlonzoEra tx -> put $ serialiseToCBOR tx
    SubmitTx ScriptDataInBabbageEra tx -> put $ serialiseToCBOR tx
  getCommand = \case
    TagSubmitTx era -> SubmitTx era <$> do
      bytes <- get @ByteString
      case era of
        ScriptDataInAlonzoEra -> case deserialiseFromCBOR (AsTx AsAlonzo) bytes of
          Left err -> fail $ show err
          Right tx -> pure tx
        ScriptDataInBabbageEra -> case deserialiseFromCBOR (AsTx AsBabbage) bytes of
          Left err -> fail $ show err
          Right tx -> pure tx
  putStatus = \case
    TagSubmitTx _ -> absurd
  getStatus = \case
    TagSubmitTx _ -> fail "SubmitTx does not support job statuses"
  putErr = \case
    TagSubmitTx _ -> put
  getErr = \case
    TagSubmitTx _ -> get
  putResult = \case
    TagSubmitTx _ -> mempty
  getResult = \case
    TagSubmitTx _ -> pure ()

eraEq :: ScriptDataSupportedInEra era1 -> ScriptDataSupportedInEra era2 -> Maybe (era1 :~: era2)
eraEq ScriptDataInAlonzoEra ScriptDataInAlonzoEra   = Just Refl
eraEq ScriptDataInAlonzoEra _                       = Nothing
eraEq ScriptDataInBabbageEra ScriptDataInBabbageEra = Just Refl
eraEq ScriptDataInBabbageEra _                      = Nothing

-- * Orphan instances

instance Variations ProtocolParameters where

instance Variations C.PraosNonce where
  variations = C.makePraosNonce <$> variations

instance Variations C.Lovelace where
  variations = C.Lovelace <$> variations

instance Variations C.EpochNo where

instance Variations C.AnyPlutusScriptVersion where
  variations = NE.fromList
    [ C.AnyPlutusScriptVersion C.PlutusScriptV1
    , C.AnyPlutusScriptVersion C.PlutusScriptV2
    ]

instance Variations C.CostModel where
  variations = C.CostModel <$> variations

instance Variations C.ExecutionUnitPrices where
  variations = C.ExecutionUnitPrices <$> variations `varyAp` variations

instance Variations C.ExecutionUnits where
  variations = C.ExecutionUnits <$> variations `varyAp` variations

instance Variations (C.EraHistory CardanoMode) where
  variations = C.EraHistory C.CardanoMode <$> variations

instance Variations (Counting.NonEmpty xs EraSummary) => Variations (Interpreter xs) where
  variations = mkInterpreter <$> variations

instance Variations (Counting.NonEmpty xs EraSummary) => Variations (Summary xs) where
  variations = Summary <$> variations

instance {-# OVERLAPPING #-} Variations a => Variations (Counting.NonEmpty '[x] a) where
  variations = Counting.NonEmptyOne <$> variations

instance {-# OVERLAPPING #-} (Variations a, Variations (Counting.NonEmpty xs a)) => Variations (Counting.NonEmpty (x ': xs) a) where
  variations = Counting.NonEmptyCons <$> variations `varyAp` pure (NE.head variations)

instance Variations EraSummary where

instance Variations EraParams where

instance Variations SafeZone where

instance Variations EpochSize where

instance Variations SlotLength where

instance Variations EraEnd where

instance Variations Bound where

instance Variations Cardano.SlotNo where

instance Variations RelativeTime where
