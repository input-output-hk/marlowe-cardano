{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.ChainSync.Api
  ( Address(..)
  , AssetId(..)
  , Assets(..)
  , BlockHeader(..)
  , BlockHeaderHash(..)
  , BlockNo(..)
  , CertIx(..)
  , ChainPoint
  , ChainSyncQuery(..)
  , Credential(..)
  , Datum(..)
  , DatumHash(..)
  , IntersectError(..)
  , Lovelace(..)
  , Metadata
  , Move(..)
  , module Network.Protocol.ChainSeek.Client
  , module Network.Protocol.ChainSeek.Codec
  , module Network.Protocol.ChainSeek.Server
  , module Network.Protocol.ChainSeek.Types
  , PaymentKeyHash(..)
  , PolicyId(..)
  , Quantity(..)
  , Redeemer(..)
  , RuntimeChainSeek
  , RuntimeChainSeekClient
  , RuntimeChainSeekCodec
  , RuntimeChainSeekServer
  , ScriptHash(..)
  , SlotConfig(..)
  , SlotNo(..)
  , StakeReference(..)
  , TokenName(..)
  , Tokens(..)
  , Transaction(..)
  , TransactionInput(..)
  , TransactionOutput(..)
  , TxError(..)
  , TxId(..)
  , TxIx(..)
  , TxOutRef(..)
  , UTxOError(..)
  , ValidityRange(..)
  , WithGenesis(..)
  , fromBech32
  , fromCardanoStakeAddressPointer
  , fromDatum
  , fromPlutusData
  , fromRedeemer
  , getUTCTime
  , isAfter
  , moveSchema
  , paymentCredential
  , putUTCTime
  , runtimeChainSeekCodec
  , slotToUTCTime
  , stakeReference
  , toBech32
  , toCardanoAddress
  , toDatum
  , toPlutusData
  , toRedeemer
  ) where

import Cardano.Api (AsType(..), SerialiseAsRawBytes(serialiseToRawBytes), deserialiseFromBech32, serialiseToBech32)
import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.BaseTypes as Base
import Cardano.Ledger.Credential (ptrCertIx, ptrSlotNo, ptrTxIx)
import Control.Monad ((>=>))
import Data.Bifunctor (bimap)
import Data.Binary (Binary(..), Get, Put, get, getWord8, put, putWord8)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16, encodeBase16)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import Data.Set (Set)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.These (These(..))
import Data.Time
  ( NominalDiffTime
  , UTCTime(..)
  , addUTCTime
  , diffTimeToPicoseconds
  , nominalDiffTimeToSeconds
  , picosecondsToDiffTime
  , secondsToNominalDiffTime
  )
import Data.Time.Calendar.OrdinalDate (fromOrdinalDateValid, toOrdinalDate)
import Data.Type.Equality (type (:~:)(Refl))
import Data.Void (Void, absurd)
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Network.Protocol.ChainSeek.Client
import Network.Protocol.ChainSeek.Codec
import Network.Protocol.ChainSeek.Server
import Network.Protocol.ChainSeek.TH (mkSchemaVersion)
import Network.Protocol.ChainSeek.Types
import qualified Network.Protocol.Query.Types as Query
import Network.TypedProtocol.Codec (Codec)
import qualified Plutus.V1.Ledger.Api as Plutus

-- | Extends a type with a "Genesis" member.
data WithGenesis a = Genesis | At a
  deriving stock (Show, Eq, Ord, Functor, Generic)
  deriving anyclass (Binary)

-- | A point in the chain, identified by a slot number, block header hash, and
-- block number.
type ChainPoint = WithGenesis BlockHeader

-- | A block header, consisting of a slot number, a hash, and a block number.
data BlockHeader = BlockHeader
  { slotNo     :: !SlotNo          -- ^ The slot number when this block was produced.
  , headerHash :: !BlockHeaderHash -- ^ The hash of this block's header.
  , blockNo    :: !BlockNo         -- ^ The ordinal number of this block.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

isAfter :: SlotNo -> BlockHeader -> Bool
isAfter s BlockHeader{..} = slotNo > s

-- | A transaction body
data Transaction = Transaction
  { txId          :: !TxId                   -- ^ The hash of this transaction.
  , validityRange :: !ValidityRange          -- ^ The range of slots during which this transaction is valid.
  , metadata      :: !(Maybe Metadata)       -- ^ The metadata of this transaction (only includes Marlowe-relevant metadata).
  , inputs        :: !(Set TransactionInput) -- ^ The inputs consumed by the transaction
  , outputs       :: ![TransactionOutput]    -- ^ The outputs produced by the transaction.
  , mintedTokens  :: !Tokens                 -- ^ Tokens minted by the transaction.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- | A validity range for a transaction
data ValidityRange
  = Unbounded                 -- ^ The transaction is always valid.
  | MinBound SlotNo           -- ^ The transaction is only valid after a specific slot.
  | MaxBound SlotNo           -- ^ The transaction is only valid before a specific slot.
  | MinMaxBound SlotNo SlotNo -- ^ The transaction is only valid between two slots.
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- TODO add content
data Metadata
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- | An input of a transaction.
data TransactionInput = TransactionInput
  { txId       :: !TxId             -- ^ The txId of the TransactionOutput this input consumes.
  , txIx       :: !TxIx             -- ^ The txIx of the TransactionOutput this input consumes.
  , address    :: !Address          -- ^ The address of the TransactionOutput this input consumes.
  , datumBytes :: !(Maybe Datum)    -- ^ The script datum for this input
  , redeemer   :: !(Maybe Redeemer) -- ^ The script redeemer datum for this input (if one was provided).
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- | An output of a transaction.
data TransactionOutput = TransactionOutput
  { address   :: !Address           -- ^ The address that receives the assets of this output.
  , assets    :: !Assets            -- ^ The assets this output produces.
  , datumHash :: !(Maybe DatumHash) -- ^ The hash of the script datum associated with this output.
  , datum     :: !(Maybe Datum)     -- ^ The script datum associated with this output.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- | A script datum that is used to spend the output of a script tx.
newtype Redeemer = Redeemer { unRedeemer :: Datum }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary)

-- | A datum as a sum-of-products.
data Datum
  = Constr Integer [Datum]
  | Map [(Datum, Datum)]
  | List [Datum]
  | I Integer
  | B ByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

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
  { ada    :: !Lovelace -- ^ The ADA sent by the tx output.
  , tokens :: !Tokens   -- ^ Additional tokens sent by the tx output.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- | A collection of token quantities by their asset ID.
newtype Tokens = Tokens { unTokens :: Map AssetId Quantity }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary, Semigroup, Monoid)

-- | A newtype wrapper for parsing base 16 strings as byte strings.
newtype Base16 = Base16 { unBase16 :: ByteString }

instance Show Base16 where
  show = show . encodeBase16 . unBase16

instance IsString Base16 where
  fromString = either (error . T.unpack) Base16 . decodeBase16 . encodeUtf8 . T.pack

newtype DatumHash = DatumHash { unDatumHash :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary)
  deriving (IsString, Show) via Base16

newtype TxId = TxId { unTxId :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary)
  deriving (IsString, Show) via Base16

newtype TxIx = TxIx { unTxIx :: Word16 }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary)

newtype CertIx = CertIx { unCertIx :: Word64 }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary)

data TxOutRef = TxOutRef
  { txId :: !TxId
  , txIx :: !TxIx
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

newtype SlotNo = SlotNo { unSlotNo :: Word64 }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary)

newtype BlockNo = BlockNo { unBlockNo :: Word64 }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary)

newtype BlockHeaderHash = BlockHeaderHash { unBlockHeaderHash :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary)
  deriving (IsString, Show) via Base16

data AssetId = AssetId
  { policyId  :: !PolicyId
  , tokenName :: !TokenName
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

newtype PolicyId = PolicyId { unPolicyId :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary)
  deriving (IsString, Show) via Base16

newtype TokenName = TokenName { unTokenName :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary)
  deriving (IsString, Show) via Base16

newtype Quantity = Quantity { unQuantity :: Word64 }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary)

newtype Lovelace = Lovelace { unLovelace :: Word64 }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary)

newtype Address = Address { unAddress :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary)
  deriving (IsString, Show) via Base16

toBech32 :: Address -> Maybe Text
toBech32 = toCardanoAddress >=> \case
  Cardano.AddressShelley address ->
    Just $ serialiseToBech32 address
  _ -> Nothing

fromBech32 :: Text -> Maybe Address
fromBech32 = fmap (Address . serialiseToRawBytes)
  . either (const Nothing) Just
  . deserialiseFromBech32 (AsAddress AsShelleyAddr)

toCardanoAddress :: Address -> Maybe Cardano.AddressAny
toCardanoAddress = Cardano.deserialiseFromRawBytes Cardano.AsAddressAny . unAddress

paymentCredential :: Address -> Maybe Credential
paymentCredential = toCardanoAddress >=> \case
  Cardano.AddressShelley (Cardano.ShelleyAddress _ credential _) ->
    Just $ fromCardanoPaymentCredential $ Cardano.fromShelleyPaymentCredential credential
  _ -> Nothing

stakeReference :: Address -> Maybe StakeReference
stakeReference = toCardanoAddress >=> \case
  Cardano.AddressShelley (Cardano.ShelleyAddress _ _ ref) ->
    fromCardanoStakeAddressReference $ Cardano.fromShelleyStakeReference ref
  _ -> Nothing

data Credential
  = PaymentKeyCredential PaymentKeyHash
  | ScriptCredential ScriptHash
  deriving stock (Show, Eq, Ord, Generic)

fromCardanoPaymentCredential :: Cardano.PaymentCredential -> Credential
fromCardanoPaymentCredential = \case
  Cardano.PaymentCredentialByKey pkh           -> PaymentKeyCredential $ fromCardanoPaymentKeyHash pkh
  Cardano.PaymentCredentialByScript scriptHash -> ScriptCredential $ fromCardanoScriptHash scriptHash

newtype PaymentKeyHash = PaymentKeyHash { unPaymentKeyHash :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary)
  deriving (IsString, Show) via Base16

fromCardanoPaymentKeyHash :: Cardano.Hash Cardano.PaymentKey -> PaymentKeyHash
fromCardanoPaymentKeyHash = PaymentKeyHash . Cardano.serialiseToRawBytes

fromCardanoStakeKeyHash :: Cardano.Hash Cardano.StakeKey -> PaymentKeyHash
fromCardanoStakeKeyHash = PaymentKeyHash . Cardano.serialiseToRawBytes

newtype ScriptHash = ScriptHash { unScriptHash :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving (IsString, Show) via Base16
  deriving anyclass (Binary)

fromCardanoScriptHash :: Cardano.ScriptHash -> ScriptHash
fromCardanoScriptHash = ScriptHash . Cardano.serialiseToRawBytes

data StakeReference
  = StakeCredential Credential
  | StakePointer SlotNo TxIx CertIx
  deriving stock (Show, Eq, Ord, Generic)

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

fromCardanoStakeCredential :: Cardano.StakeCredential -> Credential
fromCardanoStakeCredential = \case
  Cardano.StakeCredentialByKey pkh           -> PaymentKeyCredential $ fromCardanoStakeKeyHash pkh
  Cardano.StakeCredentialByScript scriptHash -> ScriptCredential $ fromCardanoScriptHash scriptHash

-- | Reasons a 'FindTx' request can be rejected.
data TxError
  = TxNotFound
  | TxInPast BlockHeader
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- | Reasons a 'FindConsumingTx' request can be rejected.
data UTxOError
  = UTxONotFound
  | UTxOSpent TxId
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- | Reasons an 'Intersect' request can be rejected.
data IntersectError = IntersectionNotFound
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- | The 'query' type for the Marlowe Chain Sync.
data Move err result where

  -- | Perform two moves in parallel and collect the results from the one which
  -- resolves first (or both if they resolve simultaneously).
  Fork
    :: Move err1 result1
    -> Move err2 result2
    -> Move (These err1 err2) (These result1 result2)

  -- | Advance a minimum number of slots without collecting any results..
  AdvanceSlots :: Natural -> Move Void ()

  -- | Advance a fixed number of blocks without collecting any results..
  AdvanceBlocks :: Natural -> Move Void ()

  -- | Jump to the latest intersection from a list of known block headers
  -- without collecting any results.
  Intersect :: [BlockHeader] -> Move IntersectError ()

  -- | Advance to the block when a tx out is consumed and collect the tx that
  -- consumes the tx out.
  FindConsumingTx :: TxOutRef -> Move UTxOError Transaction

  -- | Advance to the block when a tx out is consumed and collect the tx that
  -- consumes the tx out.
  FindConsumingTxs :: Set TxOutRef -> Move (Map TxOutRef UTxOError) (Map TxOutRef Transaction)

  -- | Advance to the block containing a transaction.
  FindTx :: TxId -> Move TxError Transaction

mkSchemaVersion "moveSchema" ''Move

deriving instance Show (Move err result)
deriving instance Eq (Move err result)
deriving instance Ord (Move err result)

type RuntimeChainSeek = ChainSeek Move ChainPoint ChainPoint

type RuntimeChainSeekClient = ChainSeekClient Move ChainPoint ChainPoint

type RuntimeChainSeekServer = ChainSeekServer Move ChainPoint ChainPoint

type RuntimeChainSeekCodec m = Codec RuntimeChainSeek DeserializeError m LBS.ByteString

runtimeChainSeekCodec :: Applicative m => RuntimeChainSeekCodec m
runtimeChainSeekCodec = codecChainSeek

instance Query Move where
  data Tag Move err result where
    TagFork
      :: Tag Move err1 result1
      -> Tag Move err2 result2
      -> Tag Move (These err1 err2) (These result1 result2)
    TagAdvanceSlots :: Tag Move Void ()
    TagAdvanceBlocks :: Tag Move Void ()
    TagIntersect :: Tag Move IntersectError ()
    TagFindConsumingTx :: Tag Move UTxOError Transaction
    TagFindTx :: Tag Move TxError Transaction
    TagFindConsumingTxs :: Tag Move (Map TxOutRef UTxOError) (Map TxOutRef Transaction)

  tagFromQuery = \case
    Fork m1 m2         -> TagFork (tagFromQuery m1) (tagFromQuery m2)
    AdvanceSlots _     -> TagAdvanceSlots
    AdvanceBlocks _    -> TagAdvanceBlocks
    Intersect _        -> TagIntersect
    FindConsumingTx _  -> TagFindConsumingTx
    FindTx _           -> TagFindTx
    FindConsumingTxs _ -> TagFindConsumingTxs

  tagEq = curry \case
    (TagFork m1 m2, TagFork m3 m4)           ->
      case (,) <$> tagEq m1 m3 <*> tagEq m2 m4 of
        Nothing                           -> Nothing
        Just ((Refl, Refl), (Refl, Refl)) -> Just (Refl, Refl)
    -- Please don't refactor this to use a single catch-all wildcard pattern.
    -- The idea of doing it this way is to cause an incomplete pattern match
    -- warning when a new 'Tag' constructor is added.
    (TagFork _ _, _)                         -> Nothing
    (TagAdvanceSlots, TagAdvanceSlots)       -> Just (Refl, Refl)
    (TagAdvanceSlots, _)                     -> Nothing
    (TagAdvanceBlocks, TagAdvanceBlocks)     -> Just (Refl, Refl)
    (TagAdvanceBlocks, _)                    -> Nothing
    (TagIntersect, TagIntersect)             -> Just (Refl, Refl)
    (TagIntersect, _)                        -> Nothing
    (TagFindConsumingTx, TagFindConsumingTx) -> Just (Refl, Refl)
    (TagFindConsumingTx, _)                  -> Nothing
    (TagFindTx, TagFindTx)                   -> Just (Refl, Refl)
    (TagFindTx, _)                           -> Nothing
    (TagFindConsumingTxs, TagFindConsumingTxs) -> Just (Refl, Refl)
    (TagFindConsumingTxs, _)                  -> Nothing

  putTag = \case
    TagFork t1 t2 -> do
      putWord8 0x01
      putTag t1
      putTag t2
    TagAdvanceSlots -> putWord8 0x02
    TagAdvanceBlocks -> putWord8 0x03
    TagFindConsumingTx -> putWord8 0x04
    TagIntersect -> putWord8 0x05
    TagFindTx -> putWord8 0x06
    TagFindConsumingTxs -> putWord8 0x07

  putQuery = \case
    Fork m1 m2 -> do
      putQuery m1
      putQuery m2
    AdvanceSlots slots -> put slots
    AdvanceBlocks blocks -> put blocks
    FindConsumingTx utxo -> put utxo
    Intersect points -> put points
    FindTx txId -> put txId
    FindConsumingTxs utxos -> put utxos

  getTag = do
    tag <- getWord8
    case tag of
      0x01 -> do
        SomeTag m1 <- getTag
        SomeTag m2 <- getTag
        pure $ SomeTag $ TagFork m1 m2
      0x02 -> pure $ SomeTag TagAdvanceSlots
      0x03 -> pure $ SomeTag TagAdvanceBlocks
      0x04 -> pure $ SomeTag TagFindConsumingTx
      0x05 -> pure $ SomeTag TagIntersect
      0x06 -> pure $ SomeTag TagFindTx
      0x07 -> pure $ SomeTag TagFindConsumingTxs
      _ -> fail $ "Invalid move tag " <> show tag

  getQuery = \case
    TagFork t1 t2       -> Fork <$> getQuery t1 <*> getQuery t2
    TagAdvanceSlots     -> AdvanceSlots <$> get
    TagAdvanceBlocks    -> AdvanceBlocks <$> get
    TagFindConsumingTx  -> FindConsumingTx <$> get
    TagIntersect        -> Intersect <$> get
    TagFindTx           -> FindTx <$> get
    TagFindConsumingTxs -> FindConsumingTxs <$> get

  putResult = \case
    TagFork t1 t2 -> \case
      This r1 -> do
        putWord8 0x01
        putResult t1 r1
      That r2 -> do
        putWord8 0x02
        putResult t2 r2
      These r1 r2 -> do
        putWord8 0x03
        putResult t1 r1
        putResult t2 r2
    TagAdvanceSlots -> mempty
    TagAdvanceBlocks -> mempty
    TagFindConsumingTx -> put
    TagFindTx -> put
    TagIntersect -> mempty
    TagFindConsumingTxs -> put

  getResult = \case
    TagFork t1 t2    -> do
      tag <- getWord8
      case tag of
        0x01 -> This <$> getResult t1
        0x02 -> That <$> getResult t2
        0x03 -> These <$> getResult t1 <*> getResult t2
        _    -> fail $ "Invalid align result tag " <> show tag
    TagAdvanceSlots -> get
    TagAdvanceBlocks -> get
    TagFindConsumingTx -> get
    TagFindTx -> get
    TagIntersect -> get
    TagFindConsumingTxs -> get

  putErr = \case
    TagFork t1 t2 -> \case
      This e1 -> do
        putWord8 0x01
        putErr t1 e1
      That e2 -> do
        putWord8 0x02
        putErr t2 e2
      These e1 e2 -> do
        putWord8 0x03
        putErr t1 e1
        putErr t2 e2
    TagAdvanceSlots -> put
    TagAdvanceBlocks -> put
    TagFindConsumingTx -> put
    TagFindTx -> put
    TagIntersect -> put
    TagFindConsumingTxs -> put

  getErr = \case
    TagFork t1 t2    -> do
      tag <- getWord8
      case tag of
        0x01 -> This <$> getErr t1
        0x02 -> That <$> getErr t2
        0x03 -> These <$> getErr t1 <*> getErr t2
        _    -> fail $ "Invalid fork error tag " <> show tag
    TagAdvanceSlots -> get
    TagAdvanceBlocks -> get
    TagFindConsumingTx -> get
    TagFindTx -> get
    TagIntersect -> get
    TagFindConsumingTxs -> get

slotToUTCTime :: SlotConfig -> SlotNo -> UTCTime
slotToUTCTime SlotConfig{..} slot = addUTCTime (slotLength * fromIntegral slot) slotZeroTime

data SlotConfig = SlotConfig
  { slotZeroTime :: UTCTime
  , slotLength   :: NominalDiffTime
  }
  deriving stock (Show, Eq, Ord, Generic)

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

instance Binary SlotConfig where
  put SlotConfig{..} = do
    putUTCTime slotZeroTime
    put $ nominalDiffTimeToSeconds slotLength
  get = SlotConfig <$> getUTCTime <*> (secondsToNominalDiffTime <$> get)

data ChainSyncQuery delimiter err result where
  GetSlotConfig :: ChainSyncQuery Void () SlotConfig
  GetSecurityParameter :: ChainSyncQuery Void () Int

instance Query.IsQuery ChainSyncQuery where
  data Tag ChainSyncQuery delimiter err result where
    TagGetSlotConfig :: Query.Tag ChainSyncQuery Void () SlotConfig
    TagGetSecurityParameter :: Query.Tag ChainSyncQuery Void () Int
  tagEq TagGetSlotConfig TagGetSlotConfig               = Just (Refl, Refl, Refl)
  tagEq TagGetSlotConfig _                              = Nothing
  tagEq TagGetSecurityParameter TagGetSecurityParameter = Just (Refl, Refl, Refl)
  tagEq TagGetSecurityParameter _                       = Nothing
  putTag = \case
    TagGetSlotConfig        -> putWord8 0x01
    TagGetSecurityParameter -> putWord8 0x02
  getTag = do
    word <- getWord8
    case word of
      0x01 -> pure $ Query.SomeTag TagGetSlotConfig
      0x02 -> pure $ Query.SomeTag TagGetSecurityParameter
      _    -> fail "Invalid ChainSyncQuery tag"
  putQuery = \case
    GetSlotConfig        -> mempty
    GetSecurityParameter -> mempty
  getQuery = \case
    TagGetSlotConfig        -> pure GetSlotConfig
    TagGetSecurityParameter -> pure GetSecurityParameter
  putDelimiter = \case
    TagGetSlotConfig        -> absurd
    TagGetSecurityParameter -> absurd
  getDelimiter = \case
    TagGetSlotConfig        -> fail "no delimiter defined"
    TagGetSecurityParameter -> fail "no delimiter defined"
  putErr = \case
    TagGetSlotConfig        -> put
    TagGetSecurityParameter -> put
  getErr = \case
    TagGetSlotConfig        -> get
    TagGetSecurityParameter -> get
  putResult = \case
    TagGetSlotConfig        -> put
    TagGetSecurityParameter -> put
  getResult = \case
    TagGetSlotConfig        -> get
    TagGetSecurityParameter -> get
  tagFromQuery = \case
    GetSlotConfig        -> TagGetSlotConfig
    GetSecurityParameter -> TagGetSecurityParameter
