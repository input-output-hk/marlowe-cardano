{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving     #-}
{-# LANGUAGE GADTs                 #-}

module Language.Marlowe.Runtime.ChainSync.Api where

import Data.Bifunctor (bimap)
import Data.Binary (Binary (..), Get, Put, get, getWord8, put, putWord8)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16, encodeBase16)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import Data.Set (Set)
import Data.String (IsString (..))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.These (These (..))
import Data.Void (Void)
import Data.Word (Word16, Word64)
import Debug.Trace (traceShowId)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Network.Protocol.ChainSeek.Client (ChainSeekClient)
import Network.Protocol.ChainSeek.Codec (DeserializeError, SomeQuery (..), codecChainSeek)
import Network.Protocol.ChainSeek.Server (ChainSeekServer)
import Network.Protocol.ChainSeek.Types (ChainSeek, SchemaVersion (SchemaVersion))
import Network.TypedProtocol.Codec (Codec)
import qualified Plutus.V1.Ledger.Api as Plutus
import Text.Read (Read (..), pfail)

-- | Extends a type with a "Genesis" member.
data WithGenesis a = Genesis | At a
  deriving stock (Show, Read, Eq, Ord, Functor, Generic)
  deriving anyclass (Binary)

-- | A point in the chain, identified by a slot number, block header hash, and
-- block numner.
type ChainPoint = WithGenesis BlockHeader

-- | A block header, consisting of a slot number, a hash, and a block number.
data BlockHeader = BlockHeader
  { slotNo     :: !SlotNo          -- ^ The slot number when this block was produced.
  , headerHash :: !BlockHeaderHash -- ^ The hash of this block's header.
  , blockNo    :: !BlockNo         -- ^ The ordinal number of this block.
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- | A transaction body
data Transaction = Transaction
  { txId          :: !TxId                   -- ^ The hash of this transaction.
  , validityRange :: !ValidityRange          -- ^ The range of slots during which this transaction is valid.
  , metadata      :: !(Maybe Metadata)       -- ^ The metadata of this transaction (only includes Marlowe-relevant metadata).
  , inputs        :: !(Set TransactionInput) -- ^ The inputs consumed by the transaction
  , outputs       :: ![TransactionOutput]    -- ^ The outputs produced by the transaction.
  , mintedTokens  :: !Tokens                 -- ^ Tokens minted by the transaction.
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- | A validity range for a transaction
data ValidityRange
  = Unbounded                 -- ^ The transaction is always valid.
  | MinBound SlotNo           -- ^ The transaction is only valid after a specific slot.
  | MaxBound SlotNo           -- ^ The transaction is only valid before a specific slot.
  | MinMaxBound SlotNo SlotNo -- ^ The transaction is only valid between two slots.
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- TODO invlude validator versions
data Metadata
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- | An input of a transaction.
data TransactionInput = TransactionInput
  { txId     :: !TxId             -- ^ The txId of the TransactionOutput this input consumes.
  , txIx     :: !TxIx             -- ^ The txIx of the TransactionOutput this input consumes.
  , redeemer :: !(Maybe Redeemer) -- ^ The script redeemer dataum for this input (if one was provided).
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- | An output of a transaction.
data TransactionOutput = TransactionOutput
  { address   :: !Address           -- ^ The address that receives the assets of this output.
  , assets    :: !Assets            -- ^ The assets this ouptut produces.
  , datumHash :: !(Maybe DatumHash) -- ^ The hash of the script datum associated with this output.
  , datum     :: !(Maybe Datum)     -- ^ The script datum associated with this output.
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- | A script datum that is used to spend the output of a script tx.
newtype Redeemer = Redeemer { unRedeemer :: Datum }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (Binary)

-- | A datum as a sum-of-products.
data Datum
  = Constr Integer [Datum]
  | Map [(Datum, Datum)]
  | List [Datum]
  | I Integer
  | B ByteString
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- | Convert from Plutus.V1.Ledger.Api.Data to Datum
fromPlutusData :: Plutus.Data -> Datum
fromPlutusData (Plutus.Constr i dats) = Constr i $ fromPlutusData <$> dats
fromPlutusData (Plutus.Map m)         = Map $ bimap fromPlutusData fromPlutusData <$> m
fromPlutusData (Plutus.List dats)     = List $ fromPlutusData <$> dats
fromPlutusData (Plutus.I i)           = I i
fromPlutusData (Plutus.B b)           = B b

-- | Convert to Plutus.V1.Ledger.Api.Data to Datum
toPlutusData :: Datum -> Plutus.Data
toPlutusData (Constr i dats) = Plutus.Constr i $ toPlutusData <$> dats
toPlutusData (Map m)         = Plutus.Map $ bimap toPlutusData toPlutusData <$> m
toPlutusData (List dats)     = Plutus.List $ toPlutusData <$> dats
toPlutusData (I i)           = Plutus.I i
toPlutusData (B b)           = Plutus.B b

-- | A collection of assets transferred by a trasaction output.
data Assets = Assets
  { ada    :: !Lovelace -- ^ The ADA sent by the tx output.
  , tokens :: !Tokens   -- ^ Additional tokens sent by the tx output.
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- | A collection of token quantities by their asset ID.
newtype Tokens = Tokens { unTokens :: Map AssetId Quantity }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (Binary, Semigroup, Monoid)

-- | A newtype wrapper for parsing base 16 strings as byte strings.
newtype Base16 = Base16 { unBase16 :: ByteString }

instance Show Base16 where
  show = show . encodeBase16 . unBase16

instance Read Base16 where
  readPrec = either (const pfail) (pure . Base16) . decodeBase16 . encodeUtf8 . traceShowId =<< readPrec

instance IsString Base16 where
  fromString = either (error . T.unpack) Base16 . decodeBase16 . encodeUtf8 . T.pack

newtype DatumHash = DatumHash { unDatumHash :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary)
  deriving (IsString, Show, Read) via Base16

newtype TxId = TxId { unTxId :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary)
  deriving (IsString, Show, Read) via Base16

newtype TxIx = TxIx { unTxIx :: Word16 }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary)

data TxOutRef = TxOutRef
  { txId :: !TxId
  , txIx :: !TxIx
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

newtype SlotNo = SlotNo { unSlotNo :: Word64 }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary)

newtype BlockNo = BlockNo { unBlockNo :: Word64 }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary)

newtype BlockHeaderHash = BlockHeaderHash { unBlockHeaderHash :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary)
  deriving (IsString, Show, Read) via Base16

data AssetId = AssetId
  { policyId  :: !PolicyId
  , tokenName :: !TokenName
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

newtype PolicyId = PolicyId { unPolicyId :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary)
  deriving (IsString, Show, Read) via Base16

newtype TokenName = TokenName { unTokenName :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary)
  deriving (IsString, Show, Read) via Base16

newtype Quantity = Quantity { unQuantity :: Word64 }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary)

newtype Lovelace = Lovelace { unLovelace :: Word64 }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary)

newtype Address = Address { unAddress :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary)
  deriving (IsString, Show, Read) via Base16

-- data Address = Address
--   { paymentCredential :: !Credential
--   , stakingCredential :: !(Maybe StakingCredential)
--   }
--   deriving stock (Show, Read, Eq, Ord, Generic)
--   deriving anyclass (Binary)

data Credential
  = PubKeyCredential PubKeyHash
  | ScriptCredential ValidatorHash
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

newtype PubKeyHash = PubKeyHash { unPubKeyHash :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary)
  deriving (IsString, Show, Read) via Base16

newtype ValidatorHash = ValidatorHash { unValidatorHash :: ByteString }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary)
  deriving (IsString, Show, Read) via Base16

data StakingCredential
  = StakingHash Credential
  | StakingPtr Word64 Natural Natural
  deriving stock (Show, Read, Eq, Ord, Generic)
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

  -- | Jump to the lastest intersection from a list of known block headers
  -- without collecting any results.
  Intersect :: [BlockHeader] -> Move IntersectError ()

  -- | Advance to the block when a tx out is consumed and collect the tx that
  -- consumes the tx out.
  FindConsumingTx :: TxOutRef -> Move UTxOError Transaction

  -- | Advance to the block containing a transaction.
  FindTx :: TxId -> Move TxError Transaction

-- | Reasons a 'FindConsumingTx' request can be rejected.
data UTxOError
  = UTxONotFound
  | UTxOSpent TxId
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- | Reasons a 'FindTx' request can be rejected.
data TxError
  = TxNotFound
  | TxInPast BlockHeader
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- | Reasons an 'Intersect' request can be rejected.
data IntersectError = IntersectionNotFound
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

type RuntimeChainSeek = ChainSeek Move ChainPoint ChainPoint

type RuntimeChainSeekClient = ChainSeekClient Move ChainPoint ChainPoint

type RuntimeChainSeekServer = ChainSeekServer Move ChainPoint ChainPoint

type RuntimeChainSeekCodec m = Codec RuntimeChainSeek DeserializeError m LBS.ByteString

runtimeChainSeekCodec :: Applicative m => RuntimeChainSeekCodec m
runtimeChainSeekCodec = codecChainSeek putMove getMove putResult getResult putError getError put get put get

putMove :: SomeQuery Move -> Put
putMove (SomeQuery move) = case move of
  Fork m1 m2 -> do
    putWord8 0x01
    putMove $ SomeQuery m1
    putMove $ SomeQuery m2

  AdvanceSlots slots -> do
    putWord8 0x02
    put slots

  AdvanceBlocks blocks -> do
    putWord8 0x03
    put blocks

  FindConsumingTx txOutRef -> do
    putWord8 0x04
    put txOutRef

  Intersect points -> do
    putWord8 0x05
    put points

  FindTx txId -> do
    putWord8 0x06
    put txId

getMove :: Get (SomeQuery Move)
getMove = do
  tag <- getWord8
  case tag of
    0x01 -> do
      SomeQuery m1 <- getMove
      SomeQuery m2 <- getMove
      pure $ SomeQuery $ Fork m1 m2
    0x02 -> SomeQuery . AdvanceSlots <$> get
    0x03 -> SomeQuery . AdvanceBlocks <$> get
    0x04 -> SomeQuery . FindConsumingTx <$> get
    0x05 -> SomeQuery . Intersect <$> get
    0x06 -> SomeQuery . Intersect <$> get
    _ -> fail $ "Invalid move tag " <> show tag

putResult :: forall err result. Move err result -> result -> Put
putResult = \case
  Fork m1 m2 -> \case
    This r1 -> do
      putWord8 0x01
      putResult m1 r1
    That r2 -> do
      putWord8 0x02
      putResult m2 r2
    These r1 r2 -> do
      putWord8 0x03
      putResult m1 r1
      putResult m2 r2
  AdvanceSlots _ -> mempty
  AdvanceBlocks _ -> mempty
  FindConsumingTx _ -> put
  FindTx _ -> put
  Intersect _ -> mempty

getResult :: forall err result. Move err result -> Get result
getResult = \case
  Fork m1 m2    -> do
    tag <- getWord8
    case tag of
      0x01 -> This <$> getResult m1
      0x02 -> That <$> getResult m2
      0x03 -> These <$> getResult m1 <*> getResult m2
      _    -> fail $ "Invalid align result tag " <> show tag
  AdvanceSlots _ -> get
  AdvanceBlocks _ -> get
  FindConsumingTx _ -> get
  FindTx _ -> get
  Intersect _ -> get

putError :: forall err result. Move err result -> err -> Put
putError = \case
  Fork m1 m2 -> \case
    This e1 -> do
      putWord8 0x01
      putError m1 e1
    That e2 -> do
      putWord8 0x02
      putError m2 e2
    These e1 e2 -> do
      putWord8 0x03
      putError m1 e1
      putError m2 e2
  AdvanceSlots _ -> put
  AdvanceBlocks _ -> put
  FindConsumingTx _ -> put
  FindTx _ -> put
  Intersect _ -> put

getError :: forall err result. Move err result -> Get err
getError = \case
  Fork m1 m2    -> do
    tag <- getWord8
    case tag of
      0x01 -> This <$> getError m1
      0x02 -> That <$> getError m2
      0x03 -> These <$> getError m1 <*> getError m2
      _    -> fail $ "Invalid fork error tag " <> show tag
  AdvanceSlots _ -> get
  AdvanceBlocks _ -> get
  FindConsumingTx _ -> get
  FindTx _ -> get
  Intersect _ -> get

schemaVersion1_0 :: SchemaVersion
schemaVersion1_0 = SchemaVersion "marlowe-chain-sync-1.0"
