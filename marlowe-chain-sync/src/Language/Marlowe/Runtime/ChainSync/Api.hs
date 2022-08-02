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

data WithGenesis a = Genesis | At a
  deriving stock (Show, Read, Eq, Ord, Functor, Generic)
  deriving anyclass (Binary)

type ChainPoint = WithGenesis BlockHeader

data BlockHeader = BlockHeader
  { slotNo     :: !SlotNo
  , headerHash :: !BlockHeaderHash
  , blockNo    :: !BlockNo
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

data Transaction = Transaction
  { txId          :: !TxId
  , validityRange :: !ValidityRange
  , metadata      :: !(Maybe Metadata)
  , inputs        :: !(Set TransactionInput)
  , outputs       :: ![TransactionOutput]
  , mintedTokens  :: !Tokens
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

data ValidityRange
  = Unbounded
  | MinBound SlotNo
  | MaxBound SlotNo
  | MinMaxBound SlotNo SlotNo
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- TODO invlude validator versions
data Metadata
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

data TransactionInput = TransactionInput
  { txId     :: !TxId
  , txIx     :: !TxIx
  , redeemer :: !(Maybe Redeemer)
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

data TransactionOutput = TransactionOutput
  { address   :: !Address
  , assets    :: !Assets
  , datumHash :: !(Maybe DatumHash)
  , datum     :: !(Maybe Datum)
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

newtype Redeemer = Redeemer { unRedeemer :: Datum }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (Binary)

data Datum
  = Constr Integer [Datum]
  | Map [(Datum, Datum)]
  | List [Datum]
  | I Integer
  | B ByteString
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

fromPlutusData :: Plutus.Data -> Datum
fromPlutusData (Plutus.Constr i dats) = Constr i $ fromPlutusData <$> dats
fromPlutusData (Plutus.Map m)         = Map $ bimap fromPlutusData fromPlutusData <$> m
fromPlutusData (Plutus.List dats)     = List $ fromPlutusData <$> dats
fromPlutusData (Plutus.I i)           = I i
fromPlutusData (Plutus.B b)           = B b

data Assets = Assets
  { ada    :: !Lovelace
  , tokens :: !Tokens
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

newtype Tokens = Tokens { unTokens :: Map AssetId Quantity }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (Binary, Semigroup, Monoid)

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

data Move err result where

  Fork
    :: Move err1 result1
    -> Move err2 result2
    -> Move (These err1 err2) (These result1 result2)

  AdvanceSlots :: Natural -> Move Void ()

  AdvanceBlocks :: Natural -> Move Void ()

  Intersect :: [BlockHeader] -> Move IntersectError ()

  ConsumeUTxO :: TxOutRef -> Move UTxOError Transaction

data MoveResult err result
  = RollForward result BlockHeader ChainPoint
  | RollBack ChainPoint ChainPoint
  | Reject err ChainPoint
  | Wait ChainPoint

data UTxOError
  = UTxONotFound
  | UTxOSpent TxId
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

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

  ConsumeUTxO txOutRef -> do
    putWord8 0x04
    put txOutRef

  Intersect points -> do
    putWord8 0x05
    put points

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
    0x04 -> SomeQuery . ConsumeUTxO <$> get
    0x05 -> SomeQuery . Intersect <$> get
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
  ConsumeUTxO _ -> put
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
  ConsumeUTxO _ -> get
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
  ConsumeUTxO _ -> put
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
  ConsumeUTxO _ -> get
  Intersect _ -> get

schemaVersion1_0 :: SchemaVersion
schemaVersion1_0 = SchemaVersion "marlowe-chain-sync-1.0"
