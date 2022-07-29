{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving     #-}

module Language.Marlowe.Runtime.ChainSync.Types where

import Data.Bifunctor (bimap)
import Data.Binary (Binary (..))
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16, encodeBase16)
import Data.Map (Map)
import Data.Set (Set)
import Data.String (IsString (..))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word16, Word64)
import Debug.Trace (traceShowId)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
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
