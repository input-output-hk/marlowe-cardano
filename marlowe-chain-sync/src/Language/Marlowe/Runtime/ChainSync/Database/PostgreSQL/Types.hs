{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Types where

import Cardano.Api
import Data.Base16.Types (extractBase16)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (encodeBase16)
import Data.Csv
import Data.Int
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)

newtype Bytea = Bytea ByteString

instance ToField Bytea where
  toField (Bytea bs) = "\\x" <> encodeUtf8 (extractBase16 . encodeBase16 $ bs)

newtype SqlBool = SqlBool Bool

instance ToField SqlBool where
  toField (SqlBool True) = "TRUE"
  toField (SqlBool False) = "FALSE"

data BlockRow = BlockRow
  { hash :: !Bytea
  , slotNo :: !Int64
  , blockNo :: !Int64
  }
  deriving (Generic)

instance ToRecord BlockRow

data TxRow = TxRow
  { blockHash :: !Bytea
  , txId :: !Bytea
  , slotNo :: !Int64
  , validityLowerBound :: !(Maybe Int64)
  , validityUpperBound :: !(Maybe Int64)
  , metadata :: !(Maybe Bytea)
  , isValid :: !SqlBool
  }
  deriving (Generic)

instance ToRecord TxRow

data TxOutRow = TxOutRow
  { txId :: !Bytea
  , txIx :: !Int16
  , slotNo :: !Int64
  , address :: !Bytea
  , lovelace :: !Int64
  , datumHash :: !(Maybe Bytea)
  , datumBytes :: !(Maybe Bytea)
  , isCollateral :: !SqlBool
  , addressHeader :: !Bytea
  , addressPaymentCredential :: !(Maybe Bytea)
  , addressStakeAddressReference :: !(Maybe Bytea)
  }
  deriving (Generic)

instance ToRecord TxOutRow

data TxInRow = TxInRow
  { txOutId :: !Bytea
  , txOutIx :: !Int16
  , txInId :: !Bytea
  , slotNo :: !Int64
  , redeemerDatumBytes :: !(Maybe Bytea)
  , isCollateral :: !SqlBool
  }
  deriving (Generic)

instance ToRecord TxInRow

data AssetOutRow = AssetOutRow
  { txId :: !Bytea
  , txIx :: !Int16
  , slotNo :: !Int64
  , policyId :: !Bytea
  , tokenName :: !Bytea
  , quantity :: !Int64
  }
  deriving (Generic)

instance ToRecord AssetOutRow

data AssetMintRow = AssetMintRow
  { txId :: !Bytea
  , slotNo :: !Int64
  , policyId :: !Bytea
  , tokenName :: !Bytea
  , quantity :: !Int64
  }
  deriving (Generic)

instance ToRecord AssetMintRow

type BlockRowGroup = (BlockRow, [TxRowGroup])

type TxRowGroup = (TxRow, [TxInRow], [TxOutRowGroup], [AssetMintRow])

type TxOutRowGroup = (TxOutRow, [AssetOutRow])

serialiseToBytea :: (SerialiseAsRawBytes a) => a -> Bytea
serialiseToBytea = Bytea . serialiseToRawBytes

convertSlotNo :: SlotNo -> Int64
convertSlotNo = fromIntegral . unSlotNo

convertBlockNo :: BlockNo -> Int64
convertBlockNo = fromIntegral . unBlockNo
