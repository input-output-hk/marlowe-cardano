{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Byron where

import Cardano.Chain.Common (Address, unsafeGetLovelace)
import Cardano.Chain.UTxO (Tx (..), TxIn (..), TxOut (..))
import Cardano.Crypto (AbstractHash, hashToBytes)
import Cardano.Ledger.Binary (byronProtVer, serialize')
import qualified Data.ByteString as BS
import Data.Int (Int16, Int64)
import qualified Data.List.NonEmpty as NE
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Types (
  Bytea (..),
  SqlBool (SqlBool),
  TxInRow (..),
  TxOutRow (
    TxOutRow,
    address,
    addressHeader,
    addressPaymentCredential,
    addressStakeAddressReference,
    datumBytes,
    datumHash,
    isCollateral,
    lovelace,
    slotNo,
    txId,
    txIx
  ),
  TxOutRowGroup,
  TxRow (
    TxRow,
    blockHash,
    isValid,
    metadata,
    slotNo,
    txId,
    validityLowerBound,
    validityUpperBound
  ),
  TxRowGroup,
 )

byronTxRow :: Int64 -> Bytea -> Bytea -> Tx -> TxRowGroup
byronTxRow slotNo blockHash txId UnsafeTx{..} =
  ( TxRow
      { blockHash
      , txId
      , slotNo
      , validityUpperBound = Nothing
      , validityLowerBound = Nothing
      , metadata = Nothing
      , isValid = SqlBool True
      }
  , NE.toList $ byronTxInRow slotNo txId <$> txInputs
  , zipWith (byronTxOutRow slotNo txId) [0 ..] $ NE.toList txOutputs
  , []
  )

byronTxInRow :: Int64 -> Bytea -> TxIn -> TxInRow
byronTxInRow slotNo txInId (TxInUtxo txId txIx) =
  TxInRow
    { txOutId = hashToBytea txId
    , txOutIx = fromIntegral txIx
    , txInId
    , slotNo
    , redeemerDatumBytes = Nothing
    , isCollateral = SqlBool False
    }

byronTxOutRow :: Int64 -> Bytea -> Int16 -> TxOut -> TxOutRowGroup
byronTxOutRow slotNo txId txIx TxOut{..} =
  ( TxOutRow
      { txId
      , txIx
      , slotNo
      , address
      , lovelace = fromIntegral $ unsafeGetLovelace txOutValue
      , datumHash = Nothing
      , datumBytes = Nothing
      , isCollateral = SqlBool False
      , addressHeader
      , addressPaymentCredential
      , addressStakeAddressReference
      }
  , []
  )
  where
    AddressFields{..} = byronAddressFields txOutAddress

hashToBytea :: AbstractHash al a -> Bytea
hashToBytea = Bytea . hashToBytes

byronAddressFields :: Address -> AddressFields
byronAddressFields (serialize' byronProtVer -> addressBytes) =
  AddressFields
    { address = Bytea addressBytes
    , addressHeader = Bytea $ BS.take 1 addressBytes
    , addressPaymentCredential = Nothing
    , addressStakeAddressReference = Nothing
    }

data AddressFields = AddressFields
  { address :: Bytea
  , addressHeader :: Bytea
  , addressPaymentCredential :: Maybe Bytea
  , addressStakeAddressReference :: Maybe Bytea
  }
