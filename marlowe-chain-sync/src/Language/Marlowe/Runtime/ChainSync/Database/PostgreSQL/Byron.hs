{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Byron where

import Cardano.Chain.Common (Address, addrToBase58, unsafeGetLovelace)
import Cardano.Chain.UTxO
import Cardano.Crypto (AbstractHash, hashToBytes)
import Cardano.Ledger.Binary (Annotated (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int
import qualified Data.List.NonEmpty as NE
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Types

byronTxRow :: Int64 -> Bytea -> Bytea -> ATxAux ByteString -> TxRowGroup
byronTxRow slotNo blockHash txId (unAnnotated . aTaTx -> UnsafeTx{..}) =
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
byronAddressFields (addrToBase58 -> addressBytes) =
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
