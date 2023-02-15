{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetTransaction
  where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.ByteString (ByteString)
import Data.Int (Int16, Int64)
import qualified Data.Map as Map
import Data.Time (LocalTime, localTimeToUTC, utc)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Hasql.TH (maybeStatement, vectorStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.ChainSync.Api (TxId(..), TxOutRef)
import Language.Marlowe.Runtime.Core.Api
  (MarloweVersion(..), MarloweVersionTag(..), Transaction(..), TransactionOutput(..))
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetContractState
  ( decodeBlockHeader
  , decodeContractId
  , decodeDatumBytes
  , decodeMetadata
  , decodePayout
  , decodeTransactionScriptOutput
  , decodeTxOutRef
  )
import Prelude hiding (init)

getTransaction :: TxId -> T.Transaction (Maybe SomeTransaction)
getTransaction txId = runMaybeT do
  SomeTransaction MarloweV1 input consumedBy tx <- fmap decodeSomeTransaction $ MaybeT $ T.statement (unTxId txId)
    [maybeStatement|
      SELECT
        applyTx.txId :: bytea,
        (ARRAY_AGG(applyTx.createTxId))[1] :: bytea,
        (ARRAY_AGG(applyTx.createTxIx))[1] :: smallint,
        (ARRAY_AGG(applyTx.outputTxIx))[1] :: smallint?,
        (ARRAY_AGG(applyTx.inputTxId))[1] :: bytea,
        (ARRAY_AGG(applyTx.inputTxIx))[1] :: smallint,
        (ARRAY_AGG(consumer.txId))[1] :: bytea?,
        (ARRAY_AGG(applyTx.metadata))[1] :: bytea?,
        (ARRAY_AGG(applyTx.inputs))[1] :: bytea,
        (ARRAY_AGG(applyTx.invalidBefore))[1] :: timestamp,
        (ARRAY_AGG(applyTx.invalidHereafter))[1] :: timestamp,
        (ARRAY_AGG(applyTx.slotNo))[1] :: bigint,
        (ARRAY_AGG(applyTx.blockId))[1] :: bytea,
        (ARRAY_AGG(applyTx.blockNo))[1] :: bigint,
        (ARRAY_AGG(contractTxOut.rolesCurrency))[1] :: bytea?,
        (ARRAY_AGG(contractTxOut.state))[1] :: bytea?,
        (ARRAY_AGG(contractTxOut.contract))[1] :: bytea?,
        (ARRAY_AGG(txOut.address))[1] :: bytea?,
        (ARRAY_AGG(txOut.lovelace))[1] :: bigint?,
        ARRAY_REMOVE(ARRAY_AGG(txOutAsset.policyId), NULL) :: bytea[],
        ARRAY_REMOVE(ARRAY_AGG(txOutAsset.name), NULL) :: bytea[],
        ARRAY_REMOVE(ARRAY_AGG(txOutAsset.quantity), NULL) :: bigint[]
      FROM marlowe.applyTx
      LEFT JOIN marlowe.contractTxOut
        ON contractTxOut.txId = applyTx.txId
        AND contractTxOut.txIx = applyTx.outputTxIx
      LEFT JOIN marlowe.txOut
        ON txOut.txId = applyTx.txId
        AND txOut.txIx = applyTx.outputTxIx
      LEFT JOIN marlowe.txOutAsset
        ON txOutAsset.txId = applyTx.txId
        AND txOutAsset.txIx = applyTx.outputTxIx
      LEFT JOIN marlowe.applyTx AS consumer
        ON consumer.inputTxId = applyTx.txId
        AND consumer.inputTxIx = applyTx.outputTxIx
      WHERE applyTx.txId = $1 :: bytea
      GROUP BY applyTx.txId
    |]

  payouts <- lift
    $ Map.fromDistinctAscList . V.toList . fmap decodePayout
    <$> T.statement (unTxId txId) [vectorStatement|
      SELECT
        payoutTxOut.txId :: bytea,
        payoutTxOut.txIx :: smallint,
        (ARRAY_AGG(payoutTxOut.rolesCurrency))[1] :: bytea,
        (ARRAY_AGG(payoutTxOut.role))[1] :: bytea,
        (ARRAY_AGG(txOut.address))[1] :: bytea,
        (ARRAY_AGG(txOut.lovelace))[1] :: bigint,
        ARRAY_REMOVE(ARRAY_AGG(txOutAsset.policyId), NULL) :: bytea[],
        ARRAY_REMOVE(ARRAY_AGG(txOutAsset.name), NULL) :: bytea[],
        ARRAY_REMOVE(ARRAY_AGG(txOutAsset.quantity), NULL) :: bigint[]
      FROM marlowe.payoutTxOut
      JOIN marlowe.txOut USING (txId, txIx)
      LEFT JOIN marlowe.txOutAsset USING (txId, txIx)
      WHERE payoutTxOut.txId = $1 :: bytea
      GROUP BY payoutTxOut.txId, payoutTxOut.txIx
      ORDER BY payoutTxOut.txId, payoutTxOut.txIx
    |]

  pure $ SomeTransaction MarloweV1 input consumedBy tx { output = (output tx) { payouts } }

type ResultRow =
  ( ByteString
  , ByteString
  , Int16
  , Maybe Int16
  , ByteString
  , Int16
  , Maybe ByteString
  , Maybe ByteString
  , ByteString
  , LocalTime
  , LocalTime
  , Int64
  , ByteString
  , Int64
  , Maybe ByteString
  , Maybe ByteString
  , Maybe ByteString
  , Maybe ByteString
  , Maybe Int64
  , Vector ByteString
  , Vector ByteString
  , Vector Int64
  )

decodeSomeTransaction :: ResultRow -> SomeTransaction
decodeSomeTransaction row = SomeTransaction
  { version = MarloweV1
  , input
  , consumedBy = TxId <$> consumedBy
  , transaction
  }
  where
    ( txId
      , createTxId
      , createTxIx
      , outputTxIx
      , inputTxId
      , inputTxIx
      , consumedBy
      , metadata
      , inputs
      , invalidBefore
      , invalidHereafter
      , slotNo
      , hash
      , blockNo
      , rolesCurrency
      , state
      , contract
      , address
      , lovelace
      , policyIds
      , tokenNames
      , quantities
      ) = row
    (input, transaction) = decodeTransaction
      ( txId
      , createTxId
      , createTxIx
      , inputTxId
      , inputTxIx
      , outputTxIx
      , metadata
      , inputs
      , invalidBefore
      , invalidHereafter
      , slotNo
      , hash
      , blockNo
      , rolesCurrency
      , state
      , contract
      , address
      , lovelace
      , policyIds
      , tokenNames
      , quantities
      )


decodeTransaction
  :: ( ByteString
     , ByteString
     , Int16
     , ByteString
     , Int16
     , Maybe Int16
     , Maybe ByteString
     , ByteString
     , LocalTime
     , LocalTime
     , Int64
     , ByteString
     , Int64
     , Maybe ByteString
     , Maybe ByteString
     , Maybe ByteString
     , Maybe ByteString
     , Maybe Int64
     , Vector ByteString
     , Vector ByteString
     , Vector Int64
     )
  -> (TxOutRef, Transaction 'V1)
decodeTransaction
  ( txId
  , createTxId
  , createTxIx
  , inputTxId
  , inputTxIx
  , outputTxIx
  , metadata
  , inputs
  , invalidBefore
  , invalidHereafter
  , slotNo
  , hash
  , blockNo
  , rolesCurrency
  , state
  , contract
  , address
  , lovelace
  , policyIds
  , tokenNames
  , quantities
  ) = ( decodeTxOutRef inputTxId inputTxIx
      , Transaction
        { transactionId = TxId txId
        , contractId = decodeContractId createTxId createTxIx
        , metadata = decodeMetadata metadata
        , blockHeader = decodeBlockHeader slotNo hash blockNo
        , validityLowerBound = localTimeToUTC utc invalidBefore
        , validityUpperBound = localTimeToUTC utc invalidHereafter
        , inputs = decodeDatumBytes inputs
        , output = TransactionOutput mempty $ decodeTransactionScriptOutput txId
            <$> outputTxIx
            <*> address
            <*> lovelace
            <*> pure policyIds
            <*> pure tokenNames
            <*> pure quantities
            <*> rolesCurrency
            <*> state
            <*> contract
        }
      )
