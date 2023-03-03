{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetContractState
  where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Binary (Binary, get)
import Data.Binary.Get (runGet)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Int (Int16, Int64)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Hasql.TH (maybeStatement, vectorStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe (MarloweData(..), MarloweParams(MarloweParams))
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.ChainSync.Api
  ( Address(..)
  , AssetId(AssetId)
  , Assets(..)
  , BlockHeader(..)
  , BlockHeaderHash(..)
  , PolicyId(..)
  , TokenName(TokenName)
  , Tokens(Tokens)
  , TxId(..)
  , TxOutRef(..)
  , fromDatum
  )
import Language.Marlowe.Runtime.Core.Api
  ( ContractId(..)
  , MarloweTransactionMetadata
  , MarloweVersion(..)
  , MarloweVersionTag(..)
  , Payout(..)
  , TransactionScriptOutput(..)
  , emptyMarloweTransactionMetadata
  )
import qualified Plutus.V2.Ledger.Api as PV2
import Prelude hiding (init)

getContractState :: ContractId -> T.Transaction (Maybe SomeContractState)
getContractState (ContractId TxOutRef{..}) = runMaybeT do
  let params = (unTxId txId, fromIntegral txIx)
  (contractId, roleTokenMintingPolicyId, metadata, initialBlock, initialOutput) <- fmap decodeCreateResults
    $ MaybeT
    $ T.statement params [maybeStatement|
      SELECT
        createTxOut.txId :: bytea,
        createTxOut.txIx :: smallint,
        (ARRAY_AGG(createTxOut.metadata))[1] :: bytea?,
        (ARRAY_AGG(createTxOut.slotNo))[1] :: bigint,
        (ARRAY_AGG(createTxOut.blockId))[1] :: bytea,
        (ARRAY_AGG(createTxOut.blockNo))[1] :: bigint,
        (ARRAY_AGG(contractTxOut.rolesCurrency))[1] :: bytea,
        (ARRAY_AGG(contractTxOut.state))[1] :: bytea,
        (ARRAY_AGG(contractTxOut.contract))[1] :: bytea,
        (ARRAY_AGG(txOut.address))[1] :: bytea,
        (ARRAY_AGG(txOut.lovelace))[1] :: bigint,
        ARRAY_REMOVE(ARRAY_AGG(txOutAsset.policyId), NULL) :: bytea[],
        ARRAY_REMOVE(ARRAY_AGG(txOutAsset.name), NULL) :: bytea[],
        ARRAY_REMOVE(ARRAY_AGG(txOutAsset.quantity), NULL) :: bigint[]
      FROM marlowe.createTxOut
      JOIN marlowe.contractTxOut USING (txId, txIx)
      JOIN marlowe.txOut USING (txId, txIx)
      LEFT JOIN marlowe.txOutAsset USING (txId, txIx)
      WHERE createTxOut.txId = $1 :: bytea
        AND createTxOut.txIx = $2 :: smallint
      GROUP BY createTxOut.txId, createTxOut.txIx
    |]
  (latestBlock, latestOutput) <- lift $ maybe (initialBlock, Just initialOutput) decodeLatestResults <$> T.statement params [maybeStatement|
    SELECT
      applyTx.txId :: bytea,
      (ARRAY_AGG(applyTx.outputTxIx))[1] :: smallint?,
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
    WHERE applyTx.createTxId = $1 :: bytea
      AND applyTx.createTxIx = $2 :: smallint
      AND consumer.txId IS NULL
    GROUP BY applyTx.txId
  |]
  unclaimedPayouts <- lift
    $ Map.fromDistinctAscList . V.toList . fmap decodePayout
    <$> T.statement params [vectorStatement|
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
      JOIN marlowe.applyTx USING (txId)
      LEFT JOIN marlowe.withdrawalTxIn
        ON withdrawalTxIn.payoutTxId = payoutTxOut.txId
        AND withdrawalTxIn.payoutTxIx = payoutTxOut.txIx
      WHERE applyTx.createTxId = $1 :: bytea
        AND applyTx.createTxIx = $2 :: smallint
        AND withdrawalTxIn.txId IS NULL
      GROUP BY payoutTxOut.txId, payoutTxOut.txIx
      ORDER BY payoutTxOut.txId, payoutTxOut.txIx
    |]
  pure $ SomeContractState MarloweV1 ContractState{..}

type CreateResultRow =
  ( ByteString
  , Int16
  , Maybe ByteString
  , Int64
  , ByteString
  , Int64
  , ByteString
  , ByteString
  , ByteString
  , ByteString
  , Int64
  , Vector ByteString
  , Vector ByteString
  , Vector Int64
  )

decodeCreateResults :: CreateResultRow -> (ContractId, PolicyId, MarloweTransactionMetadata, BlockHeader, TransactionScriptOutput 'V1)
decodeCreateResults row =
  ( decodeContractId txId txIx
  , PolicyId rolesCurrency
  , decodeMetadata metadata
  , decodeBlockHeader slotNo hash blockNo
  , decodeTransactionScriptOutput txId txIx address lovelace policyIds tokenNames quantities rolesCurrency state contract
  )
  where
    ( txId
      , txIx
      , metadata
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

type LatestResultRow =
  ( ByteString
  , Maybe Int16
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

decodeLatestResults :: LatestResultRow  -> (BlockHeader, Maybe (TransactionScriptOutput 'V1))
decodeLatestResults row =
  ( decodeBlockHeader slotNo hash blockNo
  , decodeTransactionScriptOutput txId
      <$> txIx
      <*> address
      <*> lovelace
      <*> pure policyIds
      <*> pure tokenNames
      <*> pure quantities
      <*> rolesCurrency
      <*> state
      <*> contract
  )
  where
    ( txId
      , txIx
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

type PayoutRow =
  ( ByteString
  , Int16
  , ByteString
  , ByteString
  , ByteString
  , Int64
  , Vector ByteString
  , Vector ByteString
  , Vector Int64
  )

decodePayout :: PayoutRow -> (TxOutRef, Payout 'V1)
decodePayout row =
  ( decodeTxOutRef txId txIx
  , Payout
    { address = Address address
    , assets = Assets
      { ada = fromIntegral lovelace
      , tokens = decodeTokens policyIds tokenNames quantities
      }
    , datum = AssetId (PolicyId rolesCurrency) (TokenName role)
    }
  )
  where
    ( txId
      , txIx
      , rolesCurrency
      , role
      , address
      , lovelace
      , policyIds
      , tokenNames
      , quantities
      ) = row

decodeBinary :: Binary a => ByteString -> a
decodeBinary = runGet get . fromStrict

decodeMetadata :: Maybe ByteString -> MarloweTransactionMetadata
decodeMetadata = maybe emptyMarloweTransactionMetadata decodeBinary

decodeContractId :: ByteString -> Int16 -> ContractId
decodeContractId = fmap ContractId . decodeTxOutRef

decodeTxOutRef :: ByteString -> Int16 -> TxOutRef
decodeTxOutRef txId txIx = TxOutRef (TxId txId) (fromIntegral txIx)

decodeBlockHeader :: Int64 -> ByteString -> Int64 -> BlockHeader
decodeBlockHeader slotNo hash blockNo =
  BlockHeader (fromIntegral slotNo) (BlockHeaderHash hash) (fromIntegral blockNo)

decodeTransactionScriptOutput
  :: ByteString
  -> Int16
  -> ByteString
  -> Int64
  -> Vector ByteString
  -> Vector ByteString
  -> Vector Int64
  -> ByteString
  -> ByteString
  -> ByteString
  -> TransactionScriptOutput 'V1
decodeTransactionScriptOutput txId txIx address lovelace policyIds tokenNames quantities rolesCurrency state contract = TransactionScriptOutput
  { address = Address address
  , assets = Assets
    { ada = fromIntegral lovelace
    , tokens = decodeTokens policyIds tokenNames quantities
    }
  , utxo = decodeTxOutRef txId txIx
  , datum = decodeMarloweData rolesCurrency state contract
  }

decodeMarloweData :: ByteString -> ByteString -> ByteString -> MarloweData
decodeMarloweData rolesCurrency state contract = MarloweData
  { marloweParams = MarloweParams $ PV2.CurrencySymbol $ PV2.toBuiltin rolesCurrency
  , marloweState = decodeDatumBytes state
  , marloweContract = decodeDatumBytes contract
  }

decodeDatumBytes :: PV2.FromData a => ByteString -> a
decodeDatumBytes = fromJust . fromDatum . decodeBinary

decodeTokens
  :: Vector ByteString
  -> Vector ByteString
  -> Vector Int64
  -> Tokens
decodeTokens policyIds tokenNames quantities = Tokens $ Map.fromList $ zipWith3
  (\p t q -> (AssetId (PolicyId p) (TokenName t), fromIntegral q))
  (V.toList policyIds)
  (V.toList tokenNames)
  (V.toList quantities)
