{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetTransactions
  where

import Control.Arrow ((&&&))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Bifunctor (first)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (groupBy, unfoldr)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as V
import Hasql.TH (maybeStatement, vectorStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.ChainSync.Api (TxId(..), TxOutRef(..))
import qualified Language.Marlowe.Runtime.ChainSync.Api as TxOutRef (TxOutRef(..))
import Language.Marlowe.Runtime.Core.Api
  ( ContractId(..)
  , MarloweVersion(..)
  , MarloweVersionTag(..)
  , Payout(..)
  , Transaction(..)
  , TransactionOutput(..)
  , TransactionScriptOutput(..)
  )
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetContractState (PayoutRow, decodePayout)
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetTransaction (decodeTransaction)
import Prelude hiding (init)

getTransactions :: ContractId -> T.Transaction (Maybe SomeTransactions)
getTransactions (ContractId createTxOutRef@TxOutRef{..}) = runMaybeT do
  let params = (unTxId txId, fromIntegral txIx)

  _ <- MaybeT $ T.statement params
    [maybeStatement|
      SELECT txId :: bytea FROM marlowe.createTxOut WHERE txId = $1 :: bytea AND txIx = $2 :: smallint
    |]

  txs <- lift $ Map.fromDistinctAscList . V.toList . fmap decodeTransaction <$> T.statement params
    [vectorStatement|
      SELECT
        applyTx.txId :: bytea,
        (ARRAY_AGG(applyTx.createTxId))[1] :: bytea,
        (ARRAY_AGG(applyTx.createTxIx))[1] :: smallint,
        (ARRAY_AGG(applyTx.inputTxId))[1] :: bytea,
        (ARRAY_AGG(applyTx.inputTxIx))[1] :: smallint,
        (ARRAY_AGG(applyTx.outputTxIx))[1] :: smallint?,
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
      WHERE applyTx.createTxId = $1 :: bytea
        AND applyTx.createTxIx = $2 :: smallint
      GROUP BY applyTx.txId
      ORDER BY (ARRAY_AGG(applyTx.inputTxId))[1], (ARRAY_AGG(applyTx.inputTxIx))[1]
    |]

  payouts <- lift $ processPayoutRows <$> T.statement params
    [vectorStatement|
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
      WHERE applyTx.createTxId = $1 :: bytea
        AND applyTx.createTxIx = $2 :: smallint
      GROUP BY payoutTxOut.txId, payoutTxOut.txIx
      ORDER BY payoutTxOut.txId, payoutTxOut.txIx
    |]

  pure
    $ SomeTransactions MarloweV1
    $ unfoldr ((fmap . first) (addPayouts payouts) . (findConsumer txs =<<))
    $ Just createTxOutRef

findConsumer
  :: Map TxOutRef (Transaction 'V1)
  -> TxOutRef
  -> Maybe (Transaction 'V1, Maybe TxOutRef)
findConsumer txsByInputs previousOutput = do
  consumer@Transaction{output=TransactionOutput{scriptOutput}} <- Map.lookup previousOutput txsByInputs
  pure (consumer, scriptOutput <&> \TransactionScriptOutput{utxo} -> utxo)

addPayouts
  :: Map TxId (Map TxOutRef (Payout v))
  -> Transaction v
  -> Transaction v
addPayouts allPayouts tx@Transaction{..} = case Map.lookup transactionId allPayouts of
  Nothing -> tx
  Just newPayouts -> tx { output = output { payouts = payouts output <> newPayouts } }

processPayoutRows :: V.Vector PayoutRow -> Map TxId (Map TxOutRef (Payout 'V1))
processPayoutRows = Map.fromDistinctAscList
  . (fmap . fmap) Map.fromDistinctAscList
  . groupByKey (TxOutRef.txId . fst)
  . fmap decodePayout
  . V.toList

groupByKey :: Ord k => (a -> k) -> [a] -> [(k, [a])]
groupByKey f = fmap (f . head &&& id) . groupBy (on (==) f)
