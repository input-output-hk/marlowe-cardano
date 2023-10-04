{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetNextBlocks (
  getNextBlocks,
) where

import Control.Arrow (Arrow (..))
import Control.Monad (join)
import qualified Data.Bifunctor as Bifunctor
import Data.Binary (Binary (..))
import Data.Binary.Get (runGet)
import Data.ByteString (ByteString, fromStrict)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Int (Int16, Int64)
import Data.List (groupBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Profunctor (Profunctor (..))
import qualified Data.Set as Set
import Data.Time (LocalTime)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import Hasql.TH (vectorStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Runtime.ChainSync.Api (
  Address (..),
  BlockHeader (..),
  BlockHeaderHash (BlockHeaderHash, unBlockHeaderHash),
  ChainPoint,
  ScriptHash (..),
  TxId (..),
  TxOutRef (..),
  WithGenesis (..),
 )
import Language.Marlowe.Runtime.Core.Api (
  ContractId (..),
  MarloweVersion (..),
  SomeMarloweVersion (..),
  emptyMarloweTransactionMetadata,
  output,
  payouts,
  transactionId,
 )
import Language.Marlowe.Runtime.History.Api
import Language.Marlowe.Runtime.Sync.Database (Next (..))
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetContractState (
  decodeBlockHeader,
  decodePayout,
  decodeTransactionScriptOutput,
  decodeTxOutRef,
 )
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetNextSteps (Orientation (..), orient)
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetTransaction (decodeTransaction)
import Prelude hiding (init)

getNextBlocks :: Word8 -> ChainPoint -> T.Transaction (Next MarloweBlock)
getNextBlocks extraBlockCount point = do
  orient point >>= \case
    RolledBack toPoint tip -> pure $ Rollback toPoint tip
    AtTip -> pure Wait
    BeforeTip tip -> do
      blocks <- getNextBlockHeaders extraBlockCount point
      createTransactionsByBlock <- getNewContracts blocks
      applyInputsTransactionsByBlock <- getApplyInputsTransactions blocks
      withdrawTransactionsByBlock <- getWithdrawTransactions blocks
      pure $ Next (last blocks) tip do
        blockHeader <- blocks
        pure
          MarloweBlock
            { blockHeader
            , createTransactions = fold $ Map.lookup blockHeader.headerHash createTransactionsByBlock
            , applyInputsTransactions = fold $ Map.lookup blockHeader.headerHash applyInputsTransactionsByBlock
            , withdrawTransactions = fold $ Map.lookup blockHeader.headerHash withdrawTransactionsByBlock
            }

getNextBlockHeaders :: Word8 -> ChainPoint -> T.Transaction [BlockHeader]
getNextBlockHeaders extraBlockCount Genesis =
  T.statement (fromIntegral extraBlockCount + 1) do
    fmap (V.toList . fmap (uncurry3 decodeBlockHeader)) do
      [vectorStatement|
        SELECT block.slotNo :: bigint, block.id :: bytea, block.blockNo :: bigint
        FROM marlowe.block
        ORDER BY slotNo
        LIMIT $1 :: int
      |]
getNextBlockHeaders extraBlockCount (At BlockHeader{..}) =
  T.statement (slotNo, fromIntegral extraBlockCount + 1) do
    dimap (Bifunctor.first fromIntegral) (V.toList . fmap (uncurry3 decodeBlockHeader)) do
      [vectorStatement|
        SELECT block.slotNo :: bigint, block.id :: bytea, block.blockNo :: bigint
        FROM marlowe.block
        WHERE slotNo > $1 :: int
        ORDER BY slotNo
        LIMIT $2 :: int
      |]

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

getNewContracts :: [BlockHeader] -> T.Transaction (Map BlockHeaderHash [MarloweCreateTransaction])
getNewContracts blocks =
  T.statement blocks do
    dimap (V.fromList . fmap encodeBlockHeader) decodeMarloweCreateTransactions do
      [vectorStatement|
        SELECT
          (ARRAY_AGG(createTxOut.blockId))[1] :: bytea,
          createTxOut.txId :: bytea,
          createTxOut.txIx :: smallint,
          (ARRAY_AGG(txOut.address))[1] :: bytea,
          (ARRAY_AGG(txOut.lovelace))[1] :: bigint,
          ARRAY_REMOVE(ARRAY_AGG(txOutAsset.policyId), NULL) :: bytea[],
          ARRAY_REMOVE(ARRAY_AGG(txOutAsset.name), NULL) :: bytea[],
          ARRAY_REMOVE(ARRAY_AGG(txOutAsset.quantity), NULL) :: bigint[],
          (ARRAY_AGG(contractTxOut.rolesCurrency))[1] :: bytea,
          (ARRAY_AGG(contractTxOut.state))[1] :: bytea,
          (ARRAY_AGG(contractTxOut.contract))[1] :: bytea,
          (ARRAY_AGG(createTxOut.metadata))[1] :: bytea?,
          (ARRAY_AGG(contractTxOut.payoutScriptHash))[1] :: bytea
        FROM marlowe.createTxOut
        JOIN (SELECT UNNEST($1 :: bytea[]) AS blockId) as blockId USING (blockId)
        JOIN marlowe.contractTxOut USING (txId, txIx)
        JOIN marlowe.txOut USING (txId, txIx)
        LEFT JOIN marlowe.txOutAsset USING (txId, txIx)
        GROUP BY txId, txIx
        ORDER BY (ARRAY_AGG(createTxOut.blockId))[1], createTxOut.txId, createTxOut.txIx
      |]

type CreateTransactionRow =
  ( ByteString
  , ByteString
  , Int16
  , ByteString
  , Int64
  , Vector ByteString
  , Vector ByteString
  , Vector Int64
  , ByteString
  , ByteString
  , ByteString
  , Maybe ByteString
  , ByteString
  )

decodeMarloweCreateTransactions :: Vector CreateTransactionRow -> Map BlockHeaderHash [MarloweCreateTransaction]
decodeMarloweCreateTransactions =
  Map.fromList
    . fmap ((fst . head) &&& fmap mergeMarloweCreateTransactions . groupBy (on (==) (.txId)) . fmap snd)
    . groupBy (on (==) fst)
    . fmap (extractBlockId &&& decodeMarloweCreateTransaction)
    . V.toList
  where
    extractBlockId :: CreateTransactionRow -> BlockHeaderHash
    extractBlockId (blockId, _, _, _, _, _, _, _, _, _, _, _, _) = BlockHeaderHash blockId

    mergeMarloweCreateTransactions :: [MarloweCreateTransaction] -> MarloweCreateTransaction
    mergeMarloweCreateTransactions txs =
      MarloweCreateTransaction
        { txId = (head txs).txId
        , newContracts = foldMap (.newContracts) txs
        }

    decodeMarloweCreateTransaction :: CreateTransactionRow -> MarloweCreateTransaction
    decodeMarloweCreateTransaction
      ( _
        , txId
        , txIx
        , address
        , lovelace
        , policyIds
        , tokenNames
        , quantities
        , rolesCurrency
        , state
        , contract
        , metadata
        , payoutValidatorHash
        ) =
        MarloweCreateTransaction
          { txId = TxId txId
          , newContracts =
              Map.singleton (fromIntegral txIx) $
                SomeCreateStep
                  MarloweV1
                  CreateStep
                    { createOutput =
                        decodeTransactionScriptOutput
                          txId
                          txIx
                          address
                          lovelace
                          policyIds
                          tokenNames
                          quantities
                          rolesCurrency
                          state
                          contract
                    , metadata = maybe emptyMarloweTransactionMetadata (runGet get . fromStrict) metadata
                    , payoutValidatorHash = ScriptHash payoutValidatorHash
                    }
          }

getApplyInputsTransactions :: [BlockHeader] -> T.Transaction (Map BlockHeaderHash [MarloweApplyInputsTransaction])
getApplyInputsTransactions blocks = do
  txs <- T.statement blocks do
    dimap (V.fromList . fmap encodeBlockHeader) decodeMarloweApplyInputsTransactions do
      [vectorStatement|
        SELECT
          (ARRAY_AGG(applyTx.blockId))[1] :: bytea,
          applyTx.txId :: bytea,
          (ARRAY_AGG(applyTx.createTxId))[1] :: bytea,
          (ARRAY_AGG(applyTx.createTxIx))[1] :: smallint,
          (ARRAY_AGG(inputTxOut.txId))[1] :: bytea,
          (ARRAY_AGG(inputTxOut.txIx))[1] :: smallint,
          (ARRAY_AGG(inputTxOut.address))[1] :: bytea,
          (ARRAY_AGG(inputContractTxOut.payoutScriptHash))[1] :: bytea,
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
        JOIN (SELECT UNNEST($1 :: bytea[]) AS blockId) as blockIds USING (blockId)
        JOIN marlowe.contractTxOut AS inputContractTxOut
          ON applyTx.inputTxId = inputContractTxOut.txId
            AND applyTx.inputTxIx = inputContractTxOut.txIx
        JOIN marlowe.txOut AS inputTxOut
          ON applyTx.inputTxId = inputTxOut.txId
            AND applyTx.inputTxIx = inputTxOut.txIx
        LEFT JOIN marlowe.contractTxOut
          ON applyTx.txId = contractTxOut.txId
            AND applyTx.outputTxIx = contractTxOut.txIx
        LEFT JOIN marlowe.txOut
          ON applyTx.txId = txOut.txId
            AND applyTx.outputTxIx = txOut.txIx
        LEFT JOIN marlowe.txOutAsset
          ON applyTx.txId = txOutAsset.txId
            AND applyTx.outputTxIx = txOutAsset.txIx
        GROUP BY applyTx.txId
        ORDER BY (ARRAY_AGG(applyTx.blockId))[1]
      |]
  payoutsByTxId <- T.statement txs do
    dimap encodeIds decodePayouts do
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
        JOIN (SELECT UNNEST($1 :: bytea[]) AS txId) as txIds USING (txId)
        LEFT JOIN marlowe.txOutAsset USING (txId, txIx)
        GROUP BY payoutTxOut.txId, payoutTxOut.txIx
        ORDER BY payoutTxOut.txId, payoutTxOut.txIx
      |]
  let addPayouts MarloweApplyInputsTransaction{..} =
        case Map.lookup marloweTransaction.transactionId payoutsByTxId of
          Nothing -> MarloweApplyInputsTransaction{..}
          Just payouts ->
            ( case marloweVersion of
                MarloweV1 ->
                  MarloweApplyInputsTransaction
                    { marloweTransaction =
                        marloweTransaction
                          { output = marloweTransaction.output{payouts}
                          }
                    , ..
                    }
            )
              :: MarloweApplyInputsTransaction
  pure $ fmap addPayouts <$> txs
  where
    encodeIds = V.fromList . join . Map.elems . (fmap . fmap) extractTxId
    extractTxId MarloweApplyInputsTransaction{..} = unTxId $ transactionId marloweTransaction
    decodePayouts =
      Map.fromList
        . fmap ((fst . head) &&& (Map.fromList . fmap snd))
        . groupBy (on (==) fst)
        . fmap ((\(TxOutRef{..}, payout) -> (txId, (TxOutRef{..}, payout))) . decodePayout)
        . V.toList

type ApplyInputsTransactionRow =
  ( ByteString
  , ByteString
  , ByteString
  , Int16
  , ByteString
  , Int16
  , ByteString
  , ByteString
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

decodeMarloweApplyInputsTransactions
  :: Vector ApplyInputsTransactionRow -> Map BlockHeaderHash [MarloweApplyInputsTransaction]
decodeMarloweApplyInputsTransactions =
  Map.fromList
    . fmap ((fst . head) &&& fmap snd)
    . groupBy (on (==) fst)
    . fmap (extractBlockId &&& decodeMarloweApplyInputsTransaction)
    . V.toList
  where
    extractBlockId :: ApplyInputsTransactionRow -> BlockHeaderHash
    extractBlockId (blockId, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =
      BlockHeaderHash blockId

    decodeMarloweApplyInputsTransaction :: ApplyInputsTransactionRow -> MarloweApplyInputsTransaction
    decodeMarloweApplyInputsTransaction
      ( _
        , txId
        , createTxId
        , createTxIx
        , inputTxId
        , inputTxIx
        , inputAddress
        , inputPayoutValidatorHash
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
        ) =
        MarloweApplyInputsTransaction
          { marloweVersion = MarloweV1
          , marloweInput =
              UnspentContractOutput
                { marloweVersion = SomeMarloweVersion MarloweV1
                , txOutRef = decodeTxOutRef inputTxId inputTxIx
                , marloweAddress = Address inputAddress
                , payoutValidatorHash = ScriptHash inputPayoutValidatorHash
                }
          , marloweTransaction =
              snd $
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
                  )
          }

getWithdrawTransactions :: [BlockHeader] -> T.Transaction (Map BlockHeaderHash [MarloweWithdrawTransaction])
getWithdrawTransactions blocks =
  T.statement blocks do
    dimap (V.fromList . fmap encodeBlockHeader) decodeMarloweWithdrawTransactions do
      [vectorStatement|
        SELECT
          (ARRAY_AGG(withdrawalTxIn.blockId))[1] :: bytea,
          withdrawalTxIn.txId :: bytea,
          ARRAY_AGG(withdrawalTxIn.createTxId) :: bytea[],
          ARRAY_AGG(withdrawalTxIn.createTxIx) :: smallint[],
          ARRAY_AGG(withdrawalTxIn.payoutTxId) :: bytea[],
          ARRAY_AGG(withdrawalTxIn.payoutTxIx) :: smallint[]
        FROM marlowe.withdrawalTxIn
        JOIN (SELECT UNNEST($1 :: bytea[]) AS blockId) as blockId USING (blockId)
        GROUP BY txId
        ORDER BY (ARRAY_AGG(withdrawalTxIn.blockId))[1]
      |]

type WithdrawTransactionRow =
  ( ByteString
  , ByteString
  , Vector ByteString
  , Vector Int16
  , Vector ByteString
  , Vector Int16
  )

decodeMarloweWithdrawTransactions :: Vector WithdrawTransactionRow -> Map BlockHeaderHash [MarloweWithdrawTransaction]
decodeMarloweWithdrawTransactions =
  Map.fromList
    . fmap ((fst . head) &&& fmap snd)
    . groupBy (on (==) fst)
    . fmap (extractBlockId &&& decodeMarloweCreateTransaction)
    . V.toList
  where
    extractBlockId :: WithdrawTransactionRow -> BlockHeaderHash
    extractBlockId (blockId, _, _, _, _, _) = BlockHeaderHash blockId

    decodeMarloweCreateTransaction :: WithdrawTransactionRow -> MarloweWithdrawTransaction
    decodeMarloweCreateTransaction
      ( _
        , txId
        , createTxIds
        , createTxIxs
        , payoutTxIds
        , payoutTxIxs
        ) =
        MarloweWithdrawTransaction
          { consumingTx = TxId txId
          , consumedPayouts =
              Map.fromList
                . fmap ((fst . head) &&& (Set.fromList . fmap snd))
                . groupBy (on (==) fst)
                . V.toList
                $ V.zipWith4
                  ( \createTxId createTxIx payoutTxId payoutTxIx ->
                      ( ContractId $ decodeTxOutRef createTxId createTxIx
                      , decodeTxOutRef payoutTxId payoutTxIx
                      )
                  )
                  createTxIds
                  createTxIxs
                  payoutTxIds
                  payoutTxIxs
          }

encodeBlockHeader :: BlockHeader -> ByteString
encodeBlockHeader = unBlockHeaderHash . headerHash
