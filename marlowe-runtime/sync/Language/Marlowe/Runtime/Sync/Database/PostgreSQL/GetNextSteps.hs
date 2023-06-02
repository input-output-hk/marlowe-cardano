{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetNextSteps
  where

import Control.Applicative ((<|>))
import qualified Control.Foldl as Fold
import Data.Binary (get)
import Data.Binary.Get (runGet)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time (localTimeToUTC, utc)
import qualified Data.Vector as V
import Hasql.TH (foldStatement, maybeStatement, vectorStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Core.V1.Semantics (MarloweData(..), MarloweParams(MarloweParams))
import Language.Marlowe.Runtime.ChainSync.Api
  ( Address(Address)
  , AssetId(AssetId)
  , Assets(..)
  , BlockHeader(..)
  , BlockHeaderHash(..)
  , ChainPoint
  , PolicyId(PolicyId)
  , TokenName(TokenName)
  , Tokens(Tokens)
  , TxId(..)
  , TxOutRef(..)
  , WithGenesis(..)
  , fromDatum
  )
import Language.Marlowe.Runtime.Core.Api
  ( ContractId(..)
  , MarloweVersion(..)
  , MarloweVersionTag(V1)
  , Payout(..)
  , Transaction(..)
  , TransactionOutput(..)
  , TransactionScriptOutput(..)
  , emptyMarloweTransactionMetadata
  )
import Language.Marlowe.Runtime.History.Api (ContractStep(..), RedeemStep(..))
import Language.Marlowe.Runtime.Sync.Database (Next(..))
import qualified Plutus.V2.Ledger.Api as PV2
import Prelude hiding (init)
import Witherable (catMaybes, mapMaybe)

getNextSteps :: MarloweVersion v -> ContractId -> ChainPoint -> T.Transaction (Next (ContractStep v))
getNextSteps MarloweV1 contractId point = do
  orient point >>= \case
    RolledBack toPoint -> pure $ Rollback toPoint
    AtTip -> pure Wait
    BeforeTip -> getNextTxIds contractId point >>= \case
      Nothing -> pure Wait
      Just NextTxIds{..} -> do
        applySteps <- getApplySteps nextBlock contractId nextApplyTxIds
        redeemSteps <- getRedeemSteps nextWithdrawalTxIds
        pure $ Next nextBlock $ (ApplyTransaction <$> applySteps) <> (RedeemPayout <$> redeemSteps)

data Orientation
  = BeforeTip
  | AtTip
  | RolledBack ChainPoint

orient :: ChainPoint -> T.Transaction Orientation
orient Genesis = pure BeforeTip
orient (At BlockHeader{..}) = T.statement (unBlockHeaderHash headerHash) $ decodeResult <$>
  [maybeStatement|
    SELECT
      block.slotNo :: bigint,
      block.id :: bytea,
      block.blockNo :: bigint
    FROM marlowe.rollbackBlock
    JOIN marlowe.block ON block.id = rollbackBlock.toBlock
    WHERE rollbackBlock.fromBlock = $1 :: bytea
  |]
  where
    decodeResult Nothing = BeforeTip
    decodeResult (Just (slot, hash, block))
      | slot < 0 || block < 0 = RolledBack Genesis
      | otherwise = RolledBack $ At BlockHeader
        { slotNo = fromIntegral slot
        , headerHash = BlockHeaderHash hash
        , blockNo = fromIntegral block
        }

data NextTxIds = NextTxIds
  { nextBlock :: BlockHeader
  , nextApplyTxIds :: [TxId]
  , nextWithdrawalTxIds :: [TxId]
  }

getNextTxIds :: ContractId -> ChainPoint -> T.Transaction (Maybe NextTxIds)
getNextTxIds (ContractId TxOutRef{..}) point = T.statement params $ fmap (NextTxIds <$> decodeBlockHeader <*> decodeApplyTxIds <*> decodeWithdrawalTxIds) <$>
  [maybeStatement|
    WITH params (createTxId, createTxIx, afterSlot) AS
      ( SELECT $1 :: bytea, $2 :: smallint, $3 :: bigint
      )
    , nextApplyTxIds (slotNo, blockHeaderHash, blockNo, txIds) AS
      ( SELECT
          slotNo,
          (ARRAY_AGG(blockId))[1],
          (ARRAY_AGG(blockNo))[1],
          ARRAY_AGG(txId)
        FROM marlowe.applyTx
        JOIN params USING (createTxId, createTxIx)
        WHERE slotNo > params.afterSlot
        GROUP BY slotNo
        ORDER BY slotNo
        LIMIT 1
      )
    , nextWithdrawalTxIds (slotNo, blockHeaderHash, blockNo, txIds) AS
      ( SELECT
          slotNo,
          (ARRAY_AGG(blockId))[1],
          (ARRAY_AGG(blockNo))[1],
          ARRAY_AGG(txId)
        FROM marlowe.withdrawalTxIn
        JOIN params USING (createTxId, createTxIx)
        WHERE slotNo > params.afterSlot
        GROUP BY slotNo
        ORDER BY slotNo
        LIMIT 1
      )
    SELECT
      COALESCE (nextApplyTxIds.slotNo, nextWithdrawalTxIds.slotNo) :: bigint,
      COALESCE (nextApplyTxIds.blockHeaderHash, nextWithdrawalTxIds.blockHeaderHash) :: bytea,
      COALESCE (nextApplyTxIds.blockNo, nextWithdrawalTxIds.blockNo) :: bigint,
      COALESCE (nextApplyTxIds.txIds, '{}') :: bytea[],
      COALESCE (nextWithdrawalTxIds.txIds, '{}') :: bytea[]
    FROM nextApplyTxIds
    FULL OUTER JOIN nextWithdrawalTxIds
    ON nextApplyTxIds.slotNo = nextWithdrawalTxIds.slotNo
    ORDER BY COALESCE (nextApplyTxIds.slotNo, nextWithdrawalTxIds.slotNo)
    LIMIT 1
  |]
  where
    params = (unTxId txId, fromIntegral txIx, pointSlot)
    pointSlot = case point of
      Genesis -> -1
      At BlockHeader{..} -> fromIntegral slotNo
    decodeBlockHeader (slot, hash, block, _, _) = BlockHeader
      { slotNo = fromIntegral slot
      , headerHash = BlockHeaderHash hash
      , blockNo = fromIntegral block
      }
    decodeApplyTxIds (_, _, _, ids, _) = decodeTxIds ids
    decodeWithdrawalTxIds (_, _, _, _, ids) = decodeTxIds ids
    decodeTxIds ids = V.toList $ V.map TxId ids

getApplySteps :: BlockHeader -> ContractId -> [TxId] -> T.Transaction [Transaction 'V1]
getApplySteps blockHeader contractId txIds = T.statement params $
  [foldStatement|
    WITH txIds (txId) AS
      ( SELECT * FROM UNNEST ($1 :: bytea[])
      )
    SELECT
      applyTx.txId :: bytea,
      applyTx.metadata :: bytea?,
      applyTx.invalidBefore :: timestamp,
      applyTx.invalidHereafter :: timestamp,
      applyTx.inputs :: bytea,
      applyTx.outputTxIx :: smallint?,
      payoutOut.txIx :: smallint?,
      payoutOut.address :: bytea?,
      payoutOut.lovelace :: bigint?,
      payoutOut.policyIds :: bytea[]?,
      payoutOut.tokenNames :: bytea[]?,
      payoutOut.quantities :: bigint[]?,
      payoutOut.rolesCurrency :: bytea?,
      payoutOut.role :: bytea?,
      contractOut.address :: bytea?,
      contractOut.lovelace :: bigint?,
      contractOut.policyIds :: bytea[]?,
      contractOut.tokenNames :: bytea[]?,
      contractOut.quantities :: bigint[]?,
      contractOut.rolesCurrency :: bytea?,
      contractOut.state :: bytea?,
      contractOut.contract :: bytea?
    FROM marlowe.applyTx
    JOIN txIds USING (txId)
    LEFT JOIN
      ( SELECT
          payoutTxOut.txId AS txId,
          payoutTxOut.txIx AS txIx,
          (ARRAY_AGG(txOut.address))[1] AS address,
          (ARRAY_AGG(txOut.lovelace))[1] AS lovelace,
          (ARRAY_REMOVE(ARRAY_AGG(txOutAsset.policyId), NULL)) AS policyIds,
          (ARRAY_REMOVE(ARRAY_AGG(txOutAsset.name), NULL)) AS tokenNames,
          (ARRAY_REMOVE(ARRAY_AGG(txOutAsset.quantity), NULL)) AS quantities,
          (ARRAY_AGG(payoutTxOut.rolesCurrency))[1] AS rolesCurrency,
          (ARRAY_AGG(payoutTxOut.role))[1] AS role
        FROM marlowe.payoutTxOut
        JOIN txIds USING (txId)
        JOIN marlowe.txOut USING (txId, txIx)
        LEFT JOIN marlowe.txOutAsset USING (txId, txIx)
        GROUP BY payoutTxOut.txId, payoutTxOut.txIx
      ) AS payoutOut USING (txId)
    LEFT JOIN
      ( SELECT
          contractTxOut.txId AS txId,
          contractTxOut.txIx AS txIx,
          (ARRAY_AGG(txOut.address))[1] AS address,
          (ARRAY_AGG(txOut.lovelace))[1] AS lovelace,
          (ARRAY_REMOVE(ARRAY_AGG(txOutAsset.policyId), NULL)) AS policyIds,
          (ARRAY_REMOVE(ARRAY_AGG(txOutAsset.name), NULL)) AS tokenNames,
          (ARRAY_REMOVE(ARRAY_AGG(txOutAsset.quantity), NULL)) AS quantities,
          (ARRAY_AGG(contractTxOut.rolesCurrency))[1] AS rolesCurrency,
          (ARRAY_AGG(contractTxOut.state))[1] AS state,
          (ARRAY_AGG(contractTxOut.contract))[1] AS contract
        FROM marlowe.contractTxOut
        JOIN txIds USING (txId)
        JOIN marlowe.txOut USING (txId, txIx)
        LEFT JOIN marlowe.txOutAsset USING (txId, txIx)
        GROUP BY contractTxOut.txId, contractTxOut.txIx
      ) AS contractOut USING (txId)
  |] resultFold
  where
    resultFold = catMaybes . Map.elems <$> Fold.groupBy extractTxId (mergeWithChildren extractTx addPayouts foldPayouts)

    extractTxId
      ( txId
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      ) = txId

    extractTx
      ( txId
      , metadata
      , invalidBefore
      , invalidHereafter
      , inputs
      , outputTxIx
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , outputAddress
      , outputLovelace
      , outputPolicyIds
      , outputTokenNames
      , outputQuantities
      , outputRolesCurrency
      , outputState
      , outputContract
      ) = Transaction
        { transactionId = TxId txId
        , contractId
        , metadata = maybe emptyMarloweTransactionMetadata (runGet get . fromStrict) metadata
        , blockHeader
        , validityLowerBound = localTimeToUTC utc invalidBefore
        , validityUpperBound = localTimeToUTC utc invalidHereafter
        , inputs = fromJust $ fromDatum $ runGet get $ fromStrict inputs
        , output = TransactionOutput
          { payouts = mempty
          , scriptOutput = do
            txIx <- outputTxIx
            address <- outputAddress
            lovelace <- outputLovelace
            policyIds <- V.toList <$> outputPolicyIds
            tokenNames <- V.toList <$> outputTokenNames
            quantities <- V.toList <$> outputQuantities
            rolesCurrency :: ByteString <- outputRolesCurrency
            state <- outputState
            contract <- outputContract
            pure TransactionScriptOutput
              { address = Address address
              , assets = Assets
                { ada = fromIntegral lovelace
                , tokens = Tokens $ Map.fromList $ zipWith3
                  (\p t q -> (AssetId (PolicyId p) (TokenName t), fromIntegral q))
                  policyIds
                  tokenNames
                  quantities
                }
              , utxo = TxOutRef (TxId txId) (fromIntegral txIx)
              , datum = MarloweData
                { marloweParams = MarloweParams $ PV2.CurrencySymbol $ PV2.toBuiltin rolesCurrency
                , marloweState = fromJust $ fromDatum $ runGet get $ fromStrict state
                , marloweContract = fromJust $ fromDatum $ runGet get $ fromStrict contract
                }
              }
          }
        } :: Transaction 'V1

    foldPayouts = Map.fromList . mapMaybe (\row -> (,) <$> extractPayoutTxOutRef row <*> extractPayout row) <$> Fold.list

    extractPayoutTxOutRef
      ( txId
      , _
      , _
      , _
      , _
      , _
      , txIx
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      ) = TxOutRef (TxId txId) . fromIntegral <$> txIx

    extractPayout
      ( _
      , _
      , _
      , _
      , _
      , _
      , _
      , address
      , lovelace
      , policyIds
      , tokenNames
      , quantities
      , rolesCurrency
      , role
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      , _
      ) = Payout @'V1
        <$> (Address <$> address)
        <*> ( Assets
              <$> (fromIntegral <$> lovelace)
              <*> ( Tokens . Map.fromList <$>
                    ( zipWith3 (\p t q -> (AssetId (PolicyId p) (TokenName t), fromIntegral q))
                        <$> (V.toList <$> policyIds)
                        <*> (V.toList <$> tokenNames)
                        <*> (V.toList <$> quantities)
                    )
                  )
            )
        <*> ( AssetId
              <$> (PolicyId <$> rolesCurrency)
              <*> (TokenName <$> role)
            )

    addPayouts :: Map TxOutRef (Payout v) -> Transaction v -> Transaction v
    addPayouts payouts' tx@Transaction{output} = tx
      { output = output
          { payouts = payouts output <> payouts'
          }
      }

    params = V.fromList $ unTxId <$> txIds

getRedeemSteps :: [TxId] -> T.Transaction [RedeemStep 'V1]
getRedeemSteps txIds = T.statement params $ fmap decodeRow . V.toList <$>
  [vectorStatement|
    WITH txIds (txId) AS
      ( SELECT * FROM UNNEST ($1 :: bytea[])
      )
    SELECT
      payoutTxOut.txId :: bytea,
      payoutTxOut.txIx :: smallint,
      withdrawalTxIn.txId :: bytea,
      payoutTxOut.rolesCurrency :: bytea,
      payoutTxOut.role :: bytea
    FROM marlowe.withdrawalTxIn
    JOIN txIds USING (txId)
    JOIN marlowe.payoutTxOut
      ON withdrawalTxIn.payoutTxId = payoutTxOut.txId
      AND withdrawalTxIn.payoutTxIx = payoutTxOut.txIx
  |]
  where
    params = V.fromList $ unTxId <$> txIds
    decodeRow (payoutTxId, payoutTxIx, txId, rolesCurrency, role) = RedeemStep
      { utxo = TxOutRef (TxId payoutTxId) (fromIntegral payoutTxIx)
      , redeemingTx = TxId txId
      , datum = AssetId (PolicyId rolesCurrency) (TokenName role)
      } :: RedeemStep 'V1


mergeWithChildren :: (a -> r) -> (c -> r -> r) -> Fold.Fold a c -> Fold.Fold a (Maybe r)
mergeWithChildren extractParent mergeChild (Fold.Fold fChild iChild pChild) = Fold.Fold foldRow (Nothing, iChild) mapResult
  where
    foldRow (mParent, children) row = (mParent <|> Just (extractParent row), fChild children row)
    mapResult (mParent, children) = mergeChild (pChild children) <$> mParent
