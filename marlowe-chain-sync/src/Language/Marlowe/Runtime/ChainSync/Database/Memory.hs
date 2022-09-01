{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE QuasiQuotes               #-}

module Language.Marlowe.Runtime.ChainSync.Database.Memory (databaseQueries) where

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Shelley
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Babbage.Tx as Babbage
import Control.Concurrent.STM (STM, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.ByteString.Short (fromShort, toShort)
import Data.Foldable (traverse_)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Data.Monoid (Sum (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.These (These (..))
import Data.Void (Void)
import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.ChainSync.Database (CommitBlocks (..), CommitGenesisBlock (..), CommitRollback (..),
                                                    DatabaseQueries (DatabaseQueries), GetGenesisBlock (..),
                                                    GetHeaderAtPoint (..), GetIntersectionPoints (..), MoveClient (..),
                                                    MoveResult (..))
import Language.Marlowe.Runtime.ChainSync.Genesis (GenesisBlock (..))
import Numeric.Natural (Natural)
import qualified Ouroboros.Network.Point as O
import Prelude hiding (init)

-- | In-memory implementation for the chain sync database queries.
databaseQueries :: GenesisBlock -> STM (DatabaseQueries STM)
databaseQueries genesisBlock = do
  blocksVar <- newTVar Map.empty
  rollbacksVar <- newTVar Map.empty
  consumingBlockIndexVar <- newTVar Map.empty
  producingBlockIndexVar <- newTVar Map.empty
  let

    getGenesisBlock :: GetGenesisBlock STM
    getGenesisBlock = GetGenesisBlock $ pure $ Just genesisBlock

    getHeaderAtPoint :: GetHeaderAtPoint STM
    getHeaderAtPoint = GetHeaderAtPoint \case
      Cardano.ChainPointAtGenesis               -> pure O.Origin
      Cardano.ChainPoint (Cardano.SlotNo slot) (Shelley.HeaderHash hash) -> do
        blocks <- readTVar blocksVar
        let (BlockHeader{..}, _) = fromJust $ Map.lookup (SlotNo slot, BlockHeaderHash $ fromShort hash) blocks
        pure $ O.At $ Cardano.BlockHeader
          (Cardano.SlotNo $ fromIntegral slotNo)
          (Shelley.HeaderHash $ toShort $ unBlockHeaderHash headerHash)
          (Cardano.BlockNo $ fromIntegral blockNo)

    getIntersectionPoints :: GetIntersectionPoints STM
    getIntersectionPoints = GetIntersectionPoints do
      blocks <- readTVar blocksVar
      let
        toChainPoint (slot, BlockHeaderHash hash) = Cardano.ChainPoint
          (Cardano.SlotNo $ fromIntegral slot)
          (Shelley.HeaderHash $ toShort hash)
      pure $ fmap (toChainPoint . fst) $ Map.toDescList blocks

    moveClient :: MoveClient STM
    moveClient = MoveClient performMoveWithRollbackCheck

    performMoveWithRollbackCheck :: ChainPoint -> Move err result -> STM (MoveResult err result)
    performMoveWithRollbackCheck point move = do
      tip <- getTip
      getRollbackPoint >>= \case
        Nothing -> performMove move point >>= \case
          MoveAbort err            -> pure $ Reject err tip
          MoveArrive point' result -> pure $ RollForward result point' tip
          MoveWait                 -> pure $ Wait tip
        Just rollbackPoint -> pure $ RollBack rollbackPoint tip
      where
        getRollbackPoint :: STM (Maybe ChainPoint)
        getRollbackPoint = case point of
          Genesis -> pure Nothing
          At BlockHeader{..} -> runMaybeT do
            rollbacks <- lift $ readTVar rollbacksVar
            MaybeT $ pure $ Map.lookup (slotNo, headerHash) rollbacks

        getTip :: STM ChainPoint
        getTip = do
          blocks <- readTVar blocksVar
          pure $ maybe Genesis (At . fst . fst) $ Map.maxView blocks

    performMove :: Move err result -> ChainPoint -> STM (PerformMoveResult err result)
    performMove = \case
      Fork left right           -> performFork left right
      AdvanceSlots slots        -> performAdvanceSlots slots
      AdvanceBlocks blocks      -> performAdvanceBlocks blocks
      FindConsumingTx txOutRef  -> performFindConsumingTx txOutRef
      FindTx txId               -> performFindTx txId
      Intersect points          -> performIntersect points
      FindConsumingTxs txOutRef -> performFindConsumingTxs txOutRef

    performFork :: Move err1 result1 -> Move err2 result2 -> ChainPoint -> STM (PerformMoveResult (These err1 err2) (These result1 result2))
    performFork left right point = do
      leftMoveResult <- performMove left point
      rightMoveResult <- performMove right point
      let
        alignResults leftHeader leftResult rightHeader rightResult = case compare leftHeader rightHeader of
          EQ -> MoveArrive leftHeader $ These leftResult rightResult
          LT -> MoveArrive leftHeader $ This leftResult
          GT -> MoveArrive rightHeader $ That rightResult
      pure case (leftMoveResult, rightMoveResult) of
        (MoveAbort leftErr, MoveAbort rightErr)                                -> MoveAbort $ These leftErr rightErr
        (MoveAbort leftErr, _)                                                 -> MoveAbort $ This leftErr
        (_, MoveAbort rightErr)                                                -> MoveAbort $ That rightErr
        (MoveArrive leftHeader leftResult, MoveArrive rightHeader rightResult) -> alignResults leftHeader leftResult rightHeader rightResult
        (MoveArrive leftHeader leftResult, MoveWait)                           -> MoveArrive leftHeader $ This leftResult
        (MoveWait, MoveArrive rightHeader rightResult)                         -> MoveArrive rightHeader $ That rightResult
        _                                                                      -> MoveWait

    performAdvanceSlots :: Natural -> ChainPoint -> STM (PerformMoveResult Void ())
    performAdvanceSlots slots point = do
      blocks <- readTVar blocksVar
      let
        targetSlot = case point of
          Genesis            -> fromIntegral slots
          At BlockHeader{..} -> fromIntegral slots + slotNo
      pure
        $ maybe MoveWait (flip MoveArrive ())
        $ find (isAfter (targetSlot - 1))
        $ fst . snd <$> Map.toAscList blocks

    performAdvanceBlocks :: Natural -> ChainPoint -> STM (PerformMoveResult Void ())
    performAdvanceBlocks numBlocks point = do
      blocks <- readTVar blocksVar
      pure
        $ maybe MoveWait (flip MoveArrive ())
        $ listToMaybe
        $ drop (fromIntegral numBlocks)
        $ case point of
            Genesis       -> id
            At pointBlock -> dropWhile (/= pointBlock)
        $ fst . snd <$> Map.toAscList blocks

    performFindConsumingTx :: TxOutRef -> ChainPoint -> STM (PerformMoveResult UTxOError Transaction)
    performFindConsumingTx utxo@TxOutRef{txId = txOutId} point = do
      producingBlockIndex <- readTVar producingBlockIndexVar
      consumingBlockIndex <- readTVar consumingBlockIndexVar
      let mProducingBlock = Map.lookup txOutId producingBlockIndex
      let mConsumingBlock = Map.lookup utxo consumingBlockIndex
      case (mProducingBlock, mConsumingBlock) of
        (Nothing, _) -> pure $ MoveAbort UTxONotFound
        (_, Nothing) -> pure MoveWait
        (_, Just (consumingBlock, txId)) -> do
          blocks <- readTVar blocksVar
          let (blockHeader, txs) = fromJust $ Map.lookup consumingBlock blocks
          pure if At blockHeader < point
             then MoveAbort $ UTxOSpent txId
             else MoveArrive blockHeader $ fromJust $ Map.lookup txId txs

    performFindConsumingTxs
      :: Set TxOutRef
      -> ChainPoint
      -> STM (PerformMoveResult (Map TxOutRef UTxOError) (Map TxOutRef Transaction))
    performFindConsumingTxs utxos point = do
      -- For each requested Tx, perform a FindConsumingTx query
      results <- traverse (flip performFindConsumingTx point) $ Map.fromSet id utxos

      -- partition the results obtained from the individual queries
      let (aborts, Sum waits, arrives) = partitionResults results

      -- limit the found transactions to the ones from the earliest block in the
      -- future.
      let txsFromEarliestBlock = Map.foldrWithKey foldEarliestBlockTxs Nothing arrives

      -- did any of the queries indicate that a result should be awaited?
      let encounteredWaits = waits > 0

      pure case (not $ Map.null aborts, txsFromEarliestBlock, encounteredWaits) of
        -- There were no errors, no consuming Txs were found, and there were UTxOs to
        -- wait for. In this case, the client should wait for results.
        (False, Nothing, True)              -> MoveWait

        -- There were no errors, no consuming Txs were found, and no UTxOs to wait
        -- for. In this case, the client provided an empty set of UTxOs and this is
        -- an error.
        (False, Nothing, False)             -> MoveAbort mempty

        -- There were no errors and some consuming Txs were found. move the client
        -- to the block where the Txs reside and return them.
        (False, Just (blockHeader, txs), _) -> MoveArrive blockHeader txs

        -- There were some errors. Pass these back.
        (True, _, _)                        -> MoveAbort aborts

      where
        foldEarliestBlockTxs utxo (header, tx) Nothing = Just (header, Map.singleton utxo tx)
        foldEarliestBlockTxs utxo (header', tx) (Just (header, txs)) = Just case compare header' header of
          LT -> (header', Map.singleton utxo tx)
          EQ -> (header, Map.insert utxo tx txs)
          GT -> (header, txs)

        partitionResults = Map.foldMapWithKey \utxo -> \case
          MoveArrive header tx -> (mempty, mempty, Map.singleton utxo (header, tx))
          MoveWait             -> (mempty, 1 :: Sum Int, mempty)
          MoveAbort err        -> (Map.singleton utxo err, mempty, mempty)

    performFindTx :: TxId -> ChainPoint -> STM (PerformMoveResult TxError Transaction)
    performFindTx txId point = do
      producingBlockIndex <- readTVar producingBlockIndexVar
      let mProducingBlock = Map.lookup txId producingBlockIndex
      case mProducingBlock of
        Nothing -> pure $ MoveAbort TxNotFound
        Just producingBlock -> do
          blocks <- readTVar blocksVar
          let (blockHeader, txs) = fromJust $ Map.lookup producingBlock blocks
          pure if At blockHeader < point
             then MoveAbort $ TxInPast blockHeader
             else MoveArrive blockHeader $ fromJust $ Map.lookup txId txs

    performIntersect :: [BlockHeader] -> ChainPoint -> STM (PerformMoveResult IntersectError ())
    performIntersect points point = do
      blocks <- readTVar blocksVar
      pure
        $ maybe (MoveAbort IntersectionNotFound) (flip MoveArrive () . fst)
        $ Set.maxView
        $ Set.intersection (Set.fromList points)
        $ Set.fromList
        $ case point of
            Genesis        -> id
            At blockHeader -> dropWhile (< blockHeader)
        $ fmap (fst . snd)
        $ Map.toAscList blocks

    commitRollback :: CommitRollback STM
    commitRollback = CommitRollback \point -> do
      blocks <- readTVar blocksVar
      rollbacks <- readTVar rollbacksVar
      blocks' <- case point of
        Cardano.ChainPointAtGenesis -> do
          writeTVar rollbacksVar $ (Genesis <$ rollbacks) <> (Genesis <$ blocks)
          pure Map.empty
        Cardano.ChainPoint (Cardano.SlotNo slot) _ -> do
          let (after, before) = break ((> SlotNo slot) . fst . fst) $ Map.toDescList blocks
          let (rollbacksAfter, rollbacksBefore) = break ((> SlotNo slot) . fst . fst) $ Map.toDescList rollbacks
          let (_, (blockHeader, _)) = head after
          let rollbacksAfter' = fmap (fmap $ const blockHeader) <$> rollbacksAfter
          let newRollbacks = fmap (const $ At blockHeader) <$> after
          writeTVar rollbacksVar $ Map.fromList $ newRollbacks <> rollbacksAfter' <> rollbacksBefore
          pure $ Map.fromDistinctDescList before
      writeTVar blocksVar blocks'
      let blockKeys = Map.keysSet blocks'
      modifyTVar consumingBlockIndexVar $ Map.filter $ (`Set.member` blockKeys) . fst
      modifyTVar producingBlockIndexVar $ Map.filter (`Set.member` blockKeys)

    commitGenesisBlock :: CommitGenesisBlock STM
    commitGenesisBlock = CommitGenesisBlock $ const $ pure ()


    commitBlocks :: CommitBlocks STM
    commitBlocks = CommitBlocks $ traverse_ commitBlock

    commitBlock :: Cardano.BlockInMode Cardano.CardanoMode -> STM ()
    commitBlock (Cardano.BlockInMode (Cardano.Block blockHeader txs) eraInMode) = do
      blocks <- readTVar blocksVar
      producingBlockIndex <- readTVar producingBlockIndexVar
      let txs' = Map.fromList $ processTx eraInMode producingBlockIndex blocks <$> txs
      let Cardano.BlockHeader (Cardano.SlotNo slot) (Shelley.HeaderHash hash) (Cardano.BlockNo block) = blockHeader
      let blockHeader'@BlockHeader{..} = BlockHeader (SlotNo slot) (BlockHeaderHash $ fromShort hash) (BlockNo block)
      let key = (slotNo, headerHash)
      writeTVar blocksVar $ Map.insert key (blockHeader', txs') blocks
      traverse_ (updateIndexes key) txs'

    processTx
      :: Cardano.EraInMode era Cardano.CardanoMode
      -> Map TxId (SlotNo, BlockHeaderHash)
      -> Map (SlotNo, BlockHeaderHash) (BlockHeader, Map TxId Transaction)
      -> Cardano.Tx era
      -> (TxId, Transaction)
    processTx eraInMode producingBlockIndex blocks tx@(Cardano.Tx body _) = (txId, Transaction{..})
      where
        txId = TxId $ Cardano.serialiseToRawBytes $ Cardano.getTxId body
        validityRange = case body of
          Cardano.TxBody Cardano.TxBodyContent{..} -> case txValidityRange of
            (Cardano.TxValidityNoLowerBound, Cardano.TxValidityNoUpperBound _) ->
              Unbounded
            (Cardano.TxValidityLowerBound _ (Cardano.SlotNo slot), Cardano.TxValidityNoUpperBound _) ->
              MinBound $ SlotNo slot
            (Cardano.TxValidityNoLowerBound, Cardano.TxValidityUpperBound _ (Cardano.SlotNo slot)) ->
              MaxBound $ SlotNo slot
            (Cardano.TxValidityLowerBound _ (Cardano.SlotNo s1), Cardano.TxValidityUpperBound _ (Cardano.SlotNo s2)) ->
              MinMaxBound (SlotNo s1) (SlotNo s2)
        metadata = Nothing
        inputs = extractInputs producingBlockIndex blocks tx
        outputs = case body of
          Cardano.TxBody Cardano.TxBodyContent{..} -> extractOutput eraInMode <$> txOuts
        mintedTokens = extractTokens body

    extractInputs
      :: Map TxId (SlotNo, BlockHeaderHash)
      -> Map (SlotNo, BlockHeaderHash) (BlockHeader, Map TxId Transaction)
      -> Shelley.Tx era
      -> Set TransactionInput
    extractInputs producingBlockIndex blocks tx@(Cardano.Tx body _) = case body of
      Cardano.TxBody Cardano.TxBodyContent{..} -> Set.fromList do
        txIn@(Cardano.TxIn cardanoTxId (Cardano.TxIx txIx)) <- fst <$> txIns
        let txId = TxId $ Cardano.serialiseToRawBytes cardanoTxId
        let
          TransactionOutput{..} = fromJust
            $ listToMaybe . drop (fromIntegral txIx) . outputs
            =<< Map.lookup txId . snd
            =<< flip Map.lookup blocks
            =<< Map.lookup txId producingBlockIndex
        let
        let
          redeemer :: Maybe Redeemer
          redeemer = case tx of
            Shelley.ShelleyTx Cardano.ShelleyBasedEraAlonzo alonzoTx@Alonzo.ValidatedTx{} -> do
              (Alonzo.Data alonzoDatum, _) <-
                Alonzo.indexedRdmrs alonzoTx $ Alonzo.Spending $ Shelley.toShelleyTxIn txIn
              pure $ Redeemer $ fromPlutusData alonzoDatum
            Shelley.ShelleyTx Cardano.ShelleyBasedEraBabbage babbageTx@Babbage.ValidatedTx{} -> do
              (Alonzo.Data babbageDatum, _) <-
                Babbage.indexedRdmrs babbageTx $ Babbage.Spending $ Shelley.toShelleyTxIn txIn
              pure $ Redeemer $ fromPlutusData babbageDatum
            _ -> Nothing
        pure $ TransactionInput
          { txId
          , txIx = fromIntegral txIx
          , address
          , datumBytes = datum
          , redeemer
          }

    extractOutput
      :: Cardano.EraInMode era Cardano.CardanoMode
      -> Cardano.TxOut Cardano.CtxTx era
      -> TransactionOutput
    extractOutput eraInMode (Cardano.TxOut cardanoAddress value cardanoDatum _) =
      TransactionOutput{..}
        where
          assets = case value of
            Cardano.TxOutAdaOnly _ lovelace ->
              Assets (fromIntegral lovelace) $ Tokens mempty
            Cardano.TxOutValue _ value' ->
              Assets (fromIntegral $ Cardano.selectLovelace value') $ valueToTokens value'
          datumHash = DatumHash . Cardano.serialiseToRawBytes <$> case cardanoDatum of
            Cardano.TxOutDatumNone        -> Nothing
            Cardano.TxOutDatumHash _ hash -> Just hash
            Cardano.TxOutDatumInTx _ d    -> Just $ Cardano.hashScriptData d
            Cardano.TxOutDatumInline _ d  -> Just $ Cardano.hashScriptData d
          datum = fromPlutusData . Shelley.toPlutusData <$> case cardanoDatum of
            Cardano.TxOutDatumNone       -> Nothing
            Cardano.TxOutDatumHash _ _   -> Nothing
            Cardano.TxOutDatumInTx _ d   -> Just d
            Cardano.TxOutDatumInline _ d -> Just d
          address = Address case eraInMode of
            Cardano.ByronEraInCardanoMode   -> Cardano.serialiseToRawBytes cardanoAddress
            Cardano.ShelleyEraInCardanoMode -> Cardano.serialiseToRawBytes cardanoAddress
            Cardano.AllegraEraInCardanoMode -> Cardano.serialiseToRawBytes cardanoAddress
            Cardano.MaryEraInCardanoMode    -> Cardano.serialiseToRawBytes cardanoAddress
            Cardano.AlonzoEraInCardanoMode  -> Cardano.serialiseToRawBytes cardanoAddress
            Cardano.BabbageEraInCardanoMode -> Cardano.serialiseToRawBytes cardanoAddress

    extractTokens :: Shelley.TxBody era -> Tokens
    extractTokens (Cardano.TxBody Cardano.TxBodyContent{..}) = case txMintValue of
      Cardano.TxMintNone            -> Tokens mempty
      Cardano.TxMintValue _ value _ -> valueToTokens value

    valueToTokens = Tokens
      . Map.fromList
      . mapMaybe \case
          (Cardano.AdaAssetId, _) ->
            Nothing
          (Cardano.AssetId policyId (Cardano.AssetName name), Cardano.Quantity quantity) ->
            Just
              ( AssetId (PolicyId $ Cardano.serialiseToRawBytes policyId) (TokenName name)
              , fromIntegral quantity
              )
      . Cardano.valueToList

    updateIndexes :: (SlotNo, BlockHeaderHash) -> Transaction -> STM ()
    updateIndexes key Transaction{inputs, txId = txInId} = do
      modifyTVar consumingBlockIndexVar
        $ Map.union
        $ Map.fromList
        $ fmap (\TransactionInput{..} -> (TxOutRef{..}, (key, txInId)))
        $ Set.toList inputs
      modifyTVar producingBlockIndexVar $ Map.insert txInId key

  pure $ DatabaseQueries
    commitRollback
    commitBlocks
    commitGenesisBlock
    getHeaderAtPoint
    getIntersectionPoints
    getGenesisBlock
    moveClient

data PerformMoveResult err result
  = MoveWait
  | MoveAbort err
  | MoveArrive BlockHeader result
