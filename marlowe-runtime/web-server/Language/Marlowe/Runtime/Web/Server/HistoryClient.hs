{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Marlowe.Runtime.Web.Server.HistoryClient
  where

import Cardano.Api (getTxId)
import Control.Arrow (arr)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, atomically)
import Control.Error (note)
import Data.Foldable (foldlM)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import Data.Void (Void)
import Language.Marlowe.Protocol.Sync.Client
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (TxId)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api
  (ContractId, MarloweVersion, Transaction(..), TransactionOutput(..), TransactionScriptOutput)
import Language.Marlowe.Runtime.History.Api (ContractStep(..), CreateStep(..))
import Language.Marlowe.Runtime.Transaction.Api (ContractCreated, InputsApplied(InputsApplied, txBody))
import Language.Marlowe.Runtime.Web.Server.DTO (ContractRecord(..), SomeTransaction(..))
import qualified Language.Marlowe.Runtime.Web.Server.DTO as TxRecord (TxRecord(..))
import Language.Marlowe.Runtime.Web.Server.TxClient (TempTx(..))
import Language.Marlowe.Runtime.Web.Server.Util (applyRangeToAscList)
import Observe.Event (Event, EventBackend, addField, withEvent)
import Observe.Event.BackendModification (EventBackendModifiers, modifyEventBackend)
import Observe.Event.DSL (FieldSpec(..), SelectorSpec(..))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))
import Servant.Pagination

compile $ SelectorSpec ["history", "client"]
  [ ["load", "contract", "not", "found"] ≔ ''Void
  , ["load", "contract", "found"] ≔ ''String
  , ["load", "contract", "roll", "forward"] ≔ ''String
  , ["load", "contract", "roll", "backward"] ≔ ''String
  , ["load", "contract", "roll", "back", "creation"] ≔ ''Void
  , ["load", "contract", "wait"] ≔ ''Void
  , ["load", "transactions", "not", "found"] ≔ ''Void
  , ["load", "transactions", "found"] ≔ ''String
  , ["load", "transactions", "roll", "forward"] ≔ ''String
  , ["load", "transactions", "roll", "backward"] ≔ ''String
  , ["load", "transactions", "roll", "back", "creation"] ≔ ''Void
  , ["load", "transactions", "wait"] ≔ ''Void
  , ["load", "tx", "not", "found"] ≔ ''Void
  , ["load", "tx", "contract", "found"] ≔ ''String
  , ["load", "tx", "roll", "forward"] ≔ FieldSpec ["load", "tx", "roll", "forward"]
      [ ["found", "tx"] ≔ ''String
      , ["found", "consumer"] ≔ ''String
      ]
  , ["load", "tx", "roll", "backward"] ≔ FieldSpec ["load", "tx", "roll", "backward"]
      [ ["lost", "tx"] ≔ ''String
      , ["lost", "consumer"] ≔ ''String
      ]
  , ["load", "tx", "roll", "back", "creation"] ≔ ''Void
  , ["load", "tx", "wait"] ≔ ''Void
  ]

data HistoryClientDependencies r = HistoryClientDependencies
  { runMarloweSyncClient :: forall a. MarloweSyncClient IO a -> IO a
  , lookupTempContract :: ContractId -> STM (Maybe (TempTx ContractCreated))
  , lookupTempTransaction :: ContractId -> TxId -> STM (Maybe (TempTx InputsApplied))
  , getTempTransactions :: ContractId -> STM [TempTx InputsApplied]
  , eventBackend :: EventBackend IO r HistoryClientSelector
  }

-- | Signature for a delegate that loads the state of a single contract.
type LoadContract r m
   = forall r'
   . EventBackendModifiers r r'
  -> ContractId               -- ^ ID of the contract to load
  -> m (Maybe (Either (TempTx ContractCreated) ContractRecord)) -- ^ Nothing if the ID is not found

data LoadTxError
  = ContractNotFound
  | TxNotFound

-- | Signature for a delegate that loads a list of transactions for a contract.
type LoadTransactions r m
   = forall r'
   . EventBackendModifiers r r'
  -> ContractId -- ^ ID of the contract to load transactions for.
  -> Maybe TxId -- ^ ID of the contract to start from.
  -> Int -- ^ Limit: the maximum number of contract headers to load.
  -> Int -- ^ Offset: how many contract headers after the initial one to skip.
  -> RangeOrder -- ^ Whether to load an ascending or descending list.
  -> m (Either LoadTxError [Either (TempTx InputsApplied) SomeTransaction])

-- | Signature for a delegate that loads a transaction for a contract.
type LoadTransaction r m
   = forall r'
   . EventBackendModifiers r r'
  -> ContractId -- ^ ID of the contract to load transactions for.
  -> TxId -- ^ ID of the transaction to load.
  -> m (Maybe (Either (TempTx InputsApplied) TxRecord.TxRecord))

-- | Public API of the HistoryClient
data HistoryClient r = HistoryClient
  { loadContract :: LoadContract r IO -- ^ Load contract headers from the indexer.
  , loadTransactions :: LoadTransactions r IO -- ^ Load transactions for a contract from the indexer.
  , loadTransaction :: LoadTransaction r IO -- ^ Load a transaction for a contract
  }

historyClient :: Component IO (HistoryClientDependencies r) (HistoryClient r)
historyClient = arr \HistoryClientDependencies{..} -> HistoryClient
  { loadContract = \mods contractId -> do
      result <- runMarloweSyncClient $ loadContractClient (modifyEventBackend mods eventBackend) contractId
      case result of
        Nothing -> atomically $ fmap Left <$> lookupTempContract contractId
        Just contract -> pure $ Just $ Right contract
  , loadTransaction = \mods contractId txId -> do
      result <- runMarloweSyncClient $ loadTransactionClient (modifyEventBackend mods eventBackend) contractId txId
      case result of
        Nothing -> atomically $ fmap Left <$> lookupTempTransaction contractId txId
        Just contract -> pure $ Just $ Right contract
  , loadTransactions = \mods contractId startFrom limit offset order -> do
      tempTxs <- atomically $ getTempTransactions contractId
      txs <- runMarloweSyncClient (loadTransactionsClient (modifyEventBackend mods eventBackend) contractId)
      let
        allTxs = do
          txs' <- txs
          pure $ (Right <$> txs') <> (Left <$> tempTxs)
      pure
        $ note TxNotFound . applyRangeToAscList transactionId' startFrom limit offset order
        =<< allTxs
  }
  where
    transactionId' (Left (TempTx _ _ InputsApplied{..})) = fromCardanoTxId $ getTxId txBody
    transactionId' (Right (SomeTransaction _ Transaction{..})) = transactionId

loadContractClient
  :: EventBackend IO r HistoryClientSelector
  -> ContractId
  -> MarloweSyncClient IO (Maybe ContractRecord)
loadContractClient eb contractId = MarloweSyncClient $ pure clientInit
  where
    clientInit = SendMsgFollowContract contractId ClientStFollow
      { recvMsgContractNotFound =
          withEvent eb LoadContractNotFound $ const $ pure Nothing
      , recvMsgContractFound = \block version createStep@CreateStep{..} ->
          withEvent eb LoadContractFound \ev -> do
            addField ev $ show block
            pure $ clientIdle block version createStep createOutput
      }

    clientIdle
      :: Chain.BlockHeader
      -> MarloweVersion v
      -> CreateStep v
      -> TransactionScriptOutput v
      -> ClientStIdle v IO (Maybe ContractRecord)
    clientIdle createBlock version createStep lastOutput = SendMsgRequestNext ClientStNext
      { recvMsgRollForward = \block steps ->
          withEvent eb LoadContractRollForward \ev -> do
            addField ev $ show block
            let
              mLastOutput =  listToMaybe
                $ mapMaybe \case
                    ApplyTransaction Transaction{output=TransactionOutput{..}} -> scriptOutput
                    _ -> Just lastOutput
                $ reverse steps
            pure case mLastOutput of
              Nothing -> SendMsgDone $ Just $ ContractRecord
                version
                contractId
                createBlock
                createStep
                Nothing
              Just lastOutput' -> clientIdle createBlock version createStep lastOutput'
      , recvMsgRollBackward = \block ->
          withEvent eb LoadContractRollBackward \ev -> do
            addField ev $ show block
            pure $ clientIdle createBlock version createStep lastOutput
      , recvMsgRollBackCreation =
          withEvent eb LoadContractRollBackCreation $ const $ pure Nothing
      , recvMsgWait = do
          withEvent eb LoadContractWait
            $ const
            $ pure
            $ SendMsgCancel
            $ SendMsgDone
            $ Just
            $ ContractRecord version contractId createBlock createStep
            $ Just lastOutput
      }

loadTransactionsClient
  :: EventBackend IO r HistoryClientSelector
  -> ContractId
  -> MarloweSyncClient IO (Either LoadTxError [SomeTransaction])
loadTransactionsClient eb contractId = MarloweSyncClient $ pure clientInit
  where
    clientInit = SendMsgFollowContract contractId ClientStFollow
      { recvMsgContractNotFound =
          withEvent eb LoadTransactionsNotFound $ const $ pure $ Left ContractNotFound
      , recvMsgContractFound = \block version _ ->
          withEvent eb LoadTransactionsFound \ev -> do
            addField ev $ show block
            pure $ clientIdle version []
      }

    clientIdle
      :: MarloweVersion v
      -> [Transaction v]
      -> ClientStIdle v IO (Either LoadTxError [SomeTransaction])
    clientIdle version transactions = SendMsgRequestNext ClientStNext
      { recvMsgRollForward = \block steps ->
          withEvent eb LoadTransactionsRollForward \ev -> do
            addField ev $ show block
            let
              extractTx = \case
                ApplyTransaction tx -> Just tx
                _ -> Nothing
              newTransactions = sortOn transactionId $ mapMaybe extractTx steps
              hasNoScriptOutput = isNothing . scriptOutput . output
            pure if any hasNoScriptOutput newTransactions
              then SendMsgDone $ Right $ SomeTransaction version <$> (transactions <> newTransactions)
              else clientIdle version (transactions <> newTransactions)
      , recvMsgRollBackward = \block ->
          withEvent eb LoadTransactionsRollBackward \ev -> do
            addField ev $ show block
            pure $ clientIdle version $ takeWhile ((<= block) . blockHeader) transactions
      , recvMsgRollBackCreation =
          withEvent eb LoadTransactionsRollBackCreation $ const $ pure $ Left ContractNotFound
      , recvMsgWait = do
          withEvent eb LoadTransactionsWait
            $ const
            $ pure
            $ SendMsgCancel
            $ SendMsgDone
            $ Right
            $ SomeTransaction version
            <$> transactions
      }

loadTransactionClient
  :: EventBackend IO r' HistoryClientSelector
  -> ContractId
  -> TxId
  -> MarloweSyncClient IO (Maybe TxRecord.TxRecord)
loadTransactionClient eb contractId txId = MarloweSyncClient $ pure clientInit
  where
    clientInit = SendMsgFollowContract contractId ClientStFollow
      { recvMsgContractNotFound =
          withEvent eb LoadTxNotFound $ const $ pure Nothing
      , recvMsgContractFound = \block version CreateStep{..} ->
          withEvent eb LoadTxContractFound \ev -> do
            addField ev $ show block
            pure $ clientIdle version $ SeekTx $ pure (block, createOutput)
      }

    clientIdle :: MarloweVersion v -> SeekTx v -> ClientStIdle v IO (Maybe TxRecord.TxRecord)
    clientIdle version seekTx = SendMsgRequestNext ClientStNext
      { recvMsgRollForward = \_ steps ->
          withEvent eb LoadTxRollForward \ev -> do
            seekTx' <- foldlM (stepSeekTx ev) seekTx steps
            pure case seekTx' of
              Done prevScriptOutputs' tx _ consumingTx -> SendMsgDone
                $ Just
                $ mkTxRecord version (snd $ NE.head prevScriptOutputs') tx
                $ Just consumingTx
              _ -> clientIdle version seekTx'
      , recvMsgRollBackward = \block ->
          withEvent eb LoadTxRollBackward \ev -> do
            clientIdle version <$> rollbackStepTx ev block seekTx
      , recvMsgRollBackCreation =
          withEvent eb LoadTxRollBackCreation $ const $ pure Nothing
      , recvMsgWait = do
          withEvent eb LoadTxWait
            $ const
            $ pure
            $ SendMsgCancel
            $ SendMsgDone
            $ case seekTx of
              Done prevScriptOutputs' tx _ consumingTx ->
                Just $ mkTxRecord version (snd $ NE.head prevScriptOutputs') tx $ Just consumingTx
              SeekConsuming prevScriptOutputs' tx ->
                Just $ mkTxRecord version (snd $ NE.head prevScriptOutputs') tx Nothing
              _ -> Nothing
      }

    stepSeekTx
      :: Event IO r HistoryClientSelector LoadTxRollForwardField
      -> SeekTx v
      -> ContractStep v
      -> IO (SeekTx v)
    stepSeekTx ev seekTx = \case
      ApplyTransaction newTx -> case seekTx of
        SeekTx prevOutputs
          | txId == transactionId newTx -> do
              addField ev $ FoundTx $ show txId
              pure $ SeekConsuming prevOutputs newTx
          | otherwise -> pure case scriptOutput $ output newTx of
              Nothing -> seekTx
              Just scriptOutput -> SeekTx $ (blockHeader newTx, scriptOutput) <| prevOutputs
        SeekConsuming prevOutputs tx -> do
          addField ev $ FoundConsumer $ show $ transactionId newTx
          pure $ Done prevOutputs tx (blockHeader newTx) $ transactionId newTx
        _ -> pure seekTx
      _ -> pure seekTx

    mkTxRecord
      :: MarloweVersion v
      -> TransactionScriptOutput v
      -> Transaction v
      -> Maybe TxId
      -> TxRecord.TxRecord
    mkTxRecord version input tx consumingTx = TxRecord.TxRecord{..}

    rollbackStepTx
      :: Event IO r HistoryClientSelector LoadTxRollBackwardField
      -> Chain.BlockHeader
      -> SeekTx v
      -> IO (SeekTx v)
    rollbackStepTx ev block = \case
      SeekTx prevOutputs -> pure $ SeekTx $ rollbackPrevOutputs block prevOutputs
      SeekConsuming prevOutputs tx
        | block < blockHeader tx -> do
            addField ev $ LostTx $ show $ transactionId tx
            rollbackStepTx ev block $ SeekTx prevOutputs
        | otherwise -> pure $ SeekConsuming prevOutputs tx
      Done prevOutputs tx blockConsumed consumingTx
        | block < blockConsumed -> do
            addField ev $ LostConsumer $ show consumingTx
            rollbackStepTx ev block $ SeekConsuming prevOutputs tx
        | otherwise -> pure $ Done prevOutputs tx blockConsumed consumingTx

    rollbackPrevOutputs
      :: Chain.BlockHeader
      -> NonEmpty (Chain.BlockHeader, TransactionScriptOutput v)
      -> NonEmpty (Chain.BlockHeader, TransactionScriptOutput v)
    rollbackPrevOutputs block = NE.fromList . dropWhile ((block <) . fst) . NE.toList

data SeekTx v
  = SeekTx (NonEmpty (Chain.BlockHeader, TransactionScriptOutput v))
  | SeekConsuming (NonEmpty (Chain.BlockHeader, TransactionScriptOutput v)) (Transaction v)
  | Done (NonEmpty (Chain.BlockHeader, TransactionScriptOutput v)) (Transaction v) Chain.BlockHeader TxId
