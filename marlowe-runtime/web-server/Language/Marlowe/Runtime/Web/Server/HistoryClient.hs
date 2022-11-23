{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Marlowe.Runtime.Web.Server.HistoryClient
  where

import Control.Arrow (arr)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, atomically)
import Control.Error (note)
import Data.List (sortOn)
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import Data.Void (Void)
import Language.Marlowe.Protocol.Sync.Client
import Language.Marlowe.Runtime.ChainSync.Api (TxId)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api
  (ContractId, MarloweVersion, Transaction(..), TransactionOutput(..), TransactionScriptOutput)
import Language.Marlowe.Runtime.History.Api (ContractStep(..), CreateStep(..))
import Language.Marlowe.Runtime.Transaction.Api (ContractCreated)
import Language.Marlowe.Runtime.Web.Server.DTO (ContractRecord(..), SomeTransaction(..))
import Language.Marlowe.Runtime.Web.Server.TxClient (TempTx)
import Language.Marlowe.Runtime.Web.Server.Util (applyRangeToAscList)
import Observe.Event (EventBackend, addField, withEvent)
import Observe.Event.BackendModification (EventBackendModifiers, modifyEventBackend)
import Observe.Event.DSL (SelectorSpec(..))
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
  ]

data HistoryClientDependencies r = HistoryClientDependencies
  { runMarloweSyncClient :: forall a. MarloweSyncClient IO a -> IO a
  , lookupTempContract :: ContractId -> STM (Maybe (TempTx ContractCreated))
  , eventBackend :: EventBackend IO r HistoryClientSelector
  }

-- | Signature for a delegate that loads the state of a single contract.
type LoadContract r m
   = forall r'
   . EventBackendModifiers r r'
  -> ContractId               -- ^ ID of the contract to load
  -> m (Maybe (Either (TempTx ContractCreated) ContractRecord)) -- ^ Nothing if the ID is not found

data LoadContractHeadersError
  = ContractNotFound
  | InitialTransactionNotFound

-- | Signature for a delegate that loads a list of transactions for a contract.
type LoadTransactions r m
   = forall r'
   . EventBackendModifiers r r'
  -> ContractId -- ^ ID of the contract to load transactions for.
  -> Maybe TxId -- ^ ID of the contract to start from.
  -> Int -- ^ Limit: the maximum number of contract headers to load.
  -> Int -- ^ Offset: how many contract headers after the initial one to skip.
  -> RangeOrder -- ^ Whether to load an ascending or descending list.
  -> m (Either LoadContractHeadersError [SomeTransaction]) -- ^ Nothing if the initial ID is not found

-- | Public API of the HistoryClient
data HistoryClient r = HistoryClient
  { loadContract :: LoadContract r IO -- ^ Load contract headers from the indexer.
  , loadTransactions :: LoadTransactions r IO -- ^ Load transactions for a contract from the indexer.
  }

historyClient :: Component IO (HistoryClientDependencies r) (HistoryClient r)
historyClient = arr \HistoryClientDependencies{..} -> HistoryClient
  { loadContract = \mods contractId -> do
      result <- runMarloweSyncClient $ loadContractClient (modifyEventBackend mods eventBackend) contractId
      case result of
        Nothing -> atomically $ fmap Left <$> lookupTempContract contractId
        Just contract -> pure $ Just $ Right contract
  , loadTransactions = \mods contractId startFrom limit offset order ->
      (note InitialTransactionNotFound . applyRangeToAscList transactionId' startFrom limit offset order =<<)
        <$> runMarloweSyncClient (loadTransactionsClient (modifyEventBackend mods eventBackend) contractId)
  }
  where
    transactionId' (SomeTransaction _ Transaction{..}) = transactionId

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
  -> MarloweSyncClient IO (Either LoadContractHeadersError [SomeTransaction])
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
      -> ClientStIdle v IO (Either LoadContractHeadersError [SomeTransaction])
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
