{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Web.Server.SyncClient where

import Control.Arrow (arr)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, atomically)
import Control.Error (note)
import Control.Monad (guard, mfilter)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.List (uncons)
import Language.Marlowe.Protocol.Client (MarloweRuntimeClient (..))
import Language.Marlowe.Protocol.Query.Client (
  getContractHeaders,
  getContractState,
  getTransaction,
  getTransactions,
  getWithdrawal,
  getWithdrawals,
 )
import Language.Marlowe.Protocol.Query.Types (
  ContractFilter,
  SomeContractState,
  SomeTransaction (..),
  SomeTransactions (..),
  Withdrawal,
  WithdrawalFilter,
 )
import qualified Language.Marlowe.Protocol.Query.Types as Query
import Language.Marlowe.Runtime.ChainSync.Api (TxId)
import Language.Marlowe.Runtime.Core.Api (
  ContractId,
  MarloweVersion (MarloweV1),
  MarloweVersionTag (..),
  Transaction (..),
 )
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Language.Marlowe.Runtime.Transaction.Api (ContractCreatedInEra, InputsAppliedInEra (..), WithdrawTxInEra (..))
import Language.Marlowe.Runtime.Web.Server.TxClient (TempTx (..))
import Language.Marlowe.Runtime.Web.Server.Util (applyRangeToAscList)
import Network.Protocol.Connection (Connector, runConnector)
import Servant.Pagination

data SyncClientDependencies m = SyncClientDependencies
  { connector :: Connector MarloweRuntimeClient m
  , lookupTempContract :: ContractId -> STM (Maybe (TempTx ContractCreatedInEra))
  , lookupTempTransaction :: ContractId -> TxId -> STM (Maybe (TempTx InputsAppliedInEra))
  , lookupTempWithdrawal :: TxId -> STM (Maybe (TempTx WithdrawTxInEra))
  }

-- | Signature for a delegate that loads a list of contract headers.
type LoadContractHeaders m =
  ContractFilter
  -> Query.Range ContractId
  -> m (Maybe (Query.Page ContractId ContractHeader))
  -- ^ Nothing if the initial ID is not found

-- | Signature for a delegate that loads a list of contract headers.
type LoadWithdrawals m =
  WithdrawalFilter
  -> Query.Range TxId
  -> m (Maybe (Query.Page TxId Withdrawal))
  -- ^ Nothing if the initial ID is not found

-- | Signature for a delegate that loads the state of a single contract.
type LoadContract m =
  ContractId
  -- ^ ID of the contract to load
  -> m (Maybe (Either (TempTx ContractCreatedInEra) SomeContractState))
  -- ^ Nothing if the ID is not found

-- | Signature for a delegate that loads the state of a single contract.
type LoadWithdrawal m =
  TxId
  -- ^ ID of the contract to load
  -> m (Maybe (Either (TempTx WithdrawTxInEra) Withdrawal))
  -- ^ Nothing if the ID is not found

data LoadTxError
  = ContractNotFound
  | TxNotFound

-- | Signature for a delegate that loads a list of transactions for a contract.
type LoadTransactions m =
  ContractId
  -- ^ ID of the contract to load transactions for.
  -> Query.Range TxId
  -- ^ The range of transactions to load.
  -> m (Either LoadTxError (Query.Page TxId (Transaction 'V1)))

-- | Signature for a delegate that loads a transaction for a contract.
type LoadTransaction m =
  ContractId
  -- ^ ID of the contract to load transactions for.
  -> TxId
  -- ^ ID of the transaction to load.
  -> m (Maybe (Either (TempTx InputsAppliedInEra) SomeTransaction))

-- | Public API of the SyncClient
data SyncClient m = SyncClient
  { loadContract :: LoadContract m
  , loadTransactions :: LoadTransactions m
  , loadTransaction :: LoadTransaction m
  , loadContractHeaders :: LoadContractHeaders m
  , loadWithdrawals :: LoadWithdrawals m
  , loadWithdrawal :: LoadWithdrawal m
  }

syncClient :: (MonadUnliftIO m) => Component m (SyncClientDependencies m) (SyncClient m)
syncClient = arr \SyncClientDependencies{..} ->
  SyncClient
    { loadContractHeaders = \cFilter -> runConnector connector . RunMarloweQueryClient . getContractHeaders cFilter
    , loadContract = \contractId -> runConnector connector $ RunMarloweQueryClient do
        result <- getContractState contractId
        case result of
          Nothing -> liftIO $ atomically $ fmap Left <$> lookupTempContract contractId
          Just contract -> pure $ Just $ Right contract
    , loadTransaction = \contractId txId -> runConnector connector $ RunMarloweQueryClient do
        result <- getTransaction txId
        let matchesContract (SomeTransaction _ _ _ Transaction{contractId = cid}) = cid == contractId
        case mfilter matchesContract result of
          Nothing -> liftIO $ atomically $ fmap Left <$> lookupTempTransaction contractId txId
          Just contract -> pure $ Just $ Right contract
    , loadTransactions = \contractId Query.Range{..} -> runConnector connector $ RunMarloweQueryClient do
        mTxs <- getTransactions contractId
        pure do
          SomeTransactions MarloweV1 txs <- note ContractNotFound mTxs
          let totalCount = length txs
          let direction = case rangeDirection of
                Query.Ascending -> RangeAsc
                Query.Descending -> RangeDesc
          items <- note TxNotFound $ applyRangeToAscList transactionId rangeStart rangeLimit rangeOffset direction txs
          pure
            Query.Page
              { items
              , nextRange = do
                  guard $ length items == rangeLimit
                  (Transaction{transactionId}, _) <- uncons $ reverse items
                  pure $
                    Query.Range
                      { rangeStart = Just transactionId
                      , ..
                      }
              , totalCount
              }
    , loadWithdrawals = fmap (runConnector connector . RunMarloweQueryClient) . getWithdrawals
    , loadWithdrawal = \txId -> runConnector connector $ RunMarloweQueryClient do
        result <- getWithdrawal txId
        case result of
          Nothing -> liftIO $ atomically $ fmap Left <$> lookupTempWithdrawal txId
          Just contract -> pure $ Just $ Right contract
    }
