{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Marlowe.Runtime.Web.Server.SyncClient
  where

import Cardano.Api (getTxId)
import Control.Arrow (arr)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, atomically)
import Control.Error (note)
import Control.Monad (mfilter)
import Control.Monad.IO.Class (liftIO)
import Language.Marlowe.Protocol.Query.Client
  (MarloweQueryClient, getContractHeaders, getContractState, getTransaction, getTransactions)
import Language.Marlowe.Protocol.Query.Types (SomeContractState, SomeTransaction(..), SomeTransactions(..))
import qualified Language.Marlowe.Protocol.Query.Types as Query
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (TxId)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion(MarloweV1), MarloweVersionTag(..), Transaction(..))
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Language.Marlowe.Runtime.Transaction.Api (ContractCreated, InputsApplied(InputsApplied, txBody))
import Language.Marlowe.Runtime.Web.Server.TxClient (TempTx(..))
import Language.Marlowe.Runtime.Web.Server.Util (applyRangeToAscList)
import Servant.Pagination

data SyncClientDependencies r = SyncClientDependencies
  { runMarloweQueryClient :: forall a. MarloweQueryClient IO a -> IO a
  , lookupTempContract :: ContractId -> STM (Maybe (TempTx ContractCreated))
  , lookupTempTransaction :: ContractId -> TxId -> STM (Maybe (TempTx InputsApplied))
  , getTempTransactions :: ContractId -> STM [TempTx InputsApplied]
  }

-- | Signature for a delegate that loads a list of contract headers.
type LoadContractHeaders m
   = Maybe ContractId -- ^ ID of the contract to start from.
  -> Int -- ^ Limit: the maximum number of contract headers to load.
  -> Int -- ^ Offset: how many contract headers after the initial one to skip.
  -> RangeOrder -- ^ Whether to load an ascending or descending list.
  -> m (Maybe [Either (TempTx ContractCreated) ContractHeader]) -- ^ Nothing if the initial ID is not found

-- | Signature for a delegate that loads the state of a single contract.
type LoadContract r m
   = ContractId               -- ^ ID of the contract to load
  -> m (Maybe (Either (TempTx ContractCreated) SomeContractState)) -- ^ Nothing if the ID is not found

data LoadTxError
  = ContractNotFound
  | TxNotFound

-- | Signature for a delegate that loads a list of transactions for a contract.
type LoadTransactions r m
   = ContractId -- ^ ID of the contract to load transactions for.
  -> Maybe TxId -- ^ ID of the contract to start from.
  -> Int -- ^ Limit: the maximum number of contract headers to load.
  -> Int -- ^ Offset: how many contract headers after the initial one to skip.
  -> RangeOrder -- ^ Whether to load an ascending or descending list.
  -> m (Either LoadTxError [Either (TempTx InputsApplied) (Transaction 'V1)])

-- | Signature for a delegate that loads a transaction for a contract.
type LoadTransaction r m
   = ContractId -- ^ ID of the contract to load transactions for.
  -> TxId -- ^ ID of the transaction to load.
  -> m (Maybe (Either (TempTx InputsApplied) SomeTransaction))

-- | Public API of the SyncClient
data SyncClient r = SyncClient
  { loadContract :: LoadContract r IO
  , loadTransactions :: LoadTransactions r IO
  , loadTransaction :: LoadTransaction r IO
  , loadContractHeaders :: LoadContractHeaders IO
  }

syncClient :: Component IO (SyncClientDependencies r) (SyncClient r)
syncClient = arr \SyncClientDependencies{..} -> SyncClient
  { loadContractHeaders = \rangeStart rangeLimit rangeOffset rangeOrder -> runMarloweQueryClient do
      let
        rangeDirection = case rangeOrder of
          RangeAsc -> Query.Ascending
          RangeDesc -> Query.Descending
      Query.Page{..} <- getContractHeaders Query.Range{..}
      pure $ pure $ pure <$> items
  , loadContract = \contractId -> runMarloweQueryClient do
      result <- getContractState contractId
      case result of
        Nothing -> liftIO $ atomically $ fmap Left <$> lookupTempContract contractId
        Just contract -> pure $ Just $ Right contract
  , loadTransaction = \contractId txId -> runMarloweQueryClient do
      result <- getTransaction txId
      let matchesContract (SomeTransaction _ _ _ Transaction{contractId=cid}) = cid == contractId
      case mfilter matchesContract result of
        Nothing -> liftIO $ atomically $ fmap Left <$> lookupTempTransaction contractId txId
        Just contract -> pure $ Just $ Right contract
  , loadTransactions = \contractId startFrom limit offset order -> runMarloweQueryClient do
      tempTxs <- liftIO $ atomically $ getTempTransactions contractId
      txs <- getTransactions contractId
      let
        allTxs :: Maybe [Either (TempTx InputsApplied) (Transaction 'V1)]
        allTxs = do
          SomeTransactions MarloweV1 txs' <- txs
          pure $ (Right <$> txs') <> (Left <$> tempTxs)
      pure
        $ note TxNotFound
        $ applyRangeToAscList transactionId' startFrom limit offset order =<< allTxs
  }
  where
    transactionId' (Left (TempTx _ _ InputsApplied{..})) = fromCardanoTxId $ getTxId txBody
    transactionId' (Right Transaction{..}) = transactionId
