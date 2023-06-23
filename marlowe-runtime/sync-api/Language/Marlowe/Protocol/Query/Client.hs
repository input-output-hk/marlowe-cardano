{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Marlowe.Protocol.Query.Client where

import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.ChainSync.Api (TxId)
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Network.Protocol.Query.Client

type MarloweQueryClient = QueryClient MarloweSyncRequest

getWithdrawal :: Applicative m => TxId -> MarloweQueryClient m (Maybe Withdrawal)
getWithdrawal = request . ReqWithdrawal

getWithdrawals :: Applicative m => WithdrawalFilter -> Range TxId -> MarloweQueryClient m (Maybe (Page TxId Withdrawal))
getWithdrawals = fmap request . ReqWithdrawals

getContractHeaders :: Applicative m => ContractFilter -> Range ContractId -> MarloweQueryClient m (Maybe (Page ContractId ContractHeader))
getContractHeaders = fmap request . ReqContractHeaders

getContractState :: Applicative m => ContractId -> MarloweQueryClient m (Maybe SomeContractState)
getContractState = request . ReqContractState

getTransactions :: Applicative m => ContractId -> MarloweQueryClient m (Maybe SomeTransactions)
getTransactions = request . ReqTransactions

getTransaction :: Applicative m => TxId -> MarloweQueryClient m (Maybe SomeTransaction)
getTransaction = request . ReqTransaction

getStatus :: Applicative m => MarloweQueryClient m RuntimeStatus
getStatus = request ReqStatus
