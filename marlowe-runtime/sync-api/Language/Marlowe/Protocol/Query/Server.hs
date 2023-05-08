{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Protocol.Query.Server
  where

import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.ChainSync.Api (TxId)
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Network.Protocol.Peer.Trace
import Network.TypedProtocol
import UnliftIO (MonadUnliftIO, concurrently)

type MarloweQueryServer = PeerTraced MarloweQuery 'AsServer 'StReq

marloweQueryServer
  :: forall m
   . MonadUnliftIO m
  => (ContractFilter -> Range ContractId -> m (Maybe (Page ContractId ContractHeader)))
  -> (ContractId -> m (Maybe SomeContractState))
  -> (TxId -> m (Maybe SomeTransaction))
  -> (ContractId -> m (Maybe SomeTransactions))
  -> (TxId -> m (Maybe Withdrawal))
  -> (WithdrawalFilter -> Range TxId -> m (Maybe (Page TxId Withdrawal)))
  -> MarloweQueryServer m ()
marloweQueryServer getContractHeaders getContractState getTransaction getTransactions getWithdrawal getWithdrawals = go
  where
    go = AwaitTraced (ClientAgency TokReq) \case
      MsgRequest req -> Respond (ServerAgency $ TokRes $ requestToSt req) do
        result <- serviceRequest req
        pure $ Response (MsgRespond result) go
      MsgDone -> Closed TokDone $ pure ()
    serviceRequest :: Request a -> m a
    serviceRequest = \case
      ReqContractHeaders cFilter range -> getContractHeaders cFilter range
      ReqContractState contractId -> getContractState contractId
      ReqTransaction txId -> getTransaction txId
      ReqTransactions contractId -> getTransactions contractId
      ReqWithdrawal txId -> getWithdrawal txId
      ReqWithdrawals wFilter range -> getWithdrawals wFilter range
      ReqBoth a b -> concurrently (serviceRequest a) (serviceRequest b)
