{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.Client where

import Language.Marlowe.Protocol.BulkSync.Client (
  MarloweBulkSyncClient,
  hoistMarloweBulkSyncClient,
  marloweBulkSyncClientPeer,
 )
import Language.Marlowe.Protocol.BulkSync.Types (MarloweBulkSync)
import Language.Marlowe.Protocol.HeaderSync.Client (
  MarloweHeaderSyncClient,
  hoistMarloweHeaderSyncClient,
  marloweHeaderSyncClientPeer,
 )
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import Language.Marlowe.Protocol.Load.Client (MarloweLoadClient, hoistMarloweLoadClient, marloweLoadClientPeer)
import Language.Marlowe.Protocol.Load.Types (MarloweLoad)
import Language.Marlowe.Protocol.Query.Client (MarloweQueryClient)
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient, hoistMarloweSyncClient, marloweSyncClientPeer)
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import Language.Marlowe.Protocol.Transfer.Client (
  MarloweTransferClient (..),
  hoistMarloweTransferClient,
  marloweTransferClientPeer,
 )
import Language.Marlowe.Protocol.Transfer.Types (MarloweTransfer)
import Language.Marlowe.Protocol.Types
import Language.Marlowe.Runtime.Contract.Api (ContractRequest)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Job.Client (JobClient, hoistJobClient, jobClientPeer)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Peer.Trace
import Network.Protocol.Query.Client (QueryClient, hoistQueryClient, queryClientPeer)
import Network.Protocol.Query.Types (Query)
import Network.TypedProtocol (PeerHasAgency (..), PeerRole (..))

data MarloweRuntimeClient m a
  = RunMarloweSyncClient (MarloweSyncClient m a)
  | RunMarloweHeaderSyncClient (MarloweHeaderSyncClient m a)
  | RunMarloweBulkSyncClient (MarloweBulkSyncClient m a)
  | RunMarloweQueryClient (MarloweQueryClient m a)
  | RunMarloweLoadClient (MarloweLoadClient m a)
  | RunMarloweTransferClient (MarloweTransferClient m a)
  | RunTxClient (JobClient MarloweTxCommand m a)
  | RunContractQueryClient (QueryClient ContractRequest m a)
  deriving (Functor)

hoistMarloweRuntimeClient
  :: (Functor m) => (forall x. m x -> n x) -> MarloweRuntimeClient m a -> MarloweRuntimeClient n a
hoistMarloweRuntimeClient f = \case
  RunMarloweSyncClient client -> RunMarloweSyncClient $ hoistMarloweSyncClient f client
  RunMarloweHeaderSyncClient client -> RunMarloweHeaderSyncClient $ hoistMarloweHeaderSyncClient f client
  RunMarloweBulkSyncClient client -> RunMarloweBulkSyncClient $ hoistMarloweBulkSyncClient f client
  RunMarloweQueryClient client -> RunMarloweQueryClient $ hoistQueryClient f client
  RunMarloweLoadClient client -> RunMarloweLoadClient $ hoistMarloweLoadClient f client
  RunMarloweTransferClient client -> RunMarloweTransferClient $ hoistMarloweTransferClient f client
  RunTxClient client -> RunTxClient $ hoistJobClient f client
  RunContractQueryClient client -> RunContractQueryClient $ hoistQueryClient f client

marloweRuntimeClientPeer
  :: (Monad m)
  => MarloweRuntimeClient m a
  -> PeerTraced MarloweRuntime 'AsClient 'StInit m a
marloweRuntimeClientPeer = \case
  RunMarloweSyncClient client ->
    YieldTraced (ClientAgency TokInit) MsgRunMarloweSync $
      Cast $
        liftPeerTraced liftMarloweSync $
          marloweSyncClientPeer client
  RunMarloweHeaderSyncClient client ->
    YieldTraced (ClientAgency TokInit) MsgRunMarloweHeaderSync $
      Cast $
        liftPeerTraced liftMarloweHeaderSync $
          marloweHeaderSyncClientPeer client
  RunMarloweBulkSyncClient client ->
    YieldTraced (ClientAgency TokInit) MsgRunMarloweBulkSync $
      Cast $
        liftPeerTraced liftMarloweBulkSync $
          marloweBulkSyncClientPeer client
  RunMarloweQueryClient client ->
    YieldTraced (ClientAgency TokInit) MsgRunMarloweQuery $ Cast $ liftPeerTraced liftMarloweQuery $ queryClientPeer client
  RunMarloweLoadClient client ->
    YieldTraced (ClientAgency TokInit) MsgRunMarloweLoad $
      Cast $
        liftPeerTraced liftMarloweLoad $
          marloweLoadClientPeer client
  RunMarloweTransferClient client ->
    YieldTraced (ClientAgency TokInit) MsgRunMarloweTransfer $
      Cast $
        liftPeerTraced liftMarloweTransfer $
          marloweTransferClientPeer client
  RunTxClient client -> YieldTraced (ClientAgency TokInit) MsgRunTxJob $ Cast $ liftPeerTraced liftTxJob $ jobClientPeer client
  RunContractQueryClient client ->
    YieldTraced (ClientAgency TokInit) MsgRunContractQuery $
      Cast $
        liftPeerTraced liftContractQuery $
          queryClientPeer client

liftMarloweSync :: LiftProtocol MarloweSync MarloweRuntime 'StMarloweSync
liftMarloweSync =
  LiftProtocol TokClientMarloweSync TokServerMarloweSync TokNobodyMarloweSync MsgMarloweSync \(MsgMarloweSync msg) -> SomeSubMessage msg

liftMarloweHeaderSync :: LiftProtocol MarloweHeaderSync MarloweRuntime 'StMarloweHeaderSync
liftMarloweHeaderSync =
  LiftProtocol TokClientMarloweHeaderSync TokServerMarloweHeaderSync TokNobodyMarloweHeaderSync MsgMarloweHeaderSync \(MsgMarloweHeaderSync msg) -> SomeSubMessage msg

liftMarloweBulkSync :: LiftProtocol MarloweBulkSync MarloweRuntime 'StMarloweBulkSync
liftMarloweBulkSync =
  LiftProtocol TokClientMarloweBulkSync TokServerMarloweBulkSync TokNobodyMarloweBulkSync MsgMarloweBulkSync \(MsgMarloweBulkSync msg) -> SomeSubMessage msg

liftMarloweQuery :: LiftProtocol MarloweQuery MarloweRuntime 'StMarloweQuery
liftMarloweQuery =
  LiftProtocol TokClientMarloweQuery TokServerMarloweQuery TokNobodyMarloweQuery MsgMarloweQuery \(MsgMarloweQuery msg) -> SomeSubMessage msg

liftMarloweLoad :: LiftProtocol MarloweLoad MarloweRuntime 'StMarloweLoad
liftMarloweLoad =
  LiftProtocol TokClientMarloweLoad TokServerMarloweLoad TokNobodyMarloweLoad MsgMarloweLoad \(MsgMarloweLoad msg) -> SomeSubMessage msg

liftMarloweTransfer :: LiftProtocol MarloweTransfer MarloweRuntime 'StMarloweTransfer
liftMarloweTransfer =
  LiftProtocol TokClientMarloweTransfer TokServerMarloweTransfer TokNobodyMarloweTransfer MsgMarloweTransfer \(MsgMarloweTransfer msg) -> SomeSubMessage msg

liftTxJob :: LiftProtocol (Job MarloweTxCommand) MarloweRuntime 'StTxJob
liftTxJob =
  LiftProtocol TokClientTxJob TokServerTxJob TokNobodyTxJob MsgTxJob \(MsgTxJob msg) -> SomeSubMessage msg

liftContractQuery :: LiftProtocol (Query ContractRequest) MarloweRuntime 'StContractQuery
liftContractQuery =
  LiftProtocol TokClientContractQuery TokServerContractQuery TokNobodyContractQuery MsgContractQuery \(MsgContractQuery msg) -> SomeSubMessage msg
