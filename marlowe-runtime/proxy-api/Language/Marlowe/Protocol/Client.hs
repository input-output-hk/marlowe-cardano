{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.Client
  where

import Language.Marlowe.Protocol.HeaderSync.Client
  (MarloweHeaderSyncClient, hoistMarloweHeaderSyncClient, marloweHeaderSyncClientPeer)
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import Language.Marlowe.Protocol.Query.Client (MarloweQueryClient, hoistMarloweQueryClient, marloweQueryClientPeer)
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient, hoistMarloweSyncClient, marloweSyncClientPeer)
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import Language.Marlowe.Protocol.Types
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Job.Client (JobClient, hoistJobClient, jobClientPeer)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Peer.Trace
import Network.TypedProtocol (PeerHasAgency(..), PeerRole(..))

data MarloweRuntimeClient m a
  = RunMarloweSyncClient (MarloweSyncClient m a)
  | RunMarloweHeaderSyncClient (MarloweHeaderSyncClient m a)
  | RunMarloweQueryClient (MarloweQueryClient m a)
  | RunTxClient (JobClient MarloweTxCommand m a)
  deriving Functor

hoistMarloweRuntimeClient :: Functor m => (forall x. m x -> n x) -> MarloweRuntimeClient m a -> MarloweRuntimeClient n a
hoistMarloweRuntimeClient f = \case
  RunMarloweSyncClient client -> RunMarloweSyncClient $ hoistMarloweSyncClient f client
  RunMarloweHeaderSyncClient client -> RunMarloweHeaderSyncClient $ hoistMarloweHeaderSyncClient f client
  RunMarloweQueryClient client -> RunMarloweQueryClient $ hoistMarloweQueryClient f client
  RunTxClient client -> RunTxClient $ hoistJobClient f client

marloweRuntimeClientPeer
  :: Monad m
  => MarloweRuntimeClient m a
  -> PeerTraced MarloweRuntime 'AsClient 'StInit m a
marloweRuntimeClientPeer = \case
  RunMarloweSyncClient client -> YieldTraced (ClientAgency TokInit) MsgRunMarloweSync $ Cast $ liftPeerTraced liftMarloweSync $ marloweSyncClientPeer client
  RunMarloweHeaderSyncClient client -> YieldTraced (ClientAgency TokInit) MsgRunMarloweHeaderSync $ Cast $ liftPeerTraced liftMarloweHeaderSync $ marloweHeaderSyncClientPeer client
  RunMarloweQueryClient client -> YieldTraced (ClientAgency TokInit) MsgRunMarloweQuery $ Cast $ liftPeerTraced liftMarloweQuery $ marloweQueryClientPeer client
  RunTxClient client -> YieldTraced (ClientAgency TokInit) MsgRunTxJob $ Cast $ liftPeerTraced liftTxJob $ jobClientPeer client

liftMarloweSync :: LiftProtocol MarloweSync MarloweRuntime 'StMarloweSync
liftMarloweSync =
  LiftProtocol TokClientMarloweSync TokServerMarloweSync TokNobodyMarloweSync MsgMarloweSync \(MsgMarloweSync msg) -> SomeSubMessage msg

liftMarloweHeaderSync :: LiftProtocol MarloweHeaderSync MarloweRuntime 'StMarloweHeaderSync
liftMarloweHeaderSync =
  LiftProtocol TokClientMarloweHeaderSync TokServerMarloweHeaderSync TokNobodyMarloweHeaderSync MsgMarloweHeaderSync \(MsgMarloweHeaderSync msg) -> SomeSubMessage msg

liftMarloweQuery :: LiftProtocol MarloweQuery MarloweRuntime 'StMarloweQuery
liftMarloweQuery =
  LiftProtocol TokClientMarloweQuery TokServerMarloweQuery TokNobodyMarloweQuery MsgMarloweQuery \(MsgMarloweQuery msg) -> SomeSubMessage msg

liftTxJob :: LiftProtocol (Job MarloweTxCommand) MarloweRuntime 'StTxJob
liftTxJob =
  LiftProtocol TokClientTxJob TokServerTxJob TokNobodyTxJob MsgTxJob \(MsgTxJob msg) -> SomeSubMessage msg
