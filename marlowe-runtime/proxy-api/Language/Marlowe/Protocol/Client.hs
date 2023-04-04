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
import Network.TypedProtocol (Peer(..), PeerHasAgency(..), PeerRole(..))

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

marloweRuntimeClientPeer :: Monad m => MarloweRuntimeClient m a -> Peer MarloweRuntime 'AsClient 'StInit m a
marloweRuntimeClientPeer = \case
  RunMarloweSyncClient client ->
    Yield (ClientAgency TokInit) MsgRunMarloweSync $ liftMarloweSyncPeer $ marloweSyncClientPeer client
  RunMarloweHeaderSyncClient client ->
    Yield (ClientAgency TokInit) MsgRunMarloweHeaderSync $ liftMarloweHeaderSyncPeer $ marloweHeaderSyncClientPeer client
  RunMarloweQueryClient client ->
    Yield (ClientAgency TokInit) MsgRunMarloweQuery $ liftMarloweQueryPeer $ marloweQueryClientPeer client
  RunTxClient client ->
    Yield (ClientAgency TokInit) MsgRunTxJob $ liftTxJobPeer $ jobClientPeer client

liftTxJobPeer :: Functor m => Peer (Job MarloweTxCommand) 'AsClient st m a -> Peer MarloweRuntime 'AsClient ('StTxJob st) m a
liftTxJobPeer = \case
  Effect m -> Effect $ liftTxJobPeer <$> m
  Done tok a -> Done (TokNobodyTxJob tok) a
  Yield (ClientAgency tok) msg next -> Yield (ClientAgency $ TokClientTxJob tok) (MsgTxJob msg) $ liftTxJobPeer next
  Await (ServerAgency tok) next -> Await (ServerAgency $ TokServerTxJob tok) \(MsgTxJob msg) -> liftTxJobPeer $ next msg

liftMarloweHeaderSyncPeer :: Functor m => Peer MarloweHeaderSync 'AsClient st m a -> Peer MarloweRuntime 'AsClient ('StMarloweHeaderSync st) m a
liftMarloweHeaderSyncPeer = \case
  Effect m -> Effect $ liftMarloweHeaderSyncPeer <$> m
  Done tok a -> Done (TokNobodyMarloweHeaderSync tok) a
  Yield (ClientAgency tok) msg next -> Yield (ClientAgency $ TokClientMarloweHeaderSync tok) (MsgMarloweHeaderSync msg) $ liftMarloweHeaderSyncPeer next
  Await (ServerAgency tok) next -> Await (ServerAgency $ TokServerMarloweHeaderSync tok) \(MsgMarloweHeaderSync msg) -> liftMarloweHeaderSyncPeer $ next msg

liftMarloweSyncPeer :: Functor m => Peer MarloweSync 'AsClient st m a -> Peer MarloweRuntime 'AsClient ('StMarloweSync st) m a
liftMarloweSyncPeer = \case
  Effect m -> Effect $ liftMarloweSyncPeer <$> m
  Done tok a -> Done (TokNobodyMarloweSync tok) a
  Yield (ClientAgency tok) msg next -> Yield (ClientAgency $ TokClientMarloweSync tok) (MsgMarloweSync msg) $ liftMarloweSyncPeer next
  Await (ServerAgency tok) next -> Await (ServerAgency $ TokServerMarloweSync tok) \(MsgMarloweSync msg) -> liftMarloweSyncPeer $ next msg

liftMarloweQueryPeer :: Functor m => Peer MarloweQuery 'AsClient st m a -> Peer MarloweRuntime 'AsClient ('StMarloweQuery st) m a
liftMarloweQueryPeer = \case
  Effect m -> Effect $ liftMarloweQueryPeer <$> m
  Done tok a -> Done (TokNobodyMarloweQuery tok) a
  Yield (ClientAgency tok) msg next -> Yield (ClientAgency $ TokClientMarloweQuery tok) (MsgMarloweQuery msg) $ liftMarloweQueryPeer next
  Await (ServerAgency tok) next -> Await (ServerAgency $ TokServerMarloweQuery tok) \(MsgMarloweQuery msg) -> liftMarloweQueryPeer $ next msg
