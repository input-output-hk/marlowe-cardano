{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.Client
  where

import Data.Functor ((<&>))
import Language.Marlowe.Protocol.HeaderSync.Client
  ( MarloweHeaderSyncClient
  , hoistMarloweHeaderSyncClient
  , marloweHeaderSyncClientPeer
  , marloweHeaderSyncClientPeerTraced
  )
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import Language.Marlowe.Protocol.Query.Client
  (MarloweQueryClient, hoistMarloweQueryClient, marloweQueryClientPeer, marloweQueryClientPeerTraced)
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import Language.Marlowe.Protocol.Sync.Client
  (MarloweSyncClient, hoistMarloweSyncClient, marloweSyncClientPeer, marloweSyncClientPeerTraced)
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import Language.Marlowe.Protocol.Types
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Job.Client (JobClient, hoistJobClient, jobClientPeer, jobClientPeerTraced)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Peer.Trace
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

marloweRuntimeClientPeerTraced :: Monad m => MarloweRuntimeClient m a -> PeerTraced MarloweRuntime 'AsClient 'StInit r m a
marloweRuntimeClientPeerTraced = \case
  RunMarloweSyncClient client ->
    YieldTraced (ClientAgency TokInit) MsgRunMarloweSync $ Cast $ liftMarloweSyncPeerTraced $ marloweSyncClientPeerTraced client
  RunMarloweHeaderSyncClient client ->
    YieldTraced (ClientAgency TokInit) MsgRunMarloweHeaderSync $ Cast $ liftMarloweHeaderSyncPeerTraced $ marloweHeaderSyncClientPeerTraced client
  RunMarloweQueryClient client ->
    YieldTraced (ClientAgency TokInit) MsgRunMarloweQuery $ Cast $ liftMarloweQueryPeerTraced $ marloweQueryClientPeerTraced client
  RunTxClient client ->
    YieldTraced (ClientAgency TokInit) MsgRunTxJob $ Cast $ liftTxJobPeerTraced $ jobClientPeerTraced client

liftTxJobPeerTraced :: Functor m => PeerTraced (Job MarloweTxCommand) 'AsClient st r m a -> PeerTraced MarloweRuntime 'AsClient ('StTxJob st) r m a
liftTxJobPeerTraced = \case
  EffectTraced m -> EffectTraced $ liftTxJobPeerTraced <$> m
  DoneTraced tok a -> DoneTraced (TokNobodyTxJob tok) a
  YieldTraced (ClientAgency tok) msg yield -> YieldTraced (ClientAgency $ TokClientTxJob tok) (MsgTxJob msg) case yield of
    Call (ServerAgency tok') next -> Call (ServerAgency $ TokServerTxJob tok') \(MsgTxJob msg') -> liftTxJobPeerTraced $ next msg'
    Cast next -> Cast $ liftTxJobPeerTraced next
    Close tok' a -> Close (TokNobodyTxJob tok') a
  AwaitTraced (ServerAgency tok) k -> AwaitTraced (ServerAgency $ TokServerTxJob tok) \(MsgTxJob msg) -> case k msg of
    Respond (ClientAgency tok') next -> Respond (ClientAgency $ TokClientTxJob tok') $ next <&> \case
      Response msg' next' -> Response (MsgTxJob msg') $ liftTxJobPeerTraced next'
    Receive next -> Receive $ liftTxJobPeerTraced next
    Closed tok' ma -> Closed (TokNobodyTxJob tok') ma

liftMarloweHeaderSyncPeerTraced :: Functor m => PeerTraced MarloweHeaderSync 'AsClient st r m a -> PeerTraced MarloweRuntime 'AsClient ('StMarloweHeaderSync st) r m a
liftMarloweHeaderSyncPeerTraced = \case
  EffectTraced m -> EffectTraced $ liftMarloweHeaderSyncPeerTraced <$> m
  DoneTraced tok a -> DoneTraced (TokNobodyMarloweHeaderSync tok) a
  YieldTraced (ClientAgency tok) msg yield -> YieldTraced (ClientAgency $ TokClientMarloweHeaderSync tok) (MsgMarloweHeaderSync msg) case yield of
    Call (ServerAgency tok') next -> Call (ServerAgency $ TokServerMarloweHeaderSync tok') \(MsgMarloweHeaderSync msg') -> liftMarloweHeaderSyncPeerTraced $ next msg'
    Cast next -> Cast $ liftMarloweHeaderSyncPeerTraced next
    Close tok' a -> Close (TokNobodyMarloweHeaderSync tok') a
  AwaitTraced (ServerAgency tok) k -> AwaitTraced (ServerAgency $ TokServerMarloweHeaderSync tok) \(MsgMarloweHeaderSync msg) -> case k msg of
    Respond (ClientAgency tok') next -> Respond (ClientAgency $ TokClientMarloweHeaderSync tok') $ next <&> \case
      Response msg' next' -> Response (MsgMarloweHeaderSync msg') $ liftMarloweHeaderSyncPeerTraced next'
    Receive next -> Receive $ liftMarloweHeaderSyncPeerTraced next
    Closed tok' ma -> Closed (TokNobodyMarloweHeaderSync tok') ma

liftMarloweSyncPeerTraced :: Functor m => PeerTraced MarloweSync 'AsClient st r m a -> PeerTraced MarloweRuntime 'AsClient ('StMarloweSync st) r m a
liftMarloweSyncPeerTraced = \case
  EffectTraced m -> EffectTraced $ liftMarloweSyncPeerTraced <$> m
  DoneTraced tok a -> DoneTraced (TokNobodyMarloweSync tok) a
  YieldTraced (ClientAgency tok) msg yield -> YieldTraced (ClientAgency $ TokClientMarloweSync tok) (MsgMarloweSync msg) case yield of
    Call (ServerAgency tok') next -> Call (ServerAgency $ TokServerMarloweSync tok') \(MsgMarloweSync msg') -> liftMarloweSyncPeerTraced $ next msg'
    Cast next -> Cast $ liftMarloweSyncPeerTraced next
    Close tok' a -> Close (TokNobodyMarloweSync tok') a
  AwaitTraced (ServerAgency tok) k -> AwaitTraced (ServerAgency $ TokServerMarloweSync tok) \(MsgMarloweSync msg) -> case k msg of
    Respond (ClientAgency tok') next -> Respond (ClientAgency $ TokClientMarloweSync tok') $ next <&> \case
      Response msg' next' -> Response (MsgMarloweSync msg') $ liftMarloweSyncPeerTraced next'
    Receive next -> Receive $ liftMarloweSyncPeerTraced next
    Closed tok' ma -> Closed (TokNobodyMarloweSync tok') ma

liftMarloweQueryPeerTraced :: Functor m => PeerTraced MarloweQuery 'AsClient st r m a -> PeerTraced MarloweRuntime 'AsClient ('StMarloweQuery st) r m a
liftMarloweQueryPeerTraced = \case
  EffectTraced m -> EffectTraced $ liftMarloweQueryPeerTraced <$> m
  DoneTraced tok a -> DoneTraced (TokNobodyMarloweQuery tok) a
  YieldTraced (ClientAgency tok) msg yield -> YieldTraced (ClientAgency $ TokClientMarloweQuery tok) (MsgMarloweQuery msg) case yield of
    Call (ServerAgency tok') next -> Call (ServerAgency $ TokServerMarloweQuery tok') \(MsgMarloweQuery msg') -> liftMarloweQueryPeerTraced $ next msg'
    Cast next -> Cast $ liftMarloweQueryPeerTraced next
    Close tok' a -> Close (TokNobodyMarloweQuery tok') a
  AwaitTraced (ServerAgency tok) k -> AwaitTraced (ServerAgency $ TokServerMarloweQuery tok) \(MsgMarloweQuery msg) -> case k msg of
    Respond (ClientAgency tok') next -> Respond (ClientAgency $ TokClientMarloweQuery tok') $ next <&> \case
      Response msg' next' -> Response (MsgMarloweQuery msg') $ liftMarloweQueryPeerTraced next'
    Receive next -> Receive $ liftMarloweQueryPeerTraced next
    Closed tok' ma -> Closed (TokNobodyMarloweQuery tok') ma
