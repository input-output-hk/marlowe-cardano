{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.Client
  where

import Data.Functor ((<&>))
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

marloweRuntimeClientPeer :: Monad m => MarloweRuntimeClient m a -> PeerTraced MarloweRuntime 'AsClient 'StInit m a
marloweRuntimeClientPeer = \case
  RunMarloweSyncClient client ->
    YieldTraced (ClientAgency TokInit) MsgRunMarloweSync $ Cast $ liftMarloweSyncPeer $ marloweSyncClientPeer client
  RunMarloweHeaderSyncClient client ->
    YieldTraced (ClientAgency TokInit) MsgRunMarloweHeaderSync $ Cast $ liftMarloweHeaderSyncPeer $ marloweHeaderSyncClientPeer client
  RunMarloweQueryClient client ->
    YieldTraced (ClientAgency TokInit) MsgRunMarloweQuery $ Cast $ liftMarloweQueryPeer $ marloweQueryClientPeer client
  RunTxClient client ->
    YieldTraced (ClientAgency TokInit) MsgRunTxJob $ Cast $ liftTxJobPeer $ jobClientPeer client

liftTxJobPeer :: Functor m => PeerTraced (Job MarloweTxCommand) 'AsClient st m a -> PeerTraced MarloweRuntime 'AsClient ('StTxJob st) m a
liftTxJobPeer = \case
  EffectTraced m -> EffectTraced $ liftTxJobPeer <$> m
  DoneTraced tok a -> DoneTraced (TokNobodyTxJob tok) a
  YieldTraced (ClientAgency tok) msg yield -> YieldTraced (ClientAgency $ TokClientTxJob tok) (MsgTxJob msg) case yield of
    Call (ServerAgency tok') next -> Call (ServerAgency $ TokServerTxJob tok') \(MsgTxJob msg') -> liftTxJobPeer $ next msg'
    Cast next -> Cast $ liftTxJobPeer next
    Close tok' a -> Close (TokNobodyTxJob tok') a
  AwaitTraced (ServerAgency tok) k -> AwaitTraced (ServerAgency $ TokServerTxJob tok) \(MsgTxJob msg) -> case k msg of
    Respond (ClientAgency tok') next -> Respond (ClientAgency $ TokClientTxJob tok') $ next <&> \case
      Response msg' next' -> Response (MsgTxJob msg') $ liftTxJobPeer next'
    Receive next -> Receive $ liftTxJobPeer next
    Closed tok' ma -> Closed (TokNobodyTxJob tok') ma

liftMarloweHeaderSyncPeer :: Functor m => PeerTraced MarloweHeaderSync 'AsClient st m a -> PeerTraced MarloweRuntime 'AsClient ('StMarloweHeaderSync st) m a
liftMarloweHeaderSyncPeer = \case
  EffectTraced m -> EffectTraced $ liftMarloweHeaderSyncPeer <$> m
  DoneTraced tok a -> DoneTraced (TokNobodyMarloweHeaderSync tok) a
  YieldTraced (ClientAgency tok) msg yield -> YieldTraced (ClientAgency $ TokClientMarloweHeaderSync tok) (MsgMarloweHeaderSync msg) case yield of
    Call (ServerAgency tok') next -> Call (ServerAgency $ TokServerMarloweHeaderSync tok') \(MsgMarloweHeaderSync msg') -> liftMarloweHeaderSyncPeer $ next msg'
    Cast next -> Cast $ liftMarloweHeaderSyncPeer next
    Close tok' a -> Close (TokNobodyMarloweHeaderSync tok') a
  AwaitTraced (ServerAgency tok) k -> AwaitTraced (ServerAgency $ TokServerMarloweHeaderSync tok) \(MsgMarloweHeaderSync msg) -> case k msg of
    Respond (ClientAgency tok') next -> Respond (ClientAgency $ TokClientMarloweHeaderSync tok') $ next <&> \case
      Response msg' next' -> Response (MsgMarloweHeaderSync msg') $ liftMarloweHeaderSyncPeer next'
    Receive next -> Receive $ liftMarloweHeaderSyncPeer next
    Closed tok' ma -> Closed (TokNobodyMarloweHeaderSync tok') ma

liftMarloweSyncPeer :: Functor m => PeerTraced MarloweSync 'AsClient st m a -> PeerTraced MarloweRuntime 'AsClient ('StMarloweSync st) m a
liftMarloweSyncPeer = \case
  EffectTraced m -> EffectTraced $ liftMarloweSyncPeer <$> m
  DoneTraced tok a -> DoneTraced (TokNobodyMarloweSync tok) a
  YieldTraced (ClientAgency tok) msg yield -> YieldTraced (ClientAgency $ TokClientMarloweSync tok) (MsgMarloweSync msg) case yield of
    Call (ServerAgency tok') next -> Call (ServerAgency $ TokServerMarloweSync tok') \(MsgMarloweSync msg') -> liftMarloweSyncPeer $ next msg'
    Cast next -> Cast $ liftMarloweSyncPeer next
    Close tok' a -> Close (TokNobodyMarloweSync tok') a
  AwaitTraced (ServerAgency tok) k -> AwaitTraced (ServerAgency $ TokServerMarloweSync tok) \(MsgMarloweSync msg) -> case k msg of
    Respond (ClientAgency tok') next -> Respond (ClientAgency $ TokClientMarloweSync tok') $ next <&> \case
      Response msg' next' -> Response (MsgMarloweSync msg') $ liftMarloweSyncPeer next'
    Receive next -> Receive $ liftMarloweSyncPeer next
    Closed tok' ma -> Closed (TokNobodyMarloweSync tok') ma

liftMarloweQueryPeer :: Functor m => PeerTraced MarloweQuery 'AsClient st m a -> PeerTraced MarloweRuntime 'AsClient ('StMarloweQuery st) m a
liftMarloweQueryPeer = \case
  EffectTraced m -> EffectTraced $ liftMarloweQueryPeer <$> m
  DoneTraced tok a -> DoneTraced (TokNobodyMarloweQuery tok) a
  YieldTraced (ClientAgency tok) msg yield -> YieldTraced (ClientAgency $ TokClientMarloweQuery tok) (MsgMarloweQuery msg) case yield of
    Call (ServerAgency tok') next -> Call (ServerAgency $ TokServerMarloweQuery tok') \(MsgMarloweQuery msg') -> liftMarloweQueryPeer $ next msg'
    Cast next -> Cast $ liftMarloweQueryPeer next
    Close tok' a -> Close (TokNobodyMarloweQuery tok') a
  AwaitTraced (ServerAgency tok) k -> AwaitTraced (ServerAgency $ TokServerMarloweQuery tok) \(MsgMarloweQuery msg) -> case k msg of
    Respond (ClientAgency tok') next -> Respond (ClientAgency $ TokClientMarloweQuery tok') $ next <&> \case
      Response msg' next' -> Response (MsgMarloweQuery msg') $ liftMarloweQueryPeer next'
    Receive next -> Receive $ liftMarloweQueryPeer next
    Closed tok' ma -> Closed (TokNobodyMarloweQuery tok') ma
