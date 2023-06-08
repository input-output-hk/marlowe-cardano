{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.Server where

import Control.Monad (join)
import Data.Void (absurd)
import Language.Marlowe.Protocol.Client
import Language.Marlowe.Protocol.HeaderSync.Client (marloweHeaderSyncClientPeer)
import Language.Marlowe.Protocol.HeaderSync.Server
  (MarloweHeaderSyncServer, hoistMarloweHeaderSyncServer, marloweHeaderSyncServerPeer)
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import qualified Language.Marlowe.Protocol.HeaderSync.Types as Header
import Language.Marlowe.Protocol.Load.Client (marloweLoadClientPeer)
import Language.Marlowe.Protocol.Load.Server (MarloweLoadServer, hoistMarloweLoadServer, marloweLoadServerPeer)
import Language.Marlowe.Protocol.Load.Types (MarloweLoad)
import qualified Language.Marlowe.Protocol.Load.Types as Load
import Language.Marlowe.Protocol.Query.Server (MarloweQueryServer)
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import Language.Marlowe.Protocol.Sync.Client (marloweSyncClientPeer)
import Language.Marlowe.Protocol.Sync.Server (MarloweSyncServer, hoistMarloweSyncServer, marloweSyncServerPeer)
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import qualified Language.Marlowe.Protocol.Sync.Types as Sync
import Language.Marlowe.Protocol.Types
import Language.Marlowe.Runtime.Contract.Api (ContractRequest)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Job.Client (jobClientPeer)
import Network.Protocol.Job.Server (JobServer, hoistJobServer, jobServerPeer)
import Network.Protocol.Job.Types (Job)
import qualified Network.Protocol.Job.Types as Job
import Network.Protocol.Peer.Trace
import Network.Protocol.Query.Client (queryClientPeer)
import Network.Protocol.Query.Server (QueryServer, hoistQueryServer, queryServerPeer)
import Network.Protocol.Query.Types (Query)
import qualified Network.Protocol.Query.Types as Query
import Network.TypedProtocol

data MarloweRuntimeServer m a = MarloweRuntimeServer
  { recvMsgRunMarloweSync :: m (PeerTraced MarloweSync 'AsServer 'Sync.StInit m a)
  , recvMsgRunMarloweHeaderSync :: m (PeerTraced MarloweHeaderSync 'AsServer 'Header.StIdle m a)
  , recvMsgRunMarloweQuery :: m (PeerTraced MarloweQuery 'AsServer 'Query.StReq m a)
  , recvMsgRunMarloweLoad :: m (PeerTraced MarloweLoad 'AsServer ('Load.StProcessing 'Load.RootNode) m a)
  , recvMsgRunTxJob :: m (PeerTraced (Job MarloweTxCommand) 'AsServer 'Job.StInit m a)
  , recvMsgRunContractQuery :: m (PeerTraced (Query ContractRequest) 'AsServer 'Query.StReq m a)
  } deriving Functor

data MarloweRuntimeServerDirect m a = MarloweRuntimeServerDirect
  { recvMsgRunMarloweSync :: m (MarloweSyncServer m a)
  , recvMsgRunMarloweHeaderSync :: m (MarloweHeaderSyncServer m a)
  , recvMsgRunMarloweQuery :: m (MarloweQueryServer m a)
  , recvMsgRunMarloweLoad :: m (MarloweLoadServer m a)
  , recvMsgRunTxJob :: m (JobServer MarloweTxCommand m a)
  , recvMsgRunContractQuery :: m (QueryServer ContractRequest m a)
  } deriving Functor

hoistMarloweRuntimeServer :: Functor m => (forall x. m x -> n x) -> MarloweRuntimeServer m a -> MarloweRuntimeServer n a
hoistMarloweRuntimeServer f MarloweRuntimeServer{..} = MarloweRuntimeServer
  { recvMsgRunMarloweSync = f $ hoistPeerTraced f <$> recvMsgRunMarloweSync
  , recvMsgRunMarloweHeaderSync = f $ hoistPeerTraced f <$> recvMsgRunMarloweHeaderSync
  , recvMsgRunMarloweQuery = f $ hoistPeerTraced f <$> recvMsgRunMarloweQuery
  , recvMsgRunMarloweLoad = f $ hoistPeerTraced f <$> recvMsgRunMarloweLoad
  , recvMsgRunTxJob = f $ hoistPeerTraced f <$> recvMsgRunTxJob
  , recvMsgRunContractQuery = f $ hoistPeerTraced f <$> recvMsgRunContractQuery
  , ..
  }

hoistMarloweRuntimeServerDirect :: Functor m => (forall x. m x -> n x) -> MarloweRuntimeServerDirect m a -> MarloweRuntimeServerDirect n a
hoistMarloweRuntimeServerDirect f MarloweRuntimeServerDirect{..} = MarloweRuntimeServerDirect
  { recvMsgRunMarloweSync = f $ hoistMarloweSyncServer f <$> recvMsgRunMarloweSync
  , recvMsgRunMarloweHeaderSync = f $ hoistMarloweHeaderSyncServer f <$> recvMsgRunMarloweHeaderSync
  , recvMsgRunMarloweQuery = f $ hoistQueryServer f <$> recvMsgRunMarloweQuery
  , recvMsgRunMarloweLoad = f $ hoistMarloweLoadServer f <$> recvMsgRunMarloweLoad
  , recvMsgRunTxJob = f $ hoistJobServer f <$> recvMsgRunTxJob
  , recvMsgRunContractQuery = f $ hoistQueryServer f <$> recvMsgRunContractQuery
  , ..
  }

marloweRuntimeServerPeer :: Monad m => MarloweRuntimeServer m a -> PeerTraced MarloweRuntime 'AsServer 'StInit m a
marloweRuntimeServerPeer MarloweRuntimeServer{..} =
  AwaitTraced (ClientAgency TokInit) $ Receive . EffectTraced . \case
    MsgRunMarloweSync -> liftPeerTraced liftMarloweSync <$> recvMsgRunMarloweSync
    MsgRunMarloweHeaderSync -> liftPeerTraced liftMarloweHeaderSync <$> recvMsgRunMarloweHeaderSync
    MsgRunMarloweQuery -> liftPeerTraced liftMarloweQuery <$> recvMsgRunMarloweQuery
    MsgRunMarloweLoad -> liftPeerTraced liftMarloweLoad <$> recvMsgRunMarloweLoad
    MsgRunTxJob -> liftPeerTraced liftTxJob <$> recvMsgRunTxJob
    MsgRunContractQuery -> liftPeerTraced liftContractQuery <$> recvMsgRunContractQuery

marloweRuntimeServerDirectPeer :: Monad m => MarloweRuntimeServerDirect m a -> PeerTraced MarloweRuntime 'AsServer 'StInit m a
marloweRuntimeServerDirectPeer MarloweRuntimeServerDirect{..} =
  AwaitTraced (ClientAgency TokInit) $ Receive . EffectTraced . \case
    MsgRunMarloweSync -> liftPeerTraced liftMarloweSync . marloweSyncServerPeer <$> recvMsgRunMarloweSync
    MsgRunMarloweHeaderSync -> liftPeerTraced liftMarloweHeaderSync . marloweHeaderSyncServerPeer <$> recvMsgRunMarloweHeaderSync
    MsgRunMarloweQuery -> liftPeerTraced liftMarloweQuery . queryServerPeer <$> recvMsgRunMarloweQuery
    MsgRunMarloweLoad -> liftPeerTraced liftMarloweLoad . marloweLoadServerPeer <$> recvMsgRunMarloweLoad
    MsgRunTxJob -> liftPeerTraced liftTxJob . jobServerPeer <$> recvMsgRunTxJob
    MsgRunContractQuery -> liftPeerTraced liftContractQuery . queryServerPeer <$> recvMsgRunContractQuery

serveMarloweRuntimeClient :: Monad m => MarloweRuntimeServer m a -> MarloweRuntimeClient m b -> m (a, b)
serveMarloweRuntimeClient MarloweRuntimeServer{..} = \case
  RunMarloweSyncClient client -> flip serveClient (marloweSyncClientPeer client) =<< recvMsgRunMarloweSync
  RunMarloweHeaderSyncClient client -> flip serveClient (marloweHeaderSyncClientPeer client) =<< recvMsgRunMarloweHeaderSync
  RunMarloweQueryClient client -> flip serveClient (queryClientPeer client) =<< recvMsgRunMarloweQuery
  RunMarloweLoadClient client -> flip serveClient (marloweLoadClientPeer client) =<< recvMsgRunMarloweLoad
  RunTxClient client -> flip serveClient (jobClientPeer client) =<< recvMsgRunTxJob
  RunContractQueryClient client -> flip serveClient (queryClientPeer client) =<< recvMsgRunContractQuery

serveClient
  :: forall ps st m a b
   . (Protocol ps, Monad m)
  => PeerTraced ps 'AsServer st m a
  -> PeerTraced ps 'AsClient st m b
  -> m (a, b)
serveClient s c = go (peerTracedToPeer s) (peerTracedToPeer c)
  where
    go :: Peer ps 'AsServer st' m a -> Peer ps 'AsClient st' m b -> m (a, b)
    go (Effect ms) (Effect mc) = join $ go <$> ms <*> mc
    go (Effect ms) peerC = flip go peerC =<< ms
    go peerS (Effect mc) = go peerS =<< mc
    go (Done _ a) (Done _ b) = pure (a, b)
    go (Yield _ msg peerS) (Await _ k) = go peerS $ k msg
    go (Await _ k) (Yield _ msg peerC) = go (k msg) peerC
    go (Done tokS _) (Yield (ClientAgency tokC) _ _) = absurd $ exclusionLemma_NobodyAndClientHaveAgency tokS tokC
    go (Done tokS _) (Await (ServerAgency tokC) _) = absurd $ exclusionLemma_NobodyAndServerHaveAgency tokS tokC
    go (Yield (ServerAgency tokS) _ _) (Done tokC _) = absurd $ exclusionLemma_NobodyAndServerHaveAgency tokC tokS
    go (Await (ClientAgency tokS) _) (Done tokC _) = absurd $ exclusionLemma_NobodyAndClientHaveAgency tokC tokS
    go (Yield (ServerAgency tokS) _ _) (Yield (ClientAgency tokC) _ _) = absurd $ exclusionLemma_ClientAndServerHaveAgency tokC tokS
    go (Await (ClientAgency tokS) _) (Await (ServerAgency tokC) _) = absurd $ exclusionLemma_ClientAndServerHaveAgency tokS tokC
