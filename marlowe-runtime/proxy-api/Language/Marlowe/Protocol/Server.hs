{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.Server where

import Language.Marlowe.Protocol.Client
import Language.Marlowe.Protocol.HeaderSync.Client (marloweHeaderSyncClientPeer, serveMarloweHeaderSyncClient)
import Language.Marlowe.Protocol.HeaderSync.Server (
  MarloweHeaderSyncServer,
  hoistMarloweHeaderSyncServer,
  marloweHeaderSyncServerPeer,
 )
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import qualified Language.Marlowe.Protocol.HeaderSync.Types as Header
import Language.Marlowe.Protocol.Load.Client (marloweLoadClientPeer, serveMarloweLoadClient)
import Language.Marlowe.Protocol.Load.Server (MarloweLoadServer, hoistMarloweLoadServer, marloweLoadServerPeer)
import Language.Marlowe.Protocol.Load.Types (MarloweLoad)
import qualified Language.Marlowe.Protocol.Load.Types as Load
import Language.Marlowe.Protocol.Query.Server (MarloweQueryServer)
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import Language.Marlowe.Protocol.Sync.Client (marloweSyncClientPeer, serveMarloweSyncClient)
import Language.Marlowe.Protocol.Sync.Server (MarloweSyncServer, hoistMarloweSyncServer, marloweSyncServerPeer)
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import qualified Language.Marlowe.Protocol.Sync.Types as Sync
import Language.Marlowe.Protocol.Transfer.Client (marloweTransferClientPeer, serveMarloweTransferClient)
import Language.Marlowe.Protocol.Transfer.Server (
  MarloweTransferServer (..),
  hoistMarloweTransferServer,
  marloweTransferServerPeer,
 )
import Language.Marlowe.Protocol.Transfer.Types (MarloweTransfer)
import qualified Language.Marlowe.Protocol.Transfer.Types as Transfer
import Language.Marlowe.Protocol.Types
import Language.Marlowe.Runtime.Contract.Api (ContractRequest)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Job.Client (jobClientPeer, serveJobClient)
import Network.Protocol.Job.Server (JobServer, hoistJobServer, jobServerPeer)
import Network.Protocol.Job.Types (Job)
import qualified Network.Protocol.Job.Types as Job
import Network.Protocol.Peer.Trace
import Network.Protocol.Query.Client (queryClientPeer, serveQueryClient)
import Network.Protocol.Query.Server (QueryServer, hoistQueryServer, queryServerPeer)
import Network.Protocol.Query.Types (Query)
import qualified Network.Protocol.Query.Types as Query
import Network.TypedProtocol

data MarloweRuntimeServer m a = MarloweRuntimeServer
  { recvMsgRunMarloweSync :: m (PeerTraced MarloweSync 'AsServer 'Sync.StInit m a)
  , recvMsgRunMarloweHeaderSync :: m (PeerTraced MarloweHeaderSync 'AsServer 'Header.StIdle m a)
  , recvMsgRunMarloweQuery :: m (PeerTraced MarloweQuery 'AsServer 'Query.StReq m a)
  , recvMsgRunMarloweLoad :: m (PeerTraced MarloweLoad 'AsServer ('Load.StProcessing 'Load.RootNode) m a)
  , recvMsgRunMarloweImport :: m (PeerTraced MarloweTransfer 'AsServer 'Transfer.StIdle m a)
  , recvMsgRunTxJob :: m (PeerTraced (Job MarloweTxCommand) 'AsServer 'Job.StInit m a)
  , recvMsgRunContractQuery :: m (PeerTraced (Query ContractRequest) 'AsServer 'Query.StReq m a)
  }
  deriving (Functor)

data MarloweRuntimeServerDirect m a = MarloweRuntimeServerDirect
  { recvMsgRunMarloweSync :: m (MarloweSyncServer m a)
  , recvMsgRunMarloweHeaderSync :: m (MarloweHeaderSyncServer m a)
  , recvMsgRunMarloweQuery :: m (MarloweQueryServer m a)
  , recvMsgRunMarloweLoad :: m (MarloweLoadServer m a)
  , recvMsgRunMarloweImport :: m (MarloweTransferServer m a)
  , recvMsgRunTxJob :: m (JobServer MarloweTxCommand m a)
  , recvMsgRunContractQuery :: m (QueryServer ContractRequest m a)
  }
  deriving (Functor)

hoistMarloweRuntimeServer
  :: (Functor m) => (forall x. m x -> n x) -> MarloweRuntimeServer m a -> MarloweRuntimeServer n a
hoistMarloweRuntimeServer f MarloweRuntimeServer{..} =
  MarloweRuntimeServer
    { recvMsgRunMarloweSync = f $ hoistPeerTraced f <$> recvMsgRunMarloweSync
    , recvMsgRunMarloweHeaderSync = f $ hoistPeerTraced f <$> recvMsgRunMarloweHeaderSync
    , recvMsgRunMarloweQuery = f $ hoistPeerTraced f <$> recvMsgRunMarloweQuery
    , recvMsgRunMarloweLoad = f $ hoistPeerTraced f <$> recvMsgRunMarloweLoad
    , recvMsgRunMarloweImport = f $ hoistPeerTraced f <$> recvMsgRunMarloweImport
    , recvMsgRunTxJob = f $ hoistPeerTraced f <$> recvMsgRunTxJob
    , recvMsgRunContractQuery = f $ hoistPeerTraced f <$> recvMsgRunContractQuery
    , ..
    }

hoistMarloweRuntimeServerDirect
  :: (Functor m) => (forall x. m x -> n x) -> MarloweRuntimeServerDirect m a -> MarloweRuntimeServerDirect n a
hoistMarloweRuntimeServerDirect f MarloweRuntimeServerDirect{..} =
  MarloweRuntimeServerDirect
    { recvMsgRunMarloweSync = f $ hoistMarloweSyncServer f <$> recvMsgRunMarloweSync
    , recvMsgRunMarloweHeaderSync = f $ hoistMarloweHeaderSyncServer f <$> recvMsgRunMarloweHeaderSync
    , recvMsgRunMarloweQuery = f $ hoistQueryServer f <$> recvMsgRunMarloweQuery
    , recvMsgRunMarloweLoad = f $ hoistMarloweLoadServer f <$> recvMsgRunMarloweLoad
    , recvMsgRunMarloweImport = f $ hoistMarloweTransferServer f <$> recvMsgRunMarloweImport
    , recvMsgRunTxJob = f $ hoistJobServer f <$> recvMsgRunTxJob
    , recvMsgRunContractQuery = f $ hoistQueryServer f <$> recvMsgRunContractQuery
    , ..
    }

marloweRuntimeServerPeer :: (Monad m) => MarloweRuntimeServer m a -> PeerTraced MarloweRuntime 'AsServer 'StInit m a
marloweRuntimeServerPeer MarloweRuntimeServer{..} =
  AwaitTraced (ClientAgency TokInit) $
    Receive . EffectTraced . \case
      MsgRunMarloweSync -> liftPeerTraced liftMarloweSync <$> recvMsgRunMarloweSync
      MsgRunMarloweHeaderSync -> liftPeerTraced liftMarloweHeaderSync <$> recvMsgRunMarloweHeaderSync
      MsgRunMarloweQuery -> liftPeerTraced liftMarloweQuery <$> recvMsgRunMarloweQuery
      MsgRunMarloweLoad -> liftPeerTraced liftMarloweLoad <$> recvMsgRunMarloweLoad
      MsgRunMarloweImport -> liftPeerTraced liftMarloweImport <$> recvMsgRunMarloweImport
      MsgRunTxJob -> liftPeerTraced liftTxJob <$> recvMsgRunTxJob
      MsgRunContractQuery -> liftPeerTraced liftContractQuery <$> recvMsgRunContractQuery

marloweRuntimeServerDirectPeer
  :: (Monad m) => MarloweRuntimeServerDirect m a -> PeerTraced MarloweRuntime 'AsServer 'StInit m a
marloweRuntimeServerDirectPeer MarloweRuntimeServerDirect{..} =
  AwaitTraced (ClientAgency TokInit) $
    Receive . EffectTraced . \case
      MsgRunMarloweSync -> liftPeerTraced liftMarloweSync . marloweSyncServerPeer <$> recvMsgRunMarloweSync
      MsgRunMarloweHeaderSync -> liftPeerTraced liftMarloweHeaderSync . marloweHeaderSyncServerPeer <$> recvMsgRunMarloweHeaderSync
      MsgRunMarloweQuery -> liftPeerTraced liftMarloweQuery . queryServerPeer <$> recvMsgRunMarloweQuery
      MsgRunMarloweLoad -> liftPeerTraced liftMarloweLoad . marloweLoadServerPeer <$> recvMsgRunMarloweLoad
      MsgRunMarloweImport -> liftPeerTraced liftMarloweImport . marloweTransferServerPeer <$> recvMsgRunMarloweImport
      MsgRunTxJob -> liftPeerTraced liftTxJob . jobServerPeer <$> recvMsgRunTxJob
      MsgRunContractQuery -> liftPeerTraced liftContractQuery . queryServerPeer <$> recvMsgRunContractQuery

serveMarloweRuntimeClient :: (Monad m) => MarloweRuntimeServer m a -> MarloweRuntimeClient m b -> m (a, b)
serveMarloweRuntimeClient MarloweRuntimeServer{..} = \case
  RunMarloweSyncClient client -> flip connectTraced (marloweSyncClientPeer client) =<< recvMsgRunMarloweSync
  RunMarloweHeaderSyncClient client -> flip connectTraced (marloweHeaderSyncClientPeer client) =<< recvMsgRunMarloweHeaderSync
  RunMarloweQueryClient client -> flip connectTraced (queryClientPeer client) =<< recvMsgRunMarloweQuery
  RunMarloweLoadClient client -> flip connectTraced (marloweLoadClientPeer client) =<< recvMsgRunMarloweLoad
  RunMarloweImportClient client -> flip connectTraced (marloweTransferClientPeer client) =<< recvMsgRunMarloweImport
  RunTxClient client -> flip connectTraced (jobClientPeer client) =<< recvMsgRunTxJob
  RunContractQueryClient client -> flip connectTraced (queryClientPeer client) =<< recvMsgRunContractQuery

serveMarloweRuntimeClientDirect :: (Monad m) => MarloweRuntimeServerDirect m a -> MarloweRuntimeClient m b -> m (a, b)
serveMarloweRuntimeClientDirect MarloweRuntimeServerDirect{..} = \case
  RunMarloweSyncClient client -> flip serveMarloweSyncClient client =<< recvMsgRunMarloweSync
  RunMarloweHeaderSyncClient client -> flip serveMarloweHeaderSyncClient client =<< recvMsgRunMarloweHeaderSync
  RunMarloweQueryClient client -> flip serveQueryClient client =<< recvMsgRunMarloweQuery
  RunMarloweLoadClient client -> flip serveMarloweLoadClient client =<< recvMsgRunMarloweLoad
  RunMarloweImportClient client -> flip serveMarloweTransferClient client =<< recvMsgRunMarloweImport
  RunTxClient client -> flip serveJobClient client =<< recvMsgRunTxJob
  RunContractQueryClient client -> flip serveQueryClient client =<< recvMsgRunContractQuery

connectTraced
  :: forall ps st m a b
   . (Protocol ps, Monad m)
  => PeerTraced ps 'AsServer st m a
  -> PeerTraced ps 'AsClient st m b
  -> m (a, b)
connectTraced s c = do
  (a, b, _) <- connect (peerTracedToPeer s) (peerTracedToPeer c)
  pure (a, b)
