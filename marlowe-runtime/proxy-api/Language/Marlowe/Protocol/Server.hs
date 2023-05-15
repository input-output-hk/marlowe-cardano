{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.Server
  where

import Language.Marlowe.Protocol.Client
  (liftMarloweHeaderSync, liftMarloweLoad, liftMarloweQuery, liftMarloweSync, liftTxJob)
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import qualified Language.Marlowe.Protocol.HeaderSync.Types as Header
import Language.Marlowe.Protocol.Load.Types (MarloweLoad)
import qualified Language.Marlowe.Protocol.Load.Types as Load
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import qualified Language.Marlowe.Protocol.Query.Types as Query
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import qualified Language.Marlowe.Protocol.Sync.Types as Sync
import Language.Marlowe.Protocol.Types
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Job.Types (Job)
import qualified Network.Protocol.Job.Types as Job
import Network.Protocol.Peer.Trace
import Network.TypedProtocol

data MarloweRuntimeServer m a = MarloweRuntimeServer
  { recvMsgRunMarloweSync :: m (PeerTraced MarloweSync 'AsServer 'Sync.StInit m a)
  , recvMsgRunMarloweHeaderSync :: m (PeerTraced MarloweHeaderSync 'AsServer 'Header.StIdle m a)
  , recvMsgRunMarloweQuery :: m (PeerTraced MarloweQuery 'AsServer 'Query.StReq m a)
  , recvMsgRunMarloweLoad :: m (PeerTraced MarloweLoad 'AsServer ('Load.StProcessing 'Load.RootNode) m a)
  , recvMsgRunTxJob :: m (PeerTraced (Job MarloweTxCommand) 'AsServer 'Job.StInit m a)
  } deriving Functor

hoistMarloweRuntimeServer :: Functor m => (forall x. m x -> n x) -> MarloweRuntimeServer m a -> MarloweRuntimeServer n a
hoistMarloweRuntimeServer f MarloweRuntimeServer{..} = MarloweRuntimeServer
  { recvMsgRunMarloweSync = f $ hoistPeerTraced f <$> recvMsgRunMarloweSync
  , recvMsgRunMarloweHeaderSync = f $ hoistPeerTraced f <$> recvMsgRunMarloweHeaderSync
  , recvMsgRunMarloweQuery = f $ hoistPeerTraced f <$> recvMsgRunMarloweQuery
  , recvMsgRunMarloweLoad = f $ hoistPeerTraced f <$> recvMsgRunMarloweLoad
  , recvMsgRunTxJob = f $ hoistPeerTraced f <$> recvMsgRunTxJob
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
