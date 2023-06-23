{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Proxy where

import Control.Arrow (arr)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes (Probes(..))
import Control.Monad.Event.Class (MonadEvent, localBackend)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource.Internal (ResourceT(..))
import Data.Proxy (Proxy(..))
import Language.Marlowe.Protocol.HeaderSync.Server (MarloweHeaderSyncServer, hoistMarloweHeaderSyncServer)
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import qualified Language.Marlowe.Protocol.HeaderSync.Types as Header
import Language.Marlowe.Protocol.Load.Server (MarloweLoadServer, hoistMarloweLoadServer)
import Language.Marlowe.Protocol.Load.Types (MarloweLoad, SomePeerHasAgency(..))
import qualified Language.Marlowe.Protocol.Load.Types as Load
import Language.Marlowe.Protocol.Query.Server (MarloweQueryServer)
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import Language.Marlowe.Protocol.Server
  (MarloweRuntimeServer(..), MarloweRuntimeServerDirect(..), hoistMarloweRuntimeServer, hoistMarloweRuntimeServerDirect)
import Language.Marlowe.Protocol.Sync.Server (MarloweSyncServer, hoistMarloweSyncServer)
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import qualified Language.Marlowe.Protocol.Sync.Types as Sync
import Language.Marlowe.Runtime.Contract.Api (ContractRequest)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Channel.Typed
import Network.Protocol.Connection (ServerSource(..))
import Network.Protocol.Handshake.Types (Handshake)
import qualified Network.Protocol.Handshake.Types as Handshake
import Network.Protocol.Job.Server (JobServer, hoistJobServer)
import Network.Protocol.Job.Types (Job)
import qualified Network.Protocol.Job.Types as Job
import Network.Protocol.Peer.Trace
import Network.Protocol.Query.Server (QueryServer, hoistQueryServer)
import Network.Protocol.Query.Types (Query)
import qualified Network.Protocol.Query.Types as Query
import Network.TypedProtocol
import Observe.Event.Backend (setAncestorEventBackend)
import UnliftIO (MonadUnliftIO(..))

data MarloweProxy m = MarloweProxy
  { proxyServerSource :: Bool -> ServerSource MarloweRuntimeServer m ()
  , probes :: Probes
  }

data MarloweProxyInProc m = MarloweProxyInProc
  { proxyServerSource :: ServerSource MarloweRuntimeServerDirect m ()
  , probes :: Probes
  }

proxyInProc :: MonadUnliftIO m => Component m (RouterInProc m) (MarloweProxyInProc m)
proxyInProc = arr \router -> MarloweProxyInProc
  { proxyServerSource = ServerSource $ ResourceT \releaseMap ->
      pure $ hoistMarloweRuntimeServerDirect (\(ResourceT m) -> m releaseMap) $ serverInProc router
  , probes = Probes
    { startup = pure True
    , liveness = pure True
    , readiness = pure True
    }
  }

proxy :: (MonadUnliftIO m, MonadEvent r s m, MonadFail m) => Component m (Router r (ResourceT m)) (MarloweProxy m)
proxy = arr \router -> MarloweProxy
  { proxyServerSource = \traced -> ServerSource $ ResourceT \releaseMap ->
      pure $ hoistMarloweRuntimeServer (\(ResourceT m) -> m releaseMap) $ server (not traced) router
  , probes = Probes
    { startup = pure True
    , liveness = pure True
    , readiness = pure True
    }
  }

data Router r m = Router
  { connectMarloweSync :: m (Channel (Handshake MarloweSync) 'AsClient ('Handshake.StInit 'Sync.StInit) m, r)
  , connectMarloweHeaderSync :: m (Channel (Handshake MarloweHeaderSync) 'AsClient ('Handshake.StInit 'Header.StIdle) m, r)
  , connectMarloweQuery :: m (Channel (Handshake MarloweQuery) 'AsClient ('Handshake.StInit 'Query.StReq) m, r)
  , connectMarloweLoad :: m (Channel (Handshake MarloweLoad) 'AsClient ('Handshake.StInit ('Load.StProcessing 'Load.RootNode)) m, r)
  , connectTxJob :: m (Channel (Handshake (Job MarloweTxCommand)) 'AsClient ('Handshake.StInit 'Job.StInit) m, r)
  , connectContractQuery :: m (Channel (Handshake (Query ContractRequest)) 'AsClient ('Handshake.StInit 'Query.StReq) m, r)
  }

data RouterInProc m = RouterInProc
  { marloweSyncServerSource :: ServerSource MarloweSyncServer m ()
  , marloweHeaderSyncServerSource :: ServerSource MarloweHeaderSyncServer m ()
  , marloweQueryServerSource :: ServerSource MarloweQueryServer m ()
  , marloweLoadServerSource :: ServerSource MarloweLoadServer m ()
  , txJobServerSource :: ServerSource (JobServer MarloweTxCommand) m ()
  , contractQueryServerSource :: ServerSource (QueryServer ContractRequest) m ()
  }

server
  :: (MonadFail m, MonadEvent r s m)
  => Bool
  -> Router r m
  -> MarloweRuntimeServer m ()
server useOpenRefAsParent Router{..} = MarloweRuntimeServer
  { recvMsgRunMarloweSync = withHandshake useOpenRefAsParent (ClientAgency Sync.TokInit) connectMarloweSync marloweSyncNext
  , recvMsgRunMarloweHeaderSync = withHandshake useOpenRefAsParent (ClientAgency Header.TokIdle) connectMarloweHeaderSync marloweHeaderSyncNext
  , recvMsgRunMarloweQuery = withHandshake useOpenRefAsParent (ClientAgency Query.TokReq) connectMarloweQuery queryNext
  , recvMsgRunMarloweLoad = withHandshake useOpenRefAsParent (ServerAgency $ Load.TokProcessing Load.SRootNode) connectMarloweLoad loadNext
  , recvMsgRunTxJob = withHandshake useOpenRefAsParent (ClientAgency Job.TokInit) connectTxJob jobNext
  , recvMsgRunContractQuery = withHandshake useOpenRefAsParent (ClientAgency Query.TokReq) connectContractQuery queryNext
  }

serverInProc
  :: Monad m
  => RouterInProc m
  -> MarloweRuntimeServerDirect (ResourceT m) ()
serverInProc RouterInProc{..} = MarloweRuntimeServerDirect
  { recvMsgRunMarloweSync = useServerSource hoistMarloweSyncServer marloweSyncServerSource
  , recvMsgRunMarloweHeaderSync = useServerSource hoistMarloweHeaderSyncServer marloweHeaderSyncServerSource
  , recvMsgRunMarloweQuery = useServerSource hoistQueryServer marloweQueryServerSource
  , recvMsgRunMarloweLoad = useServerSource hoistMarloweLoadServer marloweLoadServerSource
  , recvMsgRunTxJob = useServerSource hoistJobServer txJobServerSource
  , recvMsgRunContractQuery = useServerSource hoistQueryServer contractQueryServerSource
  }

useServerSource
  :: Monad m
  => (forall p q a. Functor p => (forall x. p x -> q x) -> server p a -> server q a)
  -> ServerSource server m ()
  -> ResourceT m (server (ResourceT m) ())
useServerSource hoistServer socket = do
  s <- getServer socket
  pure $ hoistServer lift s

marloweSyncNext :: PeerHasAgency pr st -> Message MarloweSync st st' -> NextAgency MarloweSync pr st st'
marloweSyncNext = \case
  ClientAgency Sync.TokInit -> \case
    Sync.MsgFollowContract _ -> NextCall $ ServerAgency Sync.TokFollow
    Sync.MsgIntersect _ v _ -> NextCall $ ServerAgency $ Sync.TokIntersect v
  ClientAgency (Sync.TokIdle v) -> \case
    Sync.MsgRequestNext -> NextCall $ ServerAgency $ Sync.TokNext v
    Sync.MsgDone -> NextClose Sync.TokDone
  ClientAgency (Sync.TokWait v) -> \case
    Sync.MsgPoll -> NextCall $ ServerAgency $ Sync.TokNext v
    Sync.MsgCancel -> NextCast $ ClientAgency $ Sync.TokIdle v
  ServerAgency Sync.TokFollow -> \case
    Sync.MsgContractNotFound -> NextClosed Sync.TokDone
    Sync.MsgContractFound _ v _ -> NextReceive $ ClientAgency $ Sync.TokIdle v
  ServerAgency (Sync.TokIntersect v) -> \case
    Sync.MsgIntersectNotFound -> NextClosed Sync.TokDone
    Sync.MsgIntersectFound{} -> NextReceive $ ClientAgency $ Sync.TokIdle v
  ServerAgency (Sync.TokNext v) -> \case
    Sync.MsgRollBackCreation -> NextClosed Sync.TokDone
    Sync.MsgRollForward{} -> NextReceive $ ClientAgency $ Sync.TokIdle v
    Sync.MsgRollBackward{} -> NextReceive $ ClientAgency $ Sync.TokIdle v
    Sync.MsgWait{} -> NextReceive $ ClientAgency $ Sync.TokWait v

marloweHeaderSyncNext :: PeerHasAgency pr st -> Message MarloweHeaderSync st st' -> NextAgency MarloweHeaderSync pr st st'
marloweHeaderSyncNext =   \case
  ClientAgency _ -> \case
    Header.MsgIntersect _ -> NextCall $ ServerAgency Header.TokIntersect
    Header.MsgRequestNext -> NextCall $ ServerAgency Header.TokNext
    Header.MsgDone -> NextClose Header.TokDone
    Header.MsgPoll -> NextCall $ ServerAgency Header.TokNext
    Header.MsgCancel -> NextCast $ ClientAgency Header.TokIdle
  ServerAgency _ -> \case
    Header.MsgIntersectNotFound -> NextReceive $ ClientAgency Header.TokIdle
    Header.MsgIntersectFound{} -> NextReceive $ ClientAgency Header.TokIdle
    Header.MsgNewHeaders{} -> NextReceive $ ClientAgency Header.TokIdle
    Header.MsgRollBackward{} -> NextReceive $ ClientAgency Header.TokIdle
    Header.MsgWait{} -> NextReceive $ ClientAgency Header.TokWait

queryNext :: Query.Request req => PeerHasAgency pr st -> Message (Query req) st st' -> NextAgency (Query req) pr st st'
queryNext =   \case
  ClientAgency _ -> \case
    Query.MsgRequest req -> NextCall $ ServerAgency $ Query.TokRes $ Query.tagFromReq req
    Query.MsgDone -> NextClose Query.TokDone
  ServerAgency _ -> \case
    Query.MsgRespond _ -> NextReceive $ ClientAgency Query.TokReq

loadNext :: PeerHasAgency pr st -> Message MarloweLoad st st' -> NextAgency MarloweLoad pr st st'
loadNext =   \case
  ClientAgency (Load.TokCanPush Zero node)  -> \case
    Load.MsgRequestResume -> NextCall $ ServerAgency $ Load.TokProcessing node
    Load.MsgAbort -> NextClose Load.TokDone
  ClientAgency (Load.TokCanPush (Succ n) node)  -> \case
    Load.MsgPushClose -> case Load.sPop n node of
      SomePeerHasAgency (ClientAgency tok) -> NextCast $ ClientAgency tok
      SomePeerHasAgency (ServerAgency tok) -> NextCall $ ServerAgency tok
    Load.MsgPushPay{} -> NextCast $ ClientAgency $ Load.TokCanPush n $ Load.SPayNode node
    Load.MsgPushIf{} -> NextCast $ ClientAgency $ Load.TokCanPush n $ Load.SIfLNode node
    Load.MsgPushWhen{} -> NextCast $ ClientAgency $ Load.TokCanPush n $ Load.SWhenNode node
    Load.MsgPushCase{} -> case node of
      Load.SWhenNode node' -> NextCast $ ClientAgency $ Load.TokCanPush n $ Load.SCaseNode node'
    Load.MsgPushLet{} -> NextCast $ ClientAgency $ Load.TokCanPush n $ Load.SLetNode node
    Load.MsgPushAssert{} -> NextCast $ ClientAgency $ Load.TokCanPush n $ Load.SAssertNode node
    Load.MsgAbort -> NextClose Load.TokDone
  ServerAgency (Load.TokProcessing node) -> \case
    Load.MsgResume n -> NextReceive $ ClientAgency $ Load.TokCanPush n node
  ServerAgency Load.TokComplete -> \case
    Load.MsgComplete{} -> NextClosed Load.TokDone

jobNext :: Job.Command cmd => PeerHasAgency pr st -> Message (Job cmd) st st' -> NextAgency (Job cmd) pr st st'
jobNext =   \case
  ClientAgency Job.TokInit -> \case
    Job.MsgExec cmd -> NextCall $ ServerAgency $ Job.TokCmd $ Job.tagFromCommand cmd
    Job.MsgAttach jobId -> NextCall $ ServerAgency $ Job.TokAttach $ Job.tagFromJobId jobId
  ClientAgency (Job.TokAwait tag) -> \case
    Job.MsgPoll -> NextCall $ ServerAgency $ Job.TokCmd tag
    Job.MsgDetach -> NextClose Job.TokDone
  ServerAgency (Job.TokCmd tag) -> \case
    Job.MsgFail _ -> NextClosed Job.TokDone
    Job.MsgSucceed _ -> NextClosed Job.TokDone
    Job.MsgAwait _ _ -> NextReceive $ ClientAgency $ Job.TokAwait tag
  ServerAgency (Job.TokAttach tag) -> \case
    Job.MsgAttached -> NextReceive $ ServerAgency $ Job.TokCmd tag
    Job.MsgAttachFailed -> NextClosed Job.TokDone

data NextAgency ps pr (st :: ps) (st' :: ps) where
  NextCall :: PeerHasAgency 'AsServer st' -> NextAgency ps 'AsClient st st'
  NextCast :: PeerHasAgency pr st' -> NextAgency ps 'AsClient st st'
  NextClose :: NobodyHasAgency st' -> NextAgency ps 'AsClient st st'
  NextRespond :: PeerHasAgency 'AsClient st' -> NextAgency ps 'AsServer st st'
  NextReceive :: PeerHasAgency pr st' -> NextAgency ps 'AsServer st st'
  NextClosed :: NobodyHasAgency st' -> NextAgency ps 'AsServer st st'


withHandshake
  :: forall ps pr st r s m
   . (Handshake.HasSignature ps, MonadFail m, MonadEvent r s m)
  => Bool
  -> PeerHasAgency pr st
  -> m (Channel (Handshake ps) 'AsClient ('Handshake.StInit st) m, r)
  -> (forall pr' st' st''. PeerHasAgency pr' st' -> Message ps st' st'' -> NextAgency ps pr' st' st'')
  -> m (PeerTraced ps 'AsServer st m ())
withHandshake useOpenRefAsParent agency openConnection nextAgency = do
  (Channel{..}, openRef) <- openConnection
  let
    runMain
      | useOpenRefAsParent = fmap (hoistPeerTraced (localBackend (setAncestorEventBackend openRef))) . localBackend (setAncestorEventBackend openRef)
      | otherwise = id
  runMain do
    let OutboundChannel{..} = yield (ClientAgency Handshake.TokInit) $ Handshake.MsgHandshake $ Handshake.signature $ Proxy @ps
    ResponseChannel msg' coPeer <- call $ ServerAgency Handshake.TokHandshake
    case msg' of
      Handshake.MsgAccept -> pure $ proxyProtocol agency (lowerChannel liftHandshake coPeer) nextAgency
      Handshake.MsgReject-> fail "Handshake rejected by server"

liftHandshake :: LiftProtocol ps (Handshake ps) 'Handshake.StLift
liftHandshake =
  LiftProtocol Handshake.TokLiftClient Handshake.TokLiftServer Handshake.TokLiftNobody Handshake.MsgLift \case
    Handshake.MsgLift msg -> SomeSubMessage msg

proxyProtocol
  :: Monad m
  => PeerHasAgency pr st
  -> Channel ps 'AsClient st m
  -> (forall pr' st' st''. PeerHasAgency pr' st' -> Message ps st' st'' -> NextAgency ps pr' st' st'')
  -> PeerTraced ps 'AsServer st m ()
proxyProtocol agency Channel{..} nextAgency = case agency of
  ClientAgency _ -> AwaitTraced agency \msg ->
    let
      OutboundChannel{..} = yield agency msg
    in case nextAgency agency msg of
      NextCall tok' -> Respond tok' do
        ResponseChannel msg' coPeer <- call tok'
        pure $ Response msg' case toAgency $ nextAgency tok' msg' of
          Left tok'' -> DoneTraced tok'' ()
          Right (SomePeerHasAgency tok'') -> proxyProtocol tok'' coPeer nextAgency
      NextCast tok' -> Receive $ EffectTraced do
        coPeer <- cast
        pure $ proxyProtocol tok' coPeer nextAgency
      NextClose tok' -> Closed tok' $ close tok'
  ServerAgency _ -> EffectTraced do
    InboundChannel{..} <- await agency
    YieldTraced agency message <$> case nextAgency agency message of
      NextRespond tok' -> respond tok' Handler
        { withSendResponse = \sendResponse -> pure $ Call tok' \msg -> EffectTraced do
            coPeer <- sendResponse msg
            pure case toAgency $ nextAgency tok' msg of
              Left tok'' -> DoneTraced tok'' ()
              Right (SomePeerHasAgency tok'') -> proxyProtocol tok'' coPeer nextAgency
        }
      NextReceive tok' -> do
        coPeer <- receive
        pure $ Cast $ proxyProtocol tok' coPeer nextAgency
      NextClosed tok' -> do
        closed tok'
        pure $ Close tok' ()

toAgency :: NextAgency ps pr st st' -> Either (NobodyHasAgency st') (SomePeerHasAgency st')
toAgency = \case
  NextCall tok -> Right $ SomePeerHasAgency tok
  NextCast tok -> Right $ SomePeerHasAgency tok
  NextClose tok -> Left tok
  NextRespond tok -> Right $ SomePeerHasAgency tok
  NextReceive tok -> Right $ SomePeerHasAgency tok
  NextClosed tok -> Left tok
