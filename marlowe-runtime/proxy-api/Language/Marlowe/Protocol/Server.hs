{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.Server
  where

import Control.Monad.Event.Class (MonadEvent, emitImmediateInjectEvent_, withInjectEvent, withInjectEventFields)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Void (Void)
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import qualified Language.Marlowe.Protocol.HeaderSync.Types as Header
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import qualified Language.Marlowe.Protocol.Query.Types as Query
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import qualified Language.Marlowe.Protocol.Sync.Types as Sync
import Language.Marlowe.Protocol.Types
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Handshake.Types (Handshake, signature)
import qualified Network.Protocol.Handshake.Types as Handshake
import Network.Protocol.Job.Types (Job)
import qualified Network.Protocol.Job.Types as Job
import Network.Protocol.Peer.Trace
import Network.TypedProtocol
import Network.TypedProtocol.Codec (AnyMessageAndAgency(..))
import Observe.Event (Event(reference), InjectSelector)

data MarloweRuntimeServer r m a = forall dState. MarloweRuntimeServer
  { getMarloweSyncDriver :: m (DriverTraced (Handshake MarloweSync) dState r m)
  , getMarloweHeaderSyncDriver :: m (DriverTraced (Handshake MarloweHeaderSync) dState r m)
  , getMarloweQueryDriver :: m (DriverTraced (Handshake MarloweQuery) dState r m)
  , getTxJobDriver :: m (DriverTraced (Handshake (Job MarloweTxCommand)) dState r m)
  , result :: a
  }

hoistMarloweRuntimeServer :: Functor m => (forall x. m x -> n x) -> MarloweRuntimeServer r m a -> MarloweRuntimeServer r n a
hoistMarloweRuntimeServer f MarloweRuntimeServer{..} = MarloweRuntimeServer
  { getMarloweSyncDriver = f $ hoistDriverTraced f <$> getMarloweSyncDriver
  , getMarloweHeaderSyncDriver = f $ hoistDriverTraced f <$> getMarloweHeaderSyncDriver
  , getMarloweQueryDriver = f $ hoistDriverTraced f <$> getMarloweQueryDriver
  , getTxJobDriver = f $ hoistDriverTraced f <$> getTxJobDriver
  , ..
  }

marloweRuntimeServerPeer
  :: (MonadEvent r s m, MonadFail m)
  => InjectSelector ProxySelector s
  -> MarloweRuntimeServer r m a
  -> Peer MarloweRuntime 'AsServer 'StInit m a
marloweRuntimeServerPeer inj = peerTracedToPeer . marloweRuntimeServerPeerTraced inj

marloweRuntimeServerPeerTraced
  :: (MonadEvent r s m, MonadFail m)
  => InjectSelector ProxySelector s
  -> MarloweRuntimeServer r m a
  -> PeerTraced MarloweRuntime 'AsServer 'StInit r m a
marloweRuntimeServerPeerTraced inj MarloweRuntimeServer{..} = AwaitTraced (ClientAgency TokInit) $ Receive . \case
  MsgRunMarloweSync -> result <$ withHandshake inj SendMarloweSync RecvMarloweSync (marloweSyncPeerTraced (ClientAgency Sync.TokInit)) getMarloweSyncDriver
  MsgRunMarloweHeaderSync -> result <$ withHandshake inj SendMarloweHeaderSync RecvMarloweHeaderSync (marloweHeaderSyncPeerTraced (ClientAgency Header.TokIdle)) getMarloweHeaderSyncDriver
  MsgRunMarloweQuery -> result <$ withHandshake inj SendMarloweQuery RecvMarloweQuery (marloweQueryPeerTraced (ClientAgency Query.TokReq)) getMarloweQueryDriver
  MsgRunTxJob -> result <$ withHandshake inj SendTxJob RecvTxJob (jobPeerTraced (ClientAgency Job.TokInit)) getTxJobDriver

data ProxySelector f where
  Handshake :: ProxySelector Text
  HandshakeAccepted :: ProxySelector Void
  HandshakeRejected :: ProxySelector Void
  SendMarloweSync :: ProxySelector (AnyMessageAndAgency MarloweSync)
  SendMarloweHeaderSync :: ProxySelector (AnyMessageAndAgency MarloweHeaderSync)
  SendMarloweQuery :: ProxySelector (AnyMessageAndAgency MarloweQuery)
  SendTxJob :: ProxySelector (AnyMessageAndAgency (Job MarloweTxCommand))
  RecvMarloweSync :: ProxySelector (AnyMessageAndAgency MarloweSync)
  RecvMarloweHeaderSync :: ProxySelector (AnyMessageAndAgency MarloweHeaderSync)
  RecvMarloweQuery :: ProxySelector (AnyMessageAndAgency MarloweQuery)
  RecvTxJob :: ProxySelector (AnyMessageAndAgency (Job MarloweTxCommand))

withHandshake
  :: forall ps dState st r s m a
   . (MonadEvent r s m, Handshake.HasSignature ps, MonadFail m)
  => InjectSelector ProxySelector s
  -> ProxySelector (AnyMessageAndAgency ps)
  -> ProxySelector (AnyMessageAndAgency ps)
  -> (dState -> Driver ps dState m -> PeerTraced MarloweRuntime 'AsServer st r m a)
  -> m (DriverTraced (Handshake ps) dState r m)
  -> PeerTraced MarloweRuntime 'AsServer st r m a
withHandshake inj sendSelector recvSelector main getDriver = EffectTraced do
  driver <- getDriver
  withInjectEventFields inj Handshake [signature $ Proxy @ps] \ev -> do
    sendMessageTraced driver (reference ev) (ClientAgency Handshake.TokInit) $ Handshake.MsgHandshake $ signature $ Proxy @ps
    (_, SomeMessage handshakeResponse, dState') <- recvMessageTraced driver (ServerAgency Handshake.TokHandshake) (startDStateTraced driver)
    case handshakeResponse of
      Handshake.MsgReject -> do
        emitImmediateInjectEvent_ inj HandshakeRejected
        fail "Handshake rejected by upstream server"
      Handshake.MsgAccept -> do
        emitImmediateInjectEvent_ inj HandshakeAccepted
        pure $ main dState' $ case driver of
          DriverTraced{..} -> Driver
            { sendMessage = \tok msg ->
              withInjectEventFields inj sendSelector [AnyMessageAndAgency tok msg] \sendEv -> case tok of
                ClientAgency tok' ->
                  sendMessageTraced (reference sendEv) (ClientAgency $ Handshake.TokLiftClient tok') $ Handshake.MsgLift msg
                ServerAgency tok' ->
                  sendMessageTraced (reference sendEv) (ServerAgency $ Handshake.TokLiftServer tok') $ Handshake.MsgLift msg
            , recvMessage = \tok dState -> withInjectEvent inj recvSelector \_ -> case tok of
                ClientAgency tok' -> do
                  (_, SomeMessage msg, dState'') <- recvMessageTraced (ClientAgency $ Handshake.TokLiftClient tok') dState
                  case msg of
                    Handshake.MsgLift msg' -> pure (SomeMessage msg', dState'')
                ServerAgency tok' -> do
                  (_, SomeMessage msg, dState'') <- recvMessageTraced (ServerAgency $ Handshake.TokLiftServer tok') dState
                  case msg of
                    Handshake.MsgLift msg' -> pure (SomeMessage msg', dState'')
            , startDState = startDStateTraced
            }

marloweSyncPeerTraced
  :: Monad m
  => PeerHasAgency pr st
  -> dState
  -> Driver MarloweSync dState m
  -> PeerTraced MarloweRuntime 'AsServer ('StMarloweSync st) r m ()
marloweSyncPeerTraced tok dState driver = case tok of
  ClientAgency tok' -> AwaitTraced (ClientAgency $ TokClientMarloweSync tok') \case
    MsgMarloweSync msg -> case (tok', msg) of
      (_, Sync.MsgFollowContract{}) -> Respond (ServerAgency $ TokServerMarloweSync Sync.TokFollow) do
        sendMessage driver tok msg
        (SomeMessage msg', dState') <- recvMessage driver (ServerAgency Sync.TokFollow) dState
        pure $ Response (MsgMarloweSync msg') case msg' of
          Sync.MsgContractNotFound -> DoneTraced (TokNobodyMarloweSync Sync.TokDone) ()
          Sync.MsgContractFound _ v _ -> marloweSyncPeerTraced (ClientAgency $ Sync.TokIdle v) dState' driver
      (_, Sync.MsgIntersect _ v _) -> Respond (ServerAgency $ TokServerMarloweSync $ Sync.TokIntersect v) do
        sendMessage driver tok msg
        (SomeMessage msg', dState') <- recvMessage driver (ServerAgency $ Sync.TokIntersect v) dState
        pure $ Response (MsgMarloweSync msg') case msg' of
          Sync.MsgIntersectNotFound -> DoneTraced (TokNobodyMarloweSync Sync.TokDone) ()
          Sync.MsgIntersectFound{}-> marloweSyncPeerTraced (ClientAgency $ Sync.TokIdle v) dState' driver
      (_, Sync.MsgDone{}) -> Closed (TokNobodyMarloweSync Sync.TokDone) do
        sendMessage driver tok msg
      (Sync.TokIdle v, Sync.MsgRequestNext{}) -> Respond (ServerAgency $ TokServerMarloweSync $ Sync.TokNext v) do
        sendMessage driver tok msg
        (SomeMessage msg', dState') <- recvMessage driver (ServerAgency $ Sync.TokNext v) dState
        pure $ Response (MsgMarloweSync msg') case msg' of
          Sync.MsgRollForward{} -> marloweSyncPeerTraced (ClientAgency $ Sync.TokIdle v) dState' driver
          Sync.MsgRollBackward{} -> marloweSyncPeerTraced (ClientAgency $ Sync.TokIdle v) dState' driver
          Sync.MsgRollBackCreation{} -> DoneTraced (TokNobodyMarloweSync Sync.TokDone) ()
          Sync.MsgWait{} -> marloweSyncPeerTraced (ClientAgency $ Sync.TokWait v) dState' driver
      (Sync.TokWait v, Sync.MsgPoll{}) -> Respond (ServerAgency $ TokServerMarloweSync $ Sync.TokNext v) do
        sendMessage driver tok msg
        (SomeMessage msg', dState') <- recvMessage driver (ServerAgency $ Sync.TokNext v) dState
        pure $ Response (MsgMarloweSync msg') case msg' of
          Sync.MsgRollForward{} -> marloweSyncPeerTraced (ClientAgency $ Sync.TokIdle v) dState' driver
          Sync.MsgRollBackward{} -> marloweSyncPeerTraced (ClientAgency $ Sync.TokIdle v) dState' driver
          Sync.MsgRollBackCreation{} -> DoneTraced (TokNobodyMarloweSync Sync.TokDone) ()
          Sync.MsgWait{} -> marloweSyncPeerTraced (ClientAgency $ Sync.TokWait v) dState' driver
      (Sync.TokWait v, Sync.MsgCancel{}) -> Receive $ EffectTraced do
        sendMessage driver tok msg
        pure $ marloweSyncPeerTraced (ClientAgency $ Sync.TokIdle v) dState driver
  ServerAgency tok' -> EffectTraced do
    (SomeMessage msg, dState') <- recvMessage driver tok dState
    pure $ YieldTraced (ServerAgency $ TokServerMarloweSync tok') (MsgMarloweSync msg) case (tok', msg) of
      (_, Sync.MsgContractNotFound{}) -> Close (TokNobodyMarloweSync Sync.TokDone) ()
      (_, Sync.MsgContractFound _ v _) -> Cast $ marloweSyncPeerTraced (ClientAgency $ Sync.TokIdle v) dState' driver
      (Sync.TokNext v, Sync.MsgRollForward{}) -> Cast $ marloweSyncPeerTraced (ClientAgency $ Sync.TokIdle v) dState' driver
      (Sync.TokNext v, Sync.MsgRollBackward{}) -> Cast $ marloweSyncPeerTraced (ClientAgency $ Sync.TokIdle v) dState' driver
      (_, Sync.MsgRollBackCreation{}) -> Close (TokNobodyMarloweSync Sync.TokDone) ()
      (Sync.TokNext v, Sync.MsgWait{}) -> Cast $ marloweSyncPeerTraced (ClientAgency $ Sync.TokWait v) dState' driver
      (Sync.TokIntersect v, Sync.MsgIntersectFound{}) -> Cast $ marloweSyncPeerTraced (ClientAgency $ Sync.TokIdle v) dState' driver
      (_, Sync.MsgIntersectNotFound{}) -> Close (TokNobodyMarloweSync Sync.TokDone) ()

marloweHeaderSyncPeerTraced
  :: Monad m
  => PeerHasAgency pr st
  -> dState
  -> Driver MarloweHeaderSync dState m
  -> PeerTraced MarloweRuntime 'AsServer ('StMarloweHeaderSync st) r m ()
marloweHeaderSyncPeerTraced tok dState driver = case tok of
  ClientAgency tok' -> AwaitTraced (ClientAgency $ TokClientMarloweHeaderSync tok') \case
    MsgMarloweHeaderSync msg -> case msg of
      Header.MsgIntersect{} -> Respond (ServerAgency $ TokServerMarloweHeaderSync Header.TokIntersect) do
        sendMessage driver tok msg
        (SomeMessage msg', dState') <- recvMessage driver (ServerAgency Header.TokIntersect) dState
        pure $ Response (MsgMarloweHeaderSync msg') case msg' of
          Header.MsgIntersectNotFound -> marloweHeaderSyncPeerTraced (ClientAgency Header.TokIdle) dState' driver
          Header.MsgIntersectFound{} -> marloweHeaderSyncPeerTraced (ClientAgency Header.TokIdle) dState' driver
      Header.MsgDone{} -> Closed (TokNobodyMarloweHeaderSync Header.TokDone) do
        sendMessage driver tok msg
      Header.MsgRequestNext{} -> Respond (ServerAgency $ TokServerMarloweHeaderSync Header.TokNext) do
        sendMessage driver tok msg
        (SomeMessage msg', dState') <- recvMessage driver (ServerAgency Header.TokNext) dState
        pure $ Response (MsgMarloweHeaderSync msg') case msg' of
          Header.MsgNewHeaders{} -> marloweHeaderSyncPeerTraced (ClientAgency Header.TokIdle) dState' driver
          Header.MsgRollBackward{} -> marloweHeaderSyncPeerTraced (ClientAgency Header.TokIdle) dState' driver
          Header.MsgWait{} -> marloweHeaderSyncPeerTraced (ClientAgency Header.TokWait) dState' driver
      Header.MsgPoll{} -> Respond (ServerAgency $ TokServerMarloweHeaderSync Header.TokNext) do
        sendMessage driver tok msg
        (SomeMessage msg', dState') <- recvMessage driver (ServerAgency Header.TokNext) dState
        pure $ Response (MsgMarloweHeaderSync msg') case msg' of
          Header.MsgNewHeaders{} -> marloweHeaderSyncPeerTraced (ClientAgency Header.TokIdle) dState' driver
          Header.MsgRollBackward{} -> marloweHeaderSyncPeerTraced (ClientAgency Header.TokIdle) dState' driver
          Header.MsgWait{} -> marloweHeaderSyncPeerTraced (ClientAgency Header.TokWait) dState' driver
      Header.MsgCancel{} -> Receive $ EffectTraced do
        sendMessage driver tok msg
        pure $ marloweHeaderSyncPeerTraced (ClientAgency Header.TokIdle) dState driver
  ServerAgency tok' -> EffectTraced do
    (SomeMessage msg, dState') <- recvMessage driver tok dState
    pure $ YieldTraced (ServerAgency $ TokServerMarloweHeaderSync tok') (MsgMarloweHeaderSync msg) case msg of
      Header.MsgNewHeaders{} -> Cast $ marloweHeaderSyncPeerTraced (ClientAgency Header.TokIdle) dState' driver
      Header.MsgRollBackward{} -> Cast $ marloweHeaderSyncPeerTraced (ClientAgency Header.TokIdle) dState' driver
      Header.MsgWait{} -> Cast $ marloweHeaderSyncPeerTraced (ClientAgency Header.TokWait) dState' driver
      Header.MsgIntersectFound{} -> Cast $ marloweHeaderSyncPeerTraced (ClientAgency Header.TokIdle) dState' driver
      Header.MsgIntersectNotFound{} -> Cast $ marloweHeaderSyncPeerTraced (ClientAgency Header.TokIdle) dState' driver

marloweQueryPeerTraced
  :: Monad m
  => PeerHasAgency pr st
  -> dState
  -> Driver MarloweQuery dState m
  -> PeerTraced MarloweRuntime 'AsServer ('StMarloweQuery st) r m ()
marloweQueryPeerTraced tok dState driver = case tok of
  ClientAgency tok' -> AwaitTraced (ClientAgency $ TokClientMarloweQuery tok') \case
    MsgMarloweQuery msg -> case msg of
      Query.MsgRequest req -> Respond (ServerAgency $ TokServerMarloweQuery $ Query.TokRes $ Query.requestToSt req) do
        sendMessage driver tok msg
        (SomeMessage msg', dState') <- recvMessage driver (ServerAgency $ Query.TokRes $ Query.requestToSt req) dState
        pure $ Response (MsgMarloweQuery msg') case msg' of
          Query.MsgRespond{} -> marloweQueryPeerTraced (ClientAgency Query.TokReq) dState' driver
      Query.MsgDone -> Closed (TokNobodyMarloweQuery Query.TokDone) do
        sendMessage driver tok msg
  ServerAgency tok' -> EffectTraced do
    (SomeMessage msg, dState') <- recvMessage driver tok dState
    pure $ YieldTraced (ServerAgency $ TokServerMarloweQuery tok') (MsgMarloweQuery msg) case msg of
      Query.MsgRespond{} -> Cast $ marloweQueryPeerTraced (ClientAgency Query.TokReq) dState' driver

jobPeerTraced
  :: Monad m
  => PeerHasAgency pr st
  -> dState
  -> Driver (Job MarloweTxCommand) dState m
  -> PeerTraced MarloweRuntime 'AsServer ('StTxJob st) r m ()
jobPeerTraced tok dState driver = case tok of
  ClientAgency tok' -> AwaitTraced (ClientAgency $ TokClientTxJob tok') \case
    MsgTxJob msg -> case (tok', msg) of
      (_, Job.MsgExec cmd) -> Respond (ServerAgency $ TokServerTxJob $ Job.TokCmd $ Job.tagFromCommand cmd) do
        sendMessage driver tok msg
        (SomeMessage msg', dState') <- recvMessage driver (ServerAgency $ Job.TokCmd $ Job.tagFromCommand cmd) dState
        pure $ Response (MsgTxJob msg') case msg' of
          Job.MsgFail{} -> DoneTraced (TokNobodyTxJob Job.TokDone) ()
          Job.MsgSucceed{} -> DoneTraced (TokNobodyTxJob Job.TokDone) ()
          Job.MsgAwait{} -> jobPeerTraced (ClientAgency $ Job.TokAwait $ Job.tagFromCommand cmd) dState' driver
      (_, Job.MsgAttach jobId) -> Respond (ServerAgency $ TokServerTxJob $ Job.TokAttach $ Job.tagFromJobId jobId) do
        sendMessage driver tok msg
        (SomeMessage msg', dState') <- recvMessage driver (ServerAgency $ Job.TokAttach $ Job.tagFromJobId jobId) dState
        pure $ Response (MsgTxJob msg') case msg' of
          Job.MsgAttachFailed{} -> DoneTraced (TokNobodyTxJob Job.TokDone) ()
          Job.MsgAttached{} -> jobPeerTraced (ServerAgency $ Job.TokCmd $ Job.tagFromJobId jobId) dState' driver
      (Job.TokAwait tag, Job.MsgPoll) -> Respond (ServerAgency $ TokServerTxJob $ Job.TokCmd tag) do
        sendMessage driver tok msg
        (SomeMessage msg', dState') <- recvMessage driver (ServerAgency $ Job.TokCmd tag) dState
        pure $ Response (MsgTxJob msg') case msg' of
          Job.MsgFail{} -> DoneTraced (TokNobodyTxJob Job.TokDone) ()
          Job.MsgSucceed{} -> DoneTraced (TokNobodyTxJob Job.TokDone) ()
          Job.MsgAwait{} -> jobPeerTraced (ClientAgency $ Job.TokAwait tag) dState' driver
      (_, Job.MsgDetach) -> Closed (TokNobodyTxJob Job.TokDone) do
        sendMessage driver tok msg
  ServerAgency tok' -> EffectTraced do
    (SomeMessage msg, dState') <- recvMessage driver tok dState
    pure $ YieldTraced (ServerAgency $ TokServerTxJob tok') (MsgTxJob msg) case (tok', msg) of
      (_, Job.MsgFail{}) -> Close (TokNobodyTxJob Job.TokDone) ()
      (_, Job.MsgSucceed{}) -> Close (TokNobodyTxJob Job.TokDone) ()
      (Job.TokCmd tag, Job.MsgAwait{}) -> Cast $ jobPeerTraced (ClientAgency $ Job.TokAwait tag) dState' driver
      (Job.TokAttach tag, Job.MsgAttached{}) -> Cast $ jobPeerTraced (ServerAgency $ Job.TokCmd tag) dState' driver
      (_, Job.MsgAttachFailed{}) -> Close (TokNobodyTxJob Job.TokDone) ()
