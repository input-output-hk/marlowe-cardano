{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.Server
  where

import Control.Monad.Event.Class
import Data.Functor (($>))
import Data.Proxy (Proxy(Proxy))
import Data.String (fromString)
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
import Network.Protocol.Codec.Spec (showsPrecMessage)
import Network.Protocol.Handshake.Types
  (ClientHasAgency(TokLiftClient), Handshake, HasSignature, Message(MsgLift), ServerHasAgency(TokLiftServer), signature)
import qualified Network.Protocol.Handshake.Types as Handshake
import Network.Protocol.Job.Types (Job)
import qualified Network.Protocol.Job.Types as Job
import Network.TypedProtocol
import Observe.Event (InjectSelector, NewEventArgs(..), addField, reference)
import Observe.Event.Backend (setInitialCauseEventBackend, simpleNewEventArgs)
import Observe.Event.Render.OpenTelemetry
import OpenTelemetry.Attributes (toAttribute)
import OpenTelemetry.Trace.Core (SpanKind(..))

type RunHandshake m = forall r. (Text -> m () -> m () -> m r) -> m r

data DriverWithRunHandshake ps dState m = DriverWithRunHandshake
  { driver :: Driver (Handshake ps) dState m
  , runHandshake :: RunHandshake m
  }

wrapDriver :: forall ps dState m. (HasSignature ps, Applicative m) => Driver (Handshake ps) dState m -> DriverWithRunHandshake ps dState m
wrapDriver driver = DriverWithRunHandshake
  { driver
  , runHandshake = \handshake -> handshake (signature $ Proxy @ps) (pure ()) (pure ())
  }

data MarloweRuntimeServer m a = forall dState. MarloweRuntimeServer
  { getMarloweSyncDriver :: m (DriverWithRunHandshake MarloweSync dState m)
  , getMarloweHeaderSyncDriver :: m (DriverWithRunHandshake MarloweHeaderSync dState m)
  , getMarloweQueryDriver :: m (DriverWithRunHandshake MarloweQuery dState m)
  , getTxJobDriver :: m (DriverWithRunHandshake (Job MarloweTxCommand) dState m)
  , result :: a
  }

marloweRuntimeServerPeer :: Monad m => MarloweRuntimeServer m a -> Peer MarloweRuntime 'AsServer 'StInit m a
marloweRuntimeServerPeer MarloweRuntimeServer{..} = Await (ClientAgency TokInit) \case
  MsgRunMarloweSync -> result <$ withHandshake (marloweSyncPeer (ClientAgency Sync.TokInit)) getMarloweSyncDriver
  MsgRunMarloweHeaderSync -> result <$ withHandshake (marloweHeaderSyncPeer (ClientAgency Header.TokIdle)) getMarloweHeaderSyncDriver
  MsgRunMarloweQuery -> result <$ withHandshake (marloweQueryPeer (ClientAgency Query.TokReq)) getMarloweQueryDriver
  MsgRunTxJob -> result <$ withHandshake (jobPeer (ClientAgency Job.TokInit)) getTxJobDriver

withHandshake
  :: forall ps dState st m a
   . Monad m
  => (dState -> Driver ps dState m -> Peer MarloweRuntime 'AsServer st m a)
  -> m (DriverWithRunHandshake ps dState m)
  -> Peer MarloweRuntime 'AsServer st m a
withHandshake main getDriver = Effect do
  DriverWithRunHandshake{..} <- getDriver
  runHandshake \sig onReject onAccept -> do
    sendMessage driver (ClientAgency Handshake.TokInit) $ Handshake.MsgHandshake sig
    (SomeMessage handshakeResponse, dState') <- recvMessage driver (ServerAgency Handshake.TokHandshake) (startDState driver)
    case handshakeResponse of
      Handshake.MsgReject -> onReject *> error "Handshake rejected by upstream server"
      Handshake.MsgAccept -> onAccept $> main dState' case driver of
        Driver{..} -> Driver
          { sendMessage = \case
              ClientAgency tok -> \msg -> sendMessage (ClientAgency $ Handshake.TokLiftClient tok) $ Handshake.MsgLift msg
              ServerAgency tok -> \msg -> sendMessage (ServerAgency $ Handshake.TokLiftServer tok) $ Handshake.MsgLift msg
          , recvMessage = \case
              ClientAgency tok -> \dState -> do
                (SomeMessage msg, dState'') <- recvMessage (ClientAgency $ Handshake.TokLiftClient tok) dState
                case msg of
                  Handshake.MsgLift msg' -> pure (SomeMessage msg', dState'')
              ServerAgency tok -> \dState -> do
                (SomeMessage msg, dState'') <- recvMessage (ServerAgency $ Handshake.TokLiftServer tok) dState
                case msg of
                  Handshake.MsgLift msg' -> pure (SomeMessage msg', dState'')
          , ..
          }

marloweSyncPeer
  :: Monad m
  => PeerHasAgency pr st
  -> dState
  -> Driver MarloweSync dState m
  -> Peer MarloweRuntime 'AsServer ('StMarloweSync st) m ()
marloweSyncPeer tok dState driver = case tok of
  ClientAgency tok' -> Await (ClientAgency $ TokClientMarloweSync tok') \case
    MsgMarloweSync msg -> Effect do
      sendMessage driver tok msg
      pure case (tok', msg) of
        (_, Sync.MsgFollowContract{}) -> marloweSyncPeer (ServerAgency Sync.TokFollow) dState driver
        (_, Sync.MsgIntersect _ v _) -> marloweSyncPeer (ServerAgency $ Sync.TokIntersect v) dState driver
        (_, Sync.MsgDone{}) -> Done (TokNobodyMarloweSync Sync.TokDone) ()
        (Sync.TokIdle v, Sync.MsgRequestNext{}) -> marloweSyncPeer (ServerAgency $ Sync.TokNext v) dState driver
        (Sync.TokWait v, Sync.MsgPoll{}) -> marloweSyncPeer (ServerAgency $ Sync.TokNext v) dState driver
        (Sync.TokWait v, Sync.MsgCancel{}) -> marloweSyncPeer (ClientAgency $ Sync.TokIdle v) dState driver
  ServerAgency tok' -> Effect do
    (SomeMessage msg, dState') <- recvMessage driver tok dState
    pure $ Yield (ServerAgency $ TokServerMarloweSync tok') (MsgMarloweSync msg) case (tok', msg) of
      (_, Sync.MsgContractNotFound{}) -> Done (TokNobodyMarloweSync Sync.TokDone) ()
      (_, Sync.MsgContractFound _ v _) -> marloweSyncPeer (ClientAgency $ Sync.TokIdle v) dState' driver
      (Sync.TokNext v, Sync.MsgRollForward{}) -> marloweSyncPeer (ClientAgency $ Sync.TokIdle v) dState' driver
      (Sync.TokNext v, Sync.MsgRollBackward{}) -> marloweSyncPeer (ClientAgency $ Sync.TokIdle v) dState' driver
      (_, Sync.MsgRollBackCreation{}) -> Done (TokNobodyMarloweSync Sync.TokDone) ()
      (Sync.TokNext v, Sync.MsgWait{}) -> marloweSyncPeer (ClientAgency $ Sync.TokWait v) dState' driver
      (Sync.TokIntersect v, Sync.MsgIntersectFound{}) -> marloweSyncPeer (ClientAgency $ Sync.TokIdle v) dState' driver
      (_, Sync.MsgIntersectNotFound{}) -> Done (TokNobodyMarloweSync Sync.TokDone) ()

marloweHeaderSyncPeer
  :: Monad m
  => PeerHasAgency pr st
  -> dState
  -> Driver MarloweHeaderSync dState m
  -> Peer MarloweRuntime 'AsServer ('StMarloweHeaderSync st) m ()
marloweHeaderSyncPeer tok dState driver = case tok of
  ClientAgency tok' -> Await (ClientAgency $ TokClientMarloweHeaderSync tok') \case
    MsgMarloweHeaderSync msg -> Effect do
      sendMessage driver tok msg
      pure case msg of
        Header.MsgIntersect{} -> marloweHeaderSyncPeer (ServerAgency Header.TokIntersect) dState driver
        Header.MsgDone{} -> Done (TokNobodyMarloweHeaderSync Header.TokDone) ()
        Header.MsgRequestNext{} -> marloweHeaderSyncPeer (ServerAgency Header.TokNext) dState driver
        Header.MsgPoll{} -> marloweHeaderSyncPeer (ServerAgency Header.TokNext) dState driver
        Header.MsgCancel{} -> marloweHeaderSyncPeer (ClientAgency Header.TokIdle) dState driver
  ServerAgency tok' -> Effect do
    (SomeMessage msg, dState') <- recvMessage driver tok dState
    pure $ Yield (ServerAgency $ TokServerMarloweHeaderSync tok') (MsgMarloweHeaderSync msg) case msg of
      Header.MsgNewHeaders{} -> marloweHeaderSyncPeer (ClientAgency Header.TokIdle) dState' driver
      Header.MsgRollBackward{} -> marloweHeaderSyncPeer (ClientAgency Header.TokIdle) dState' driver
      Header.MsgWait{} -> marloweHeaderSyncPeer (ClientAgency Header.TokWait) dState' driver
      Header.MsgIntersectFound{} -> marloweHeaderSyncPeer (ClientAgency Header.TokIdle) dState' driver
      Header.MsgIntersectNotFound{} -> marloweHeaderSyncPeer (ClientAgency Header.TokIdle) dState' driver

marloweQueryPeer
  :: Monad m
  => PeerHasAgency pr st
  -> dState
  -> Driver MarloweQuery dState m
  -> Peer MarloweRuntime 'AsServer ('StMarloweQuery st) m ()
marloweQueryPeer tok dState driver = case tok of
  ClientAgency tok' -> Await (ClientAgency $ TokClientMarloweQuery tok') \case
    MsgMarloweQuery msg -> Effect do
      sendMessage driver tok msg
      pure case msg of
        Query.MsgRequest req -> marloweQueryPeer (ServerAgency $ Query.TokRes $ Query.requestToSt req) dState driver
        Query.MsgDone -> Done (TokNobodyMarloweQuery Query.TokDone) ()
  ServerAgency tok' -> Effect do
    (SomeMessage msg, dState') <- recvMessage driver tok dState
    pure $ Yield (ServerAgency $ TokServerMarloweQuery tok') (MsgMarloweQuery msg) case msg of
      Query.MsgRespond{} -> marloweQueryPeer (ClientAgency Query.TokReq) dState' driver

jobPeer
  :: Monad m
  => PeerHasAgency pr st
  -> dState
  -> Driver (Job MarloweTxCommand) dState m
  -> Peer MarloweRuntime 'AsServer ('StTxJob st) m ()
jobPeer tok dState driver = case tok of
  ClientAgency tok' -> Await (ClientAgency $ TokClientTxJob tok') \case
    MsgTxJob msg -> Effect do
      sendMessage driver tok msg
      pure case (tok', msg) of
        (_, Job.MsgExec cmd) -> jobPeer (ServerAgency $ Job.TokCmd $ Job.tagFromCommand cmd) dState driver
        (_, Job.MsgAttach jobId) -> jobPeer (ServerAgency $ Job.TokAttach $ Job.tagFromJobId jobId) dState driver
        (Job.TokAwait tag, Job.MsgPoll) -> jobPeer (ServerAgency $ Job.TokCmd tag) dState driver
        (_, Job.MsgDetach) -> Done (TokNobodyTxJob Job.TokDone) ()
  ServerAgency tok' -> Effect do
    (SomeMessage msg, dState') <- recvMessage driver tok dState
    pure $ Yield (ServerAgency $ TokServerTxJob tok') (MsgTxJob msg) case (tok', msg) of
      (_, Job.MsgFail{}) -> Done (TokNobodyTxJob Job.TokDone) ()
      (_, Job.MsgSucceed{}) -> Done (TokNobodyTxJob Job.TokDone) ()
      (Job.TokCmd tag, Job.MsgAwait{}) -> jobPeer (ClientAgency $ Job.TokAwait tag) dState' driver
      (Job.TokAttach tag, Job.MsgAttached{}) -> jobPeer (ServerAgency $ Job.TokCmd tag) dState' driver
      (_, Job.MsgAttachFailed{}) -> Done (TokNobodyTxJob Job.TokDone) ()

data MarloweRuntimeServerSelector f where
  RunMarloweSync :: MarloweRuntimeServerSelector Void
  RunMarloweHeaderSync :: MarloweRuntimeServerSelector Void
  RunMarloweQuery :: MarloweRuntimeServerSelector Void
  RunTxJob :: MarloweRuntimeServerSelector Void
  Handshake :: MarloweRuntimeServerSelector Text
  HandshakeRejected :: MarloweRuntimeServerSelector Void
  HandshakeAccepted :: MarloweRuntimeServerSelector Void
  ProxyClientMsg :: MarloweRuntimeServerSelector ClientMsg
  ProxyServerMsg :: MarloweRuntimeServerSelector ServerMsg

data ClientMsg where
  MarloweSyncClientMsg :: ClientHasAgency st -> Message MarloweSync st st' -> ClientMsg
  MarloweHeaderSyncClientMsg :: ClientHasAgency st -> Message MarloweHeaderSync st st' -> ClientMsg
  MarloweQueryClientMsg :: ClientHasAgency st -> Message MarloweQuery st st' -> ClientMsg
  TxJobClientMsg :: ClientHasAgency st -> Message (Job MarloweTxCommand) st st' -> ClientMsg

data ServerMsg where
  MarloweSyncServerMsg :: ServerHasAgency st -> Message MarloweSync st st' -> ServerMsg
  MarloweHeaderSyncServerMsg :: ServerHasAgency st -> Message MarloweHeaderSync st st' -> ServerMsg
  MarloweQueryServerMsg :: ServerHasAgency st -> Message MarloweQuery st st' -> ServerMsg
  TxJobServerMsg :: ServerHasAgency st -> Message (Job MarloweTxCommand) st st' -> ServerMsg

traceMarloweRuntimeServer
  :: MonadEvent r s m
  => InjectSelector MarloweRuntimeServerSelector s
  -> r
  -> MarloweRuntimeServer m a
  -> MarloweRuntimeServer m a
traceMarloweRuntimeServer inj rootCause MarloweRuntimeServer{..} = MarloweRuntimeServer
  { getMarloweSyncDriver = traceDriver inj rootCause RunMarloweSync MarloweSyncClientMsg MarloweSyncServerMsg getMarloweSyncDriver
  , getMarloweHeaderSyncDriver = traceDriver inj rootCause RunMarloweHeaderSync MarloweHeaderSyncClientMsg MarloweHeaderSyncServerMsg getMarloweHeaderSyncDriver
  , getMarloweQueryDriver = traceDriver inj rootCause RunMarloweQuery MarloweQueryClientMsg MarloweQueryServerMsg getMarloweQueryDriver
  , getTxJobDriver = traceDriver inj rootCause RunTxJob TxJobClientMsg TxJobServerMsg getTxJobDriver
  , result
  }

traceDriver
  :: MonadEvent r s m
  => InjectSelector MarloweRuntimeServerSelector s
  -> r
  -> MarloweRuntimeServerSelector Void
  -> (forall st st'. ClientHasAgency st -> Message ps st st' -> ClientMsg)
  -> (forall st st'. ServerHasAgency st -> Message ps st st' -> ServerMsg)
  -> m (DriverWithRunHandshake ps dState m)
  -> m (DriverWithRunHandshake ps dState m)
traceDriver inj rootCause acquireEvent clientMsg serverMsg getDriver =
  withInjectEventArgs inj (simpleNewEventArgs acquireEvent) { newEventCauses = [rootCause] } \ev -> do
    DriverWithRunHandshake Driver{..} runHandshake <- getDriver
    pure DriverWithRunHandshake
      { driver = Driver
        { sendMessage = \case
            ClientAgency (TokLiftClient tok) -> \(MsgLift msg) ->
              withInjectEventArgs inj (simpleNewEventArgs ProxyClientMsg) { newEventInitialFields = [clientMsg tok msg], newEventCauses = [rootCause, reference ev] } \_ -> do
                sendMessage (ClientAgency $ TokLiftClient tok) $ MsgLift msg
            tok -> sendMessage tok
        , recvMessage = \case
            ServerAgency (TokLiftServer tok) -> \dState -> withInjectEventArgs inj (simpleNewEventArgs ProxyServerMsg) { newEventCauses = [rootCause, reference ev] } \ev' -> do
              (SomeMessage msg, dState') <- recvMessage (ServerAgency $ TokLiftServer tok) dState
              _ :: () <- case msg of
                MsgLift msg' -> addField ev' $ serverMsg tok msg'
              pure (SomeMessage msg, dState')
            tok -> recvMessage tok
        , ..
        }
      , runHandshake = \handshake -> localBackend (setInitialCauseEventBackend [rootCause, reference ev]) do
          runHandshake \sig onReject onAccept ->
            withInjectEventArgs inj (simpleNewEventArgs Handshake) { newEventInitialFields = [sig], newEventCauses = [rootCause, reference ev] } \_ -> do
              handshake sig
                (emitImmediateInjectEventArgs_ inj (simpleNewEventArgs HandshakeRejected) { newEventCauses = [rootCause, reference ev] } *> onReject)
                (emitImmediateInjectEventArgs_ inj (simpleNewEventArgs HandshakeAccepted) { newEventCauses = [rootCause, reference ev] } *> onAccept)
      }

renderMarloweRuntimeServerSelectorOTel :: RenderSelectorOTel MarloweRuntimeServerSelector
renderMarloweRuntimeServerSelectorOTel = \case
  RunMarloweSync -> OTelRendered
    { eventName = "marlowe_runtime/proxy/run_marlowe_sync"
    , eventKind = Server
    , renderField = \case
    }
  RunMarloweHeaderSync -> OTelRendered
    { eventName = "marlowe_runtime/proxy/run_marlowe_header_sync"
    , eventKind = Server
    , renderField = \case
    }
  RunMarloweQuery -> OTelRendered
    { eventName = "marlowe_runtime/proxy/run_marlowe_query"
    , eventKind = Server
    , renderField = \case
    }
  RunTxJob -> OTelRendered
    { eventName = "marlowe_runtime/proxy/run_tx_job"
    , eventKind = Server
    , renderField = \case
    }
  Handshake -> OTelRendered
    { eventName = "marlowe_runtime/proxy/handshake"
    , eventKind = Server
    , renderField = \sig -> [("handshake.signature", toAttribute sig)]
    }
  HandshakeRejected -> OTelRendered
    { eventName = "marlowe_runtime/proxy/handshake_rejected"
    , eventKind = Internal
    , renderField = \case
    }
  HandshakeAccepted -> OTelRendered
    { eventName = "marlowe_runtime/proxy/handshake_accepted"
    , eventKind = Internal
    , renderField = \case
    }
  ProxyClientMsg -> OTelRendered
    { eventName = "marlowe_runtime/proxy/proxy_client_msg"
    , eventKind = Server
    , renderField = \case
        MarloweSyncClientMsg tok msg ->
          [ ("marlowe_runtime.sub_protocol", "marlowe_sync")
          , ("protocol.msg", fromString $ showsPrecMessage 0 (ClientAgency tok) msg "")
          ]
        MarloweHeaderSyncClientMsg tok msg ->
          [ ("marlowe_runtime.sub_protocol", "marlowe_header_sync")
          , ("protocol.msg", fromString $ showsPrecMessage 0 (ClientAgency tok) msg "")
          ]
        MarloweQueryClientMsg tok msg ->
          [ ("marlowe_runtime.sub_protocol", "marlowe_query")
          , ("protocol.msg", fromString $ showsPrecMessage 0 (ClientAgency tok) msg "")
          ]
        TxJobClientMsg tok msg ->
          [ ("marlowe_runtime.sub_protocol", "tx_job")
          , ("protocol.msg", fromString $ showsPrecMessage 0 (ClientAgency tok) msg "")
          ]
    }
  ProxyServerMsg -> OTelRendered
    { eventName = "marlowe_runtime/proxy/proxy_server_msg"
    , eventKind = Server
    , renderField = \case
        MarloweSyncServerMsg tok msg ->
          [ ("marlowe_runtime.sub_protocol", "marlowe_sync")
          , ("protocol.msg", fromString $ showsPrecMessage 0 (ServerAgency tok) msg "")
          ]
        MarloweHeaderSyncServerMsg tok msg ->
          [ ("marlowe_runtime.sub_protocol", "marlowe_header_sync")
          , ("protocol.msg", fromString $ showsPrecMessage 0 (ServerAgency tok) msg "")
          ]
        MarloweQueryServerMsg tok msg ->
          [ ("marlowe_runtime.sub_protocol", "marlowe_query")
          , ("protocol.msg", fromString $ showsPrecMessage 0 (ServerAgency tok) msg "")
          ]
        TxJobServerMsg tok msg ->
          [ ("marlowe_runtime.sub_protocol", "tx_job")
          , ("protocol.msg", fromString $ showsPrecMessage 0 (ServerAgency tok) msg "")
          ]
    }
