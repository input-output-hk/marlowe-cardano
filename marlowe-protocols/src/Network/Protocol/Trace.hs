{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Network.Protocol.Trace
  where

import Control.Monad (replicateM)
import Control.Monad.Event.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Binary (Binary, get, getWord8, put)
import Data.Binary.Put (putWord8)
import qualified Data.ByteString as B
import Data.Foldable (traverse_)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Network.Protocol.Codec
import Network.Protocol.Connection (Connection(..), Connector(..))
import Network.TypedProtocol
import Observe.Event (InjectSelector, NewEventArgs(..), addField, reference)
import Observe.Event.Backend (setAncestorEventBackend, simpleNewEventArgs)
import Observe.Event.Render.OpenTelemetry
import OpenTelemetry.Trace.Core
  ( Span
  , SpanContext(..)
  , SpanKind(Consumer, Producer)
  , TraceFlags
  , getSpanContext
  , toAttribute
  , traceFlagsFromWord8
  , traceFlagsValue
  , wrapSpanContext
  )
import OpenTelemetry.Trace.Id (SpanId, TraceId, bytesToSpanId, bytesToTraceId, spanIdBytes, traceIdBytes)
import OpenTelemetry.Trace.TraceState (Key(..), TraceState, Value(..), empty, insert, toList)

data Trace ps where
  StInit :: ps -> Trace ps
  StTrace :: ps -> Trace ps

instance Protocol ps => Protocol (Trace ps) where
  data Message (Trace ps) st st' where
    MsgInit :: Maybe SpanContext -> UUID -> Message (Trace ps) ('StInit st) ('StTrace st)
    MsgTrace :: Maybe SpanContext -> UUID -> Message ps st st' -> Message (Trace ps) ('StTrace st) ('StTrace st')

  data ClientHasAgency st where
    TokInit :: ClientHasAgency ('StInit st)
    TokTraceClient :: ClientHasAgency st -> ClientHasAgency ('StTrace st)

  data ServerHasAgency st where
    TokTraceServer :: ServerHasAgency st -> ServerHasAgency ('StTrace st)

  data NobodyHasAgency st where
    TokTraceNobody :: NobodyHasAgency st -> NobodyHasAgency ('StTrace st)

  exclusionLemma_ClientAndServerHaveAgency (TokTraceClient tok) (TokTraceServer tok') = exclusionLemma_ClientAndServerHaveAgency tok tok'
  exclusionLemma_NobodyAndClientHaveAgency (TokTraceNobody tok) (TokTraceClient tok') = exclusionLemma_NobodyAndClientHaveAgency tok tok'
  exclusionLemma_NobodyAndServerHaveAgency (TokTraceNobody tok) (TokTraceServer tok') = exclusionLemma_NobodyAndServerHaveAgency tok tok'

instance Binary TraceFlags where
  put = putWord8 . traceFlagsValue
  get = traceFlagsFromWord8 <$> getWord8

instance Binary TraceId where
  put = traverse_ putWord8 . B.unpack . traceIdBytes
  get = either fail pure . bytesToTraceId . B.pack =<< replicateM 16 getWord8

instance Binary SpanId where
  put = traverse_ putWord8 . B.unpack . spanIdBytes
  get = either fail pure . bytesToSpanId . B.pack =<< replicateM 8 getWord8

instance Binary TraceState where
  put = put . toList
  get = fromList <$> get
    where
      fromList :: [(Key, Value)] -> TraceState
      fromList = foldr (uncurry insert) empty

instance Binary Key where
  put (Key t) = put t
  get = Key <$> get

instance Binary Value where
  put (Value t) = put t
  get = Value <$> get

instance Binary SpanContext where
  put SpanContext{..} = do
    put traceFlags
    put traceId
    put spanId
    put traceState
  get = do
    traceFlags <- get
    let isRemote = True
    traceId <- get
    spanId <- get
    traceState <- get
    pure SpanContext{..}

instance BinaryMessage ps => BinaryMessage (Trace ps) where
  putMessage tok (MsgTrace r msgId msg) = do
    put r
    put msgId
    case tok of
      ClientAgency (TokTraceClient tok') -> putMessage (ClientAgency tok') msg
      ServerAgency (TokTraceServer tok') -> putMessage (ServerAgency tok') msg
  putMessage _ (MsgInit r conversationId) = do
    put r
    put conversationId
  getMessage = \case
    ClientAgency TokInit -> SomeMessage <$> (MsgInit <$> get <*> get)
    ClientAgency (TokTraceClient tok) -> do
      r <- get
      msgId <- get
      SomeMessage msg <- getMessage (ClientAgency tok)
      pure $ SomeMessage $ MsgTrace r msgId msg
    ServerAgency (TokTraceServer tok) -> do
      r <- get
      msgId <- get
      SomeMessage msg <- getMessage (ServerAgency tok)
      pure $ SomeMessage $ MsgTrace r msgId msg

data PeerReduced ps pr (st :: ps) m a where
  YieldReduced :: WeHaveAgency pr st -> Message ps st st' -> Peer ps pr st' m a -> PeerReduced ps pr st m a
  AwaitReduced :: TheyHaveAgency pr st -> (forall st'. Message ps st st' -> Peer ps pr st' m a) -> PeerReduced ps pr st m a
  DoneReduced :: NobodyHasAgency st -> a -> PeerReduced ps pr st m a

reducePeer :: Monad m => Peer ps pr st m a -> m (PeerReduced ps pr st m a)
reducePeer = \case
  Effect m -> reducePeer =<< m
  Done tok a -> pure $ DoneReduced tok a
  Yield tok msg peer -> pure $ YieldReduced tok msg peer
  Await tok k -> pure $ AwaitReduced tok k

data PeerSelector ps f where
  Init :: PeerSelector ps UUID
  ReceiveInit :: PeerSelector ps UUID
  Publish :: PeerSelector ps (MessageField ps)
  Receive :: PeerSelector ps (MessageField ps)
  Process :: PeerSelector ps (UUID, UUID)

data MessageField ps where
  Message :: Message ps st st' -> MessageField ps
  MessageId :: UUID -> MessageField ps
  ConversationId :: UUID -> MessageField ps

class HasSpanContext r where
  context :: MonadIO m => r -> m SpanContext
  wrapContext :: SpanContext -> r

instance HasSpanContext Span where
  context = getSpanContext
  wrapContext = wrapSpanContext

instance (Monoid b, HasSpanContext a) => HasSpanContext (a, b) where
  context = context . fst
  wrapContext = (,mempty) . wrapContext

traceClientConnector
  :: forall ps client r s m
   . (MonadEvent r s m, MonadIO m, HasSpanContext r)
  => InjectSelector (PeerSelector ps) s
  -> Connector ps 'AsClient client m
  -> Connector (Trace ps) 'AsClient client m
traceClientConnector inj Connector{..} = Connector $ traceClientConnection inj <$> openConnection

traceClientConnectorNoop
  :: forall ps client m
   . MonadIO m
  => Connector ps 'AsClient client m
  -> Connector (Trace ps) 'AsClient client m
traceClientConnectorNoop Connector{..} = Connector $ traceClientConnectionNoop <$> openConnection

traceClientConnection
  :: forall ps client r s m
   . (MonadEvent r s m, MonadIO m, HasSpanContext r)
  => InjectSelector (PeerSelector ps) s
  -> Connection ps 'AsClient client m
  -> Connection (Trace ps) 'AsClient client m
traceClientConnection inj Connection{..} = Connection
  { toPeer = traceClient inj . toPeer
  , ..
  }

traceClientConnectionNoop
  :: forall ps client m
   . MonadIO m
  => Connection ps 'AsClient client m
  -> Connection (Trace ps) 'AsClient client m
traceClientConnectionNoop Connection{..} = Connection
  { toPeer = traceClientNoop . toPeer
  , ..
  }

traceClientNoop
  :: forall ps st a m
   . MonadIO m
  => Peer ps 'AsClient st m a
  -> Peer (Trace ps) 'AsClient ('StInit st) m a
traceClientNoop peer = Effect do
  msgId <- liftIO nextRandom
  pure $ Yield (ClientAgency TokInit) (MsgInit Nothing msgId) $ tracePeerNoop peer

tracePeerNoop :: MonadIO m => Peer ps pr st m a -> Peer (Trace ps) pr ('StTrace st) m a
tracePeerNoop = \case
  Effect m -> Effect $ tracePeerNoop <$> m
  Done tok a -> Done (TokTraceNobody tok) a
  Yield (ClientAgency tok) msg peer -> Effect do
    msgId <- liftIO nextRandom
    pure $ Yield (ClientAgency $ TokTraceClient tok) (MsgTrace Nothing msgId msg) $ tracePeerNoop peer
  Yield (ServerAgency tok) msg peer -> Effect do
    msgId <- liftIO nextRandom
    pure $ Yield (ServerAgency $ TokTraceServer tok) (MsgTrace Nothing msgId msg) $ tracePeerNoop peer
  Await (ClientAgency tok) k -> Await (ClientAgency $ TokTraceClient tok) \case
    MsgTrace _ _ msg -> tracePeerNoop $ k msg
  Await (ServerAgency tok) k -> Await (ServerAgency $ TokTraceServer tok) \case
    MsgTrace _ _ msg -> tracePeerNoop $ k msg

traceClient
  :: forall ps st a r s m
   . (MonadEvent r s m, MonadIO m, HasSpanContext r)
  => InjectSelector (PeerSelector ps) s
  -> Peer ps 'AsClient st m a
  -> Peer (Trace ps) 'AsClient ('StInit st) m a
traceClient inj peer = Effect $ withInjectEvent inj Init \ev -> do
  msgId <- liftIO nextRandom
  addField ev msgId
  peer' <- reducePeer peer
  spanContext <- context $ reference ev
  pure $ Yield (ClientAgency TokInit) (MsgInit (Just spanContext) msgId) $ tracePeer inj (reference ev) msgId peer'

traceServer
  :: forall ps st a r s m
   . (MonadEvent r s m, MonadIO m, HasSpanContext r)
  => InjectSelector (PeerSelector ps) s
  -> Peer ps 'AsServer st m a
  -> Peer (Trace ps) 'AsServer ('StInit st) m a
traceServer inj peer = Await (ClientAgency TokInit) \case
  MsgInit mSpanContext msgId ->
    Effect do
      let args = (simpleNewEventArgs ReceiveInit) { newEventParent = wrapContext <$> mSpanContext, newEventInitialFields = [msgId] }
      withInjectEventArgs inj args \ev -> tracePeer inj (maybe (reference ev) wrapContext mSpanContext) msgId <$> reducePeer peer

tracePeer
  :: forall ps pr st m a r s
   . (MonadEvent r s m, MonadIO m, HasSpanContext r)
  => InjectSelector (PeerSelector ps) s
  -> r
  -> UUID
  -> PeerReduced ps pr st m a
  -> Peer (Trace ps) pr ('StTrace st) m a
tracePeer inj initRef conversationId = \case
  YieldReduced (ClientAgency tok) msg peer -> Effect do
    let args = (simpleNewEventArgs Publish) { newEventCauses = [initRef] }
    withInjectEventArgs inj args \ev -> do
      addField ev $ ConversationId conversationId
      addField ev $ Message msg
      msgId <- liftIO nextRandom
      spanContext <- context $ reference ev
      addField ev $ MessageId msgId
      pure $ Yield
        (ClientAgency $ TokTraceClient tok)
        (MsgTrace (Just spanContext) msgId msg)
        (Effect $ tracePeer inj initRef conversationId <$> reducePeer peer)
  YieldReduced (ServerAgency tok) msg peer -> Effect do
    let args = (simpleNewEventArgs Publish) { newEventCauses = [initRef] }
    withInjectEventArgs inj args \ev -> do
      addField ev $ ConversationId conversationId
      addField ev $ Message msg
      msgId <- liftIO nextRandom
      spanContext <- context $ reference ev
      addField ev $ MessageId msgId
      pure $ Yield
        (ServerAgency $ TokTraceServer tok)
        (MsgTrace (Just spanContext) msgId msg)
        (Effect $ tracePeer inj initRef conversationId <$> reducePeer peer)
  AwaitReduced (ClientAgency tok) k ->
    Await (ClientAgency $ TokTraceClient tok) \case
      MsgTrace remoteRef msgId msg -> Effect $ localBackend (maybe id (setAncestorEventBackend . wrapContext) remoteRef) do
        let args = (simpleNewEventArgs Receive) { newEventCauses = [initRef] }
        withInjectEventArgs inj args \ev -> do
          addField ev $ ConversationId conversationId
          addField ev $ Message msg
          addField ev $ MessageId msgId
          withInjectEventFields inj Process [(conversationId, msgId)]
            $ const
            $ tracePeer inj initRef conversationId <$> reducePeer (k msg)
  AwaitReduced (ServerAgency tok) k ->
    Await (ServerAgency $ TokTraceServer tok) \case
      MsgTrace remoteRef msgId msg -> Effect $ localBackend (maybe id (setAncestorEventBackend . wrapContext) remoteRef) do
        let args = (simpleNewEventArgs Receive) { newEventCauses = [initRef] }
        withInjectEventArgs inj args \ev -> do
          addField ev $ ConversationId conversationId
          addField ev $ Message msg
          addField ev $ MessageId msgId
          withInjectEventFields inj Process [(conversationId, msgId)]
            $ const
            $ tracePeer inj initRef conversationId <$> reducePeer (k msg)
  DoneReduced tok a -> Done (TokTraceNobody tok) a

class OTelMessage ps where
  messageType :: Message ps st st' -> Text
  protocolName :: Proxy ps -> Text

renderPeerSelectorOTel :: forall ps. OTelMessage ps => Text -> Text -> RenderSelectorOTel (PeerSelector ps)
renderPeerSelectorOTel selfName peerName = \case
  Init -> OTelRendered
    { eventName = peerName <> " publish"
    , eventKind = Producer
    , renderField = \msgId ->
        [ ("messaging.system", "typed_protocols")
        , ("messaging.destination.name", toAttribute peerName)
        , ("messaging.source.name", toAttribute selfName)
        , ("messaging.operation", "publish")
        , ("messaging.message.id", toAttribute $ T.pack $ show msgId)
        , ("messaging.typed_protocols.message.type", "init")
        , ("messaging.message.conversation_id", toAttribute $ T.pack $ show msgId)
        , ("net.protocol.name", toAttribute $ protocolName $ Proxy @ps)
        ]
    }
  ReceiveInit -> OTelRendered
    { eventName = selfName <> " receive"
    , eventKind = Producer
    , renderField = \msgId ->
        [ ("messaging.system", "typed_protocols")
        , ("messaging.destination.name", toAttribute selfName)
        , ("messaging.source.name", toAttribute peerName)
        , ("messaging.operation", "receive")
        , ("messaging.message.id", toAttribute $ T.pack $ show msgId)
        , ("messaging.typed_protocols.message.type", "receive_init")
        , ("messaging.message.conversation_id", toAttribute $ T.pack $ show msgId)
        , ("net.protocol.name", toAttribute $ protocolName $ Proxy @ps)
        ]
    }
  Publish -> OTelRendered
    { eventName = peerName <> " publish"
    , eventKind = Producer
    , renderField = \case
        MessageId msgId ->
          [ ("messaging.system", "typed_protocols")
          , ("messaging.destination.name", toAttribute peerName)
          , ("messaging.source.name", toAttribute selfName)
          , ("messaging.operation", "publish")
          , ("messaging.message.id", toAttribute $ T.pack $ show msgId)
          , ("net.protocol.name", toAttribute $ protocolName $ Proxy @ps)
          ]
        ConversationId conversationId ->
          [ ("messaging.message.conversation_id", toAttribute $ T.pack $ show conversationId) ]
        Message msg ->
          [ ("messaging.typed_protocols.message.type", toAttribute $ messageType msg) ]
    }
  Receive -> OTelRendered
    { eventName = selfName <> " receive"
    , eventKind = Consumer
    , renderField = \case
        MessageId msgId ->
          [ ("messaging.system", "typed_protocols")
          , ("messaging.destination.name", toAttribute selfName)
          , ("messaging.source.name", toAttribute peerName)
          , ("messaging.operation", "receive")
          , ("messaging.message.id", toAttribute $ T.pack $ show msgId)
          , ("net.protocol.name", toAttribute $ protocolName $ Proxy @ps)
          ]
        ConversationId conversationId ->
          [ ("messaging.message.conversation_id", toAttribute $ T.pack $ show conversationId) ]
        Message msg ->
          [ ("messaging.typed_protocols.message.type", toAttribute $ messageType msg) ]
    }
  Process -> OTelRendered
    { eventName = selfName <> " process"
    , eventKind = Consumer
    , renderField = \(conversationId, msgId) ->
        [ ("messaging.system", "typed_protocols")
        , ("messaging.destination.name", toAttribute selfName)
        , ("messaging.source.name", toAttribute peerName)
        , ("messaging.operation", "process")
        , ("messaging.message.id", toAttribute $ T.pack $ show msgId)
        , ("messaging.typed_protocols.message.type", "init")
        , ("messaging.message.conversation_id", toAttribute $ T.pack $ show conversationId)
        , ("net.protocol.name", toAttribute $ protocolName $ Proxy @ps)
        ]
    }
