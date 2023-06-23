{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Network.Protocol.Driver.Trace where

import Colog (WithLog)
import qualified Colog as C
import Control.Concurrent.Component
import Control.Monad (join, replicateM)
import Control.Monad.Event.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Binary (Binary, get, getWord8, put)
import Data.Binary.Get (runGet)
import Data.Binary.Put (putWord8, runPut)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Base16 (encodeBase16)
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Proxy
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Void (Void)
import Network.Channel hiding (close)
import Network.Protocol.Codec (BinaryMessage, DeserializeError, decodeGet, getMessage, putMessage)
import Network.Protocol.Codec.Spec (ShowProtocol (..))
import Network.Protocol.Connection (
  Connection (..),
  Connector (..),
  DriverSelector (..),
  RecvMessageField (..),
  ServerSource (..),
  ToPeer,
 )
import Network.Protocol.Driver (TcpServerDependencies (..))
import Network.Protocol.Handshake.Client (handshakeClientPeer, simpleHandshakeClient)
import Network.Protocol.Handshake.Server (handshakeServerPeer, simpleHandshakeServer)
import Network.Protocol.Handshake.Types (Handshake, HasSignature, signature)
import Network.Protocol.Peer.Trace
import Network.Run.TCP (runTCPServer)
import Network.Socket (
  AddrInfo (..),
  AddrInfoFlag (..),
  HostName,
  PortNumber,
  SockAddr (..),
  SocketType (..),
  addrAddress,
  close,
  connect,
  defaultHints,
  getAddrInfo,
  getPeerName,
  hostAddress6ToTuple,
  hostAddressToTuple,
  openSocket,
 )
import qualified Network.Socket.ByteString.Lazy as Socket
import Network.TypedProtocol hiding (connect)
import Network.TypedProtocol.Codec
import Numeric (showHex)
import Observe.Event (Event (..), InjectSelector, NewEventArgs (..), addField, injectSelector, reference)
import Observe.Event.Backend (setAncestorEventBackend, simpleNewEventArgs)
import Observe.Event.Render.OpenTelemetry
import OpenTelemetry.Trace.Core
import OpenTelemetry.Trace.Id (SpanId, TraceId, bytesToSpanId, bytesToTraceId, spanIdBytes, traceIdBytes)
import OpenTelemetry.Trace.TraceState (Key (..), TraceState, Value (..), empty, insert, toList)
import UnliftIO (MonadUnliftIO, mask, throwIO, try, withRunInIO)

data DriverTraced ps dState r m = DriverTraced
  { sendMessageTraced
      :: forall pr st st'
       . r
      -> PeerHasAgency pr st
      -> Message ps st st'
      -> m ()
  , recvMessageTraced
      :: forall pr (st :: ps)
       . PeerHasAgency pr st
      -> dState
      -> m (r, SomeMessage st, dState)
  , startDStateTraced :: dState
  }

hoistDriverTraced :: (forall x. m x -> n x) -> DriverTraced ps dState r m -> DriverTraced ps dState r n
hoistDriverTraced f DriverTraced{..} =
  DriverTraced
    { sendMessageTraced = \r tok -> f . sendMessageTraced r tok
    , recvMessageTraced = \tok -> f . recvMessageTraced tok
    , ..
    }

runPeerWithDriverTraced
  :: forall ps dState pr st r s m a
   . (MonadEvent r s m)
  => InjectSelector (TypedProtocolsSelector ps) s
  -> DriverTraced ps dState r m
  -> PeerTraced ps pr st m a
  -> dState
  -> m a
runPeerWithDriverTraced inj driver peer dState = case peer of
  EffectTraced m -> flip (runPeerWithDriverTraced inj driver) dState =<< m
  YieldTraced tok msg yield -> runYieldPeerWithDriverTraced inj driver tok msg dState yield
  AwaitTraced tok k -> runAwaitPeerWithDriverTraced inj driver tok k dState
  DoneTraced _ a -> pure a

runYieldPeerWithDriverTraced
  :: (MonadEvent r s m)
  => InjectSelector (TypedProtocolsSelector ps) s
  -> DriverTraced ps dState r m
  -> WeHaveAgency pr st
  -> Message ps st st'
  -> dState
  -> YieldTraced ps pr st' m a
  -> m a
runYieldPeerWithDriverTraced inj driver tok msg dState = \case
  Call tok' k -> join $ withInjectEventFields inj (CallSelector tok msg) [()] \callEv -> do
    sendMessageTraced driver (reference callEv) tok msg
    (_, SomeMessage msg', dState') <- recvMessageTraced driver tok' dState
    pure $ runPeerWithDriverTraced inj driver (k msg') dState'
  Cast peer -> join $ withInjectEventFields inj (CastSelector tok msg) [()] \castEv -> do
    sendMessageTraced driver (reference castEv) tok msg
    pure $ runPeerWithDriverTraced inj driver peer dState
  Close _ a -> withInjectEventFields inj (CloseSelector tok msg) [()] \closeEv -> do
    sendMessageTraced driver (reference closeEv) tok msg
    pure a

runAwaitPeerWithDriverTraced
  :: forall ps pr st dState r s m a
   . (MonadEvent r s m)
  => InjectSelector (TypedProtocolsSelector ps) s
  -> DriverTraced ps dState r m
  -> TheyHaveAgency pr st
  -> (forall (st' :: ps). Message ps st st' -> AwaitTraced ps pr st' m a)
  -> dState
  -> m a
runAwaitPeerWithDriverTraced inj driver tok k dState = do
  (sendRef, SomeMessage msg, dState') <- recvMessageTraced driver tok dState
  case k msg of
    Respond tok' m -> join $ withInjectEventArgs inj (respondArgs sendRef tok msg) \respondEv -> do
      Response msg' nextPeer <- m
      sendMessageTraced driver (reference respondEv) tok' msg'
      pure $ runPeerWithDriverTraced inj driver nextPeer dState'
    Receive nextPeer -> join $ withInjectEventArgs inj (receiveArgs sendRef tok msg) $ const $ receive dState' nextPeer
    Closed _ ma -> withInjectEventArgs inj (closeArgs sendRef tok msg) $ const ma
  where
    receive :: dState -> PeerTraced ps pr st' m a -> m (m a)
    receive dState' = \case
      EffectTraced m -> receive dState' =<< m
      peer -> pure $ runPeerWithDriverTraced inj driver peer dState'
    respondArgs sendRef tok' msg =
      (simpleNewEventArgs (RespondSelector tok' msg))
        { newEventParent = Just sendRef
        , newEventInitialFields = [()]
        }
    receiveArgs sendRef tok' msg =
      (simpleNewEventArgs (ReceiveSelector tok' msg))
        { newEventParent = Just sendRef
        , newEventInitialFields = [()]
        }
    closeArgs sendRef tok' msg =
      (simpleNewEventArgs (CloseSelector tok' msg))
        { newEventParent = Just sendRef
        , newEventInitialFields = [()]
        }

data TcpClientSelector ps f where
  Connect :: TcpClientSelector ps AddrInfo
  ClientPeer
    :: AddrInfo
    -> TypedProtocolsSelector ps f
    -> TcpClientSelector ps f
  CloseClient :: TcpClientSelector ps Void
  ClientDriver
    :: AddrInfo
    -> DriverSelector ps f
    -> TcpClientSelector ps f

data TcpServerSelector ps f where
  Connected :: TcpServerSelector ps ConnectedField
  ServerPeer
    :: AddrInfo
    -> SockAddr
    -> TypedProtocolsSelector ps f
    -> TcpServerSelector ps f
  CloseServer :: TcpServerSelector ps Void
  ServerDriver
    :: AddrInfo
    -> SockAddr
    -> DriverSelector ps f
    -> TcpServerSelector ps f

data ConnectedField
  = ConnectedAddr AddrInfo
  | ConnectedPeer SockAddr

tcpServerTraced
  :: forall r s env m ps server
   . ( MonadUnliftIO m
     , MonadEvent r s m
     , HasSpanContext r
     , BinaryMessage ps
     , MonadFail m
     , HasSignature ps
     , WithLog env C.Message m
     )
  => String
  -> InjectSelector (TcpServerSelector (Handshake ps)) s
  -> Component m (TcpServerDependencies ps server m) ()
tcpServerTraced name inj = component_ (name <> "-tcp-server") \TcpServerDependencies{..} -> do
  withRunInIO \runInIO -> runTCPServer (Just host) (show port) \socket -> runInIO $ runResourceT do
    spanContextLength <- liftIO $ runGet get <$> Socket.recv socket 8
    spanContext <- liftIO $ runGet get <$> Socket.recv socket spanContextLength
    let parentRef = wrapContext spanContext
    withInjectEventArgs inj (simpleNewEventArgs Connected){newEventParent = Just parentRef} \ev -> do
      addr <-
        liftIO $
          head
            <$> getAddrInfo
              (Just defaultHints{addrSocketType = Stream, addrFlags = [AI_PASSIVE]})
              (Just host)
              (Just $ show port)
      addField ev $ ConnectedAddr addr
      pName <- liftIO $ getPeerName socket
      addField ev $ ConnectedPeer pName
      _ <- liftIO $ Socket.sendAll socket $ LBS.pack [0]
      let closeArgs = (simpleNewEventArgs CloseServer){newEventParent = Just parentRef}
      server <- getServer serverSource
      lift $ localBackend (setAncestorEventBackend parentRef) do
        let driver =
              mkDriverTraced
                (composeInjectSelector inj $ injectSelector $ ServerDriver addr pName)
                (socketAsChannel socket)
            handshakeServer = simpleHandshakeServer (signature $ Proxy @ps) server
            peer = handshakeServerPeer toPeer handshakeServer
        mask \restore -> do
          result <-
            restore $
              try $
                runPeerWithDriverTraced
                  (composeInjectSelector inj $ injectSelector $ ServerPeer addr pName)
                  driver
                  peer
                  (startDStateTraced driver)
          withInjectEventArgs inj closeArgs \ev' -> do
            case result of
              Left ex -> do
                finalize ev' $ Just ex
                throwIO ex
              Right a -> pure a

tcpClientTraced
  :: forall r s m ps st client
   . (MonadUnliftIO m, MonadEvent r s m, HasSpanContext r, BinaryMessage ps, MonadFail m, HasSignature ps)
  => InjectSelector (TcpClientSelector (Handshake ps)) s
  -> HostName
  -> PortNumber
  -> ToPeer client ps 'AsClient st m
  -> Connector client m
tcpClientTraced inj host port toPeer = Connector $
  withInjectEvent inj Connect \ev -> do
    addr <-
      liftIO $
        head
          <$> getAddrInfo
            (Just defaultHints{addrSocketType = Stream})
            (Just host)
            (Just $ show port)
    addField ev addr
    socket <- liftIO $ openSocket addr
    liftIO $ connect socket $ addrAddress addr
    spanContext <- context $ reference ev
    let spanContextBytes = runPut $ put spanContext
    let spanContextLength = LBS.length spanContextBytes
    liftIO $ Socket.sendAll socket $ runPut $ put spanContextLength
    liftIO $ Socket.sendAll socket spanContextBytes
    _ <- liftIO $ Socket.recv socket 1
    let closeArgs = (simpleNewEventArgs CloseClient){newEventParent = Just $ reference ev}
    pure
      Connection
        { runConnection = \client -> localBackend (setAncestorEventBackend $ reference ev) do
            let driver =
                  mkDriverTraced
                    (composeInjectSelector inj $ injectSelector $ ClientDriver addr)
                    (socketAsChannel socket)
                handshakeClient = simpleHandshakeClient (signature $ Proxy @ps) client
                peer = handshakeClientPeer toPeer handshakeClient
            mask \restore -> do
              result <-
                restore $
                  try $
                    runPeerWithDriverTraced
                      (composeInjectSelector inj $ injectSelector $ ClientPeer addr)
                      driver
                      peer
                      (startDStateTraced driver)
              withInjectEventArgs inj closeArgs \ev' -> do
                liftIO $ close socket
                case result of
                  Left ex -> do
                    finalize ev' $ Just ex
                    throwIO ex
                  Right a -> pure a
        }

class HasSpanContext r where
  context :: (MonadIO m) => r -> m SpanContext
  wrapContext :: SpanContext -> r

instance HasSpanContext Span where
  context = getSpanContext
  wrapContext = wrapSpanContext

instance (Monoid b, HasSpanContext a) => HasSpanContext (a, b) where
  context = context . fst
  wrapContext = (,mempty) . wrapContext

mkDriverTraced
  :: forall ps r s m
   . (MonadIO m, BinaryMessage ps, HasSpanContext r, MonadEvent r s m)
  => InjectSelector (DriverSelector ps) s
  -> Channel m ByteString
  -> DriverTraced ps (Maybe ByteString) r m
mkDriverTraced inj Channel{..} = DriverTraced{..}
  where
    sendMessageTraced
      :: forall (pr :: PeerRole) (st :: ps) (st' :: ps)
       . r
      -> PeerHasAgency pr st
      -> Message ps st st'
      -> m ()
    sendMessageTraced r tok msg = withInjectEventFields inj (SendMessage tok msg) [()] \ev -> do
      spanContext <- context r
      addField ev =<< send (runPut $ put spanContext *> putMessage tok msg)

    recvMessageTraced
      :: forall (pr :: PeerRole) (st :: ps)
       . PeerHasAgency pr st
      -> Maybe ByteString
      -> m (r, SomeMessage st, Maybe ByteString)
    recvMessageTraced tok trailing = do
      let
      (ctx, trailing') <- decodeChannel trailing =<< decodeGet get
      let r = wrapContext ctx
      let args =
            (simpleNewEventArgs $ RecvMessage tok)
              { newEventParent = Just r
              , newEventInitialFields =
                  [ RecvMessageStateBeforeSpan trailing
                  , RecvMessageStateBeforeMessage trailing'
                  ]
              }
      withInjectEventArgs inj args \ev -> do
        (SomeMessage msg, trailing'') <- decodeChannel trailing' =<< decodeGet (getMessage tok)
        addField ev $ RecvMessageStateAfterMessage trailing''
        addField ev $ RecvMessageMessage msg
        pure (r, SomeMessage msg, trailing'')

    decodeChannel
      :: Maybe ByteString
      -> DecodeStep ByteString DeserializeError m a
      -> m (a, Maybe ByteString)
    decodeChannel trailing (DecodeDone a _) = pure (a, trailing)
    decodeChannel _ (DecodeFail failure) = throwIO failure
    decodeChannel trailing (DecodePartial p) =
      case trailing of
        Nothing -> go $ DecodePartial p
        Just trailing' -> go =<< p (Just trailing')
      where
        go = \case
          DecodeDone a Nothing -> pure (a, Nothing)
          DecodeDone a (Just trailing') -> do
            pure (a, Just trailing')
          DecodeFail failure -> throwIO failure
          DecodePartial next -> do
            mBytes <- recv
            nextStep <- next mBytes
            go nextStep

    startDStateTraced :: Maybe ByteString
    startDStateTraced = Nothing

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

renderTcpClientSelectorOTel
  :: forall ps. (OTelProtocol ps, ShowProtocol ps) => RenderSelectorOTel (TcpClientSelector ps)
renderTcpClientSelectorOTel = \case
  Connect ->
    OTelRendered
      { eventName = "tcp/connect " <> protocolName (Proxy @ps)
      , eventKind = Producer
      , renderField = \addr ->
          ("net.protocol.name", toAttribute $ protocolName (Proxy @ps)) : addrInfoToAttributes addr
      }
  ClientPeer addr sel -> case renderTypedProtocolSelectorOTel sel of
    OTelRendered{..} ->
      OTelRendered
        { renderField = \f ->
            (("net.protocol.name", toAttribute $ protocolName (Proxy @ps)) : addrInfoToAttributes addr)
              <> renderField f
        , ..
        }
  CloseClient ->
    OTelRendered
      { eventName = "tcp/close"
      , eventKind = Producer
      , renderField = \case {}
      }
  ClientDriver addr sel -> case renderDriverSelectorOTel sel of
    OTelRendered{..} ->
      OTelRendered
        { renderField = \f ->
            (("net.protocol.name", toAttribute $ protocolName (Proxy @ps)) : addrInfoToAttributes addr)
              <> renderField f
        , ..
        }

renderTcpServerSelectorOTel
  :: forall ps. (OTelProtocol ps, ShowProtocol ps) => RenderSelectorOTel (TcpServerSelector ps)
renderTcpServerSelectorOTel = \case
  Connected ->
    OTelRendered
      { eventName = "tcp/connected " <> protocolName (Proxy @ps)
      , eventKind = Consumer
      , renderField = \case
          ConnectedAddr addr -> ("net.protocol.name", toAttribute $ protocolName (Proxy @ps)) : addrInfoToAttributes addr
          ConnectedPeer peer -> sockAddrToAttributes True peer
      }
  ServerPeer addr peer sel -> case renderTypedProtocolSelectorOTel sel of
    OTelRendered{..} ->
      OTelRendered
        { renderField = \f ->
            (("net.protocol.name", toAttribute $ protocolName (Proxy @ps)) : addrInfoToAttributes addr)
              <> sockAddrToAttributes True peer
              <> renderField f
        , ..
        }
  CloseServer ->
    OTelRendered
      { eventName = "tcp/close"
      , eventKind = Producer
      , renderField = \case {}
      }
  ServerDriver addr peer sel -> case renderDriverSelectorOTel sel of
    OTelRendered{..} ->
      OTelRendered
        { renderField = \f ->
            (("net.protocol.name", toAttribute $ protocolName (Proxy @ps)) : addrInfoToAttributes addr)
              <> sockAddrToAttributes True peer
              <> renderField f
        , ..
        }

renderDriverSelectorOTel :: (ShowProtocol ps, OTelProtocol ps) => RenderSelectorOTel (DriverSelector ps)
renderDriverSelectorOTel = \case
  SendMessage tok msg ->
    OTelRendered
      { eventName = "send_message " <> messageType (messageAttributes tok msg)
      , eventKind = Producer
      , renderField = const $ messageToAttributes $ AnyMessageAndAgency tok msg
      }
  RecvMessage tok ->
    OTelRendered
      { eventName = "recv_message"
      , eventKind = Consumer
      , renderField = \case
          RecvMessageStateBeforeSpan state ->
            [
              ( "typed-protocols.state"
              , fromString case tok of
                  ClientAgency tok' -> showsPrecClientHasAgency 0 tok' ""
                  ServerAgency tok' -> showsPrecServerHasAgency 0 tok' ""
              )
            , ("typed-protocols.driver_state_before_span", toAttribute $ TL.toStrict $ foldMap encodeBase16 state)
            ]
          RecvMessageStateBeforeMessage state -> [("typed-protocols.driver_state_before_message", toAttribute $ TL.toStrict $ foldMap encodeBase16 state)]
          RecvMessageStateAfterMessage state -> [("typed-protocols.driver_state_after_message", toAttribute $ TL.toStrict $ foldMap encodeBase16 state)]
          RecvMessageMessage msg -> messageToAttributes $ AnyMessageAndAgency tok msg
      }

messageToAttributes :: (OTelProtocol ps, ShowProtocol ps) => AnyMessageAndAgency ps -> [(Text, Attribute)]
messageToAttributes (AnyMessageAndAgency tok msg) = case messageAttributes tok msg of
  MessageAttributes{..} ->
    [ ("typed-protocols.message.type", toAttribute messageType)
    , ("typed-protocols.message.parameters", toAttribute messageParameters)
    ,
      ( "typed-protocols.state"
      , fromString case tok of
          ClientAgency tok' -> showsPrecClientHasAgency 0 tok' ""
          ServerAgency tok' -> showsPrecServerHasAgency 0 tok' ""
      )
    ]

addrInfoToAttributes :: AddrInfo -> [(T.Text, Attribute)]
addrInfoToAttributes AddrInfo{..} =
  ( "net.transport"
  , case addrSocketType of
      Stream -> "ip_tcp"
      Datagram -> "ip_udp"
      _ -> "other"
  )
    : sockAddrToAttributes False addrAddress

sockAddrToAttributes :: Bool -> SockAddr -> [(T.Text, Attribute)]
sockAddrToAttributes isRemote = \case
  SockAddrInet port (hostAddressToTuple -> (a, b, c, d)) ->
    [ (netPrefix <> "name", fromString $ intercalate "." $ show <$> [a, b, c, d])
    , (netPrefix <> "port", toAttribute @Int64 $ fromIntegral port)
    ]
  SockAddrInet6 port _ (hostAddress6ToTuple -> (a, b, c, d, e, f, g, h)) _ ->
    [ (netPrefix <> "name", fromString $ intercalate ":" $ flip showHex "" <$> [a, b, c, d, e, f, g, h])
    , (netPrefix <> "port", toAttribute @Int64 $ fromIntegral port)
    , ("net.sock.family", "inet6")
    , (sockPrefix <> "addr", fromString $ intercalate ":" $ flip showHex "" <$> [a, b, c, d, e, f, g, h])
    , (sockPrefix <> "port", toAttribute @Int64 $ fromIntegral port)
    ]
  SockAddrUnix name ->
    [ (netPrefix <> "name", fromString name)
    , ("net.sock.family", "unix")
    ]
  where
    netPrefix
      | isRemote = "net.peer."
      | otherwise = "net.host."
    sockPrefix
      | isRemote = "net.sock.peer."
      | otherwise = "net.sock.host."

peerName :: SockAddr -> Text
peerName = \case
  SockAddrInet _ (hostAddressToTuple -> (a, b, c, d)) -> fromString $ intercalate "." $ show <$> [a, b, c, d]
  SockAddrInet6 _ _ (hostAddress6ToTuple -> (a, b, c, d, e, f, g, h)) _ -> fromString $ intercalate ":" $ flip showHex "" <$> [a, b, c, d, e, f, g, h]
  SockAddrUnix name -> fromString name
