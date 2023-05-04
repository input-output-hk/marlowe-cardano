{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Protocol.Driver
  where

import Control.Concurrent.Component
import Control.Concurrent.STM (newEmptyTMVar, newTQueue, readTMVar, readTQueue, tryPutTMVar)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Monad.Event.Class
import Control.Monad.IO.Class (liftIO)
import Data.Binary (get, put)
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Functor (void)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Proxy
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Network.Channel (Channel(..), socketAsChannel)
import Network.Protocol.Codec (BinaryMessage, DeserializeError, binaryCodec)
import Network.Protocol.Connection
  ( Connection(..)
  , ConnectionSource(..)
  , ConnectionSourceTraced(..)
  , ConnectionTraced(..)
  , Connector(..)
  , ConnectorTraced(..)
  , SomeConnector(..)
  , SomeConnectorTraced(..)
  , ToPeer
  , ToPeerTraced
  )
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Peer.Trace
import Network.Run.TCP (runTCPServer)
import Network.Socket
  ( AddrInfo(..)
  , AddrInfoFlag(..)
  , HostName
  , PortNumber
  , SockAddr(..)
  , SocketType(..)
  , addrAddress
  , close
  , connect
  , defaultHints
  , getAddrInfo
  , getPeerName
  , hostAddress6ToTuple
  , hostAddressToTuple
  , openSocket
  )
import qualified Network.Socket.ByteString.Lazy as Socket
import Network.TypedProtocol (Message, PeerHasAgency, PeerRole(..), SomeMessage(..), runPeerWithDriver)
import Network.TypedProtocol.Codec (Codec(..), DecodeStep(..))
import Network.TypedProtocol.Driver (Driver(..))
import Numeric (showHex)
import Observe.Event (Event(..), InjectSelector, NewEventArgs(..), addField, injectSelector)
import Observe.Event.Backend (setAncestorEventBackend, simpleNewEventArgs)
import Observe.Event.Render.OpenTelemetry
import OpenTelemetry.Trace.Core (Attribute, SpanKind(..), toAttribute)
import UnliftIO (MonadIO, MonadUnliftIO, atomically, mask, throwIO, try)

mkDriver
  :: forall ps m
   . (MonadIO m, BinaryMessage ps)
  => Channel m ByteString
  -> Driver ps (Maybe ByteString) m
mkDriver  Channel{..} = Driver{..}
  where
    Codec{..} = binaryCodec
    sendMessage
      :: forall (pr :: PeerRole) (st :: ps) (st' :: ps)
       . PeerHasAgency pr st
      -> Message ps st st'
      -> m ()
    sendMessage tok = send . encode tok

    recvMessage
      :: forall (pr :: PeerRole) (st :: ps)
       . PeerHasAgency pr st
      -> Maybe ByteString
      -> m (SomeMessage st, Maybe ByteString)
    recvMessage tok trailing = decodeChannel trailing =<< decode tok

    decodeChannel
      :: Maybe ByteString
      -> DecodeStep ByteString DeserializeError m a
      -> m (a, Maybe ByteString)
    decodeChannel _ (DecodeDone a trailing)     = pure (a, trailing)
    decodeChannel _ (DecodeFail failure)        = throwIO failure
    decodeChannel Nothing (DecodePartial next)  = recv >>= next >>= decodeChannel Nothing
    decodeChannel trailing (DecodePartial next) = next trailing >>= decodeChannel Nothing

    startDState :: Maybe ByteString
    startDState = Nothing

hoistDriver :: (forall x. m x -> n x) -> Driver ps dState m -> Driver ps dState n
hoistDriver f Driver{..} = Driver
  { sendMessage = fmap f . sendMessage
  , recvMessage = fmap f . recvMessage
  , ..
  }

data TcpServerDependencies ps server m = forall (st :: ps). TcpServerDependencies
  { host :: HostName
  , port :: PortNumber
  , toPeer :: ToPeer server ps 'AsServer st m
  }

tcpServer
  :: (MonadIO m', MonadIO m)
  => Component m (TcpServerDependencies ps server m') (ConnectionSource ps server m')
tcpServer = component \TcpServerDependencies{..} -> do
  socketQueue <- newTQueue
  pure
    ( liftIO $ runTCPServer (Just host) (show port) \socket -> do
        closeTMVar <- atomically do
          closeTMVar <- newEmptyTMVar
          writeTQueue socketQueue (socket, void $ tryPutTMVar closeTMVar ())
          pure closeTMVar
        atomically $ readTMVar closeTMVar
    , ConnectionSource do
        (socket, closeConnection) <- readTQueue socketQueue
        pure $ Connector $ pure Connection
          { closeConnection = \_ -> atomically closeConnection
          , channel = socketAsChannel socket
          , ..
          }
    )

tcpClient
  :: MonadIO m
  => HostName
  -> PortNumber
  -> ToPeer client ps 'AsClient st m
  -> Connector ps 'AsClient client m
tcpClient host port toPeer = Connector $ liftIO $ do
  addr <- head <$> getAddrInfo
    (Just defaultHints { addrSocketType = Stream })
    (Just host)
    (Just $ show port)
  socket <- openSocket addr
  connect socket $ addrAddress addr
  pure Connection
    { closeConnection = \_ -> liftIO $ close socket
    , channel = socketAsChannel socket
    , ..
    }

data TcpClientSelector ps f where
  Connect :: TcpClientSelector ps AddrInfo
  ClientPeer
    :: AddrInfo
    -> TypedProtocolsSelector ps f
    -> TcpClientSelector ps f
  CloseClient :: TcpClientSelector ps Void

data TcpServerSelector ps f where
  Connected :: TcpServerSelector ps ConnectedField
  ServerPeer
    :: AddrInfo
    -> SockAddr
    -> TypedProtocolsSelector ps f
    -> TcpServerSelector ps f
  CloseServer :: TcpServerSelector ps Void

data ConnectedField
  = ConnectedAddr AddrInfo
  | ConnectedPeer SockAddr

data TcpServerDependenciesTraced ps server r m = forall (st :: ps). TcpServerDependenciesTraced
  { host :: HostName
  , port :: PortNumber
  , toPeer :: ToPeerTraced server ps 'AsServer st r m
  }

tcpServerTraced
  :: (MonadIO m', MonadIO m, MonadEvent r s m', HasSpanContext r)
  => InjectSelector (TcpServerSelector (Handshake ps)) s
  -> Component m (TcpServerDependenciesTraced ps server r m') (ConnectionSourceTraced ps server r TcpServerSelector m')
tcpServerTraced inj = component \TcpServerDependenciesTraced{..} -> do
  socketQueue <- newTQueue
  pure
    ( liftIO $ runTCPServer (Just host) (show port) \socket -> do
        closeTMVar <- atomically do
          closeTMVar <- newEmptyTMVar
          writeTQueue socketQueue (socket, void $ tryPutTMVar closeTMVar ())
          pure closeTMVar
        atomically $ readTMVar closeTMVar
    , ConnectionSourceTraced do
        (socket, closeConnection) <- readTQueue socketQueue
        pure $ ConnectorTraced do
          spanContextLength <- liftIO $ runGet get <$> Socket.recv socket 8
          spanContext <- liftIO $ runGet get <$> Socket.recv socket spanContextLength
          let parentRef = wrapContext spanContext
          withInjectEventArgs inj (simpleNewEventArgs Connected) { newEventParent = Just parentRef } \ev -> do
            addr <- liftIO $ head <$> getAddrInfo
              (Just defaultHints { addrSocketType = Stream, addrFlags = [AI_PASSIVE] })
              (Just host)
              (Just $ show port)
            addField ev $ ConnectedAddr addr
            peer <- liftIO $ getPeerName socket
            addField ev $ ConnectedPeer peer
            _ <- liftIO $ Socket.sendAll socket $ LBS.pack [0]
            pure ConnectionTraced
              { closeConnection = \_ -> atomically closeConnection
              , channel = socketAsChannel socket
              , injectProtocolSelector = injectSelector $ ServerPeer addr peer
              , openRef = parentRef
              , ..
              }
    )

tcpClientTraced
  :: (MonadIO m, MonadEvent r s m, HasSpanContext r)
  => InjectSelector (TcpClientSelector (Handshake ps)) s
  -> HostName
  -> PortNumber
  -> ToPeerTraced client ps 'AsClient st r m
  -> ConnectorTraced ps 'AsClient client r TcpClientSelector m
tcpClientTraced inj host port toPeer = ConnectorTraced
  $ withInjectEvent inj Connect \ev -> do
    addr <- liftIO $ head <$> getAddrInfo
      (Just defaultHints { addrSocketType = Stream })
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
    let closeArgs = (simpleNewEventArgs CloseClient) { newEventParent = Just $ reference ev }
    pure ConnectionTraced
      { closeConnection = \ex -> withInjectEventArgs inj closeArgs \ev' -> do
          liftIO $ close socket
          finalize ev' ex
      , channel = socketAsChannel socket
      , openRef = reference ev
      , injectProtocolSelector = injectSelector $ ClientPeer addr
      , ..
      }

runConnectionTraced
  :: (MonadUnliftIO m, BinaryMessage ps, MonadEvent r s m, HasSpanContext r)
  => InjectSelector (s' ps) s
  -> ConnectionTraced ps pr peer r s' m
  -> peer m a
  -> m a
runConnectionTraced inj ConnectionTraced{..} peer = localBackend (setAncestorEventBackend openRef) do
  let driver = mkDriverTraced channel
  mask \restore -> do
    result <- try $ restore $ runPeerWithDriverTraced (composeInjectSelector inj injectProtocolSelector) driver (toPeer peer) (startDStateTraced driver)
    case result of
      Left ex -> do
        closeConnection $ Just ex
        throwIO ex
      Right a -> do
        closeConnection Nothing
        pure a

runConnection :: (MonadUnliftIO m, BinaryMessage ps) => Connection ps pr peer m -> peer m a -> m a
runConnection Connection{..} peer = do
  let driver = mkDriver channel
  mask \restore -> do
    result <- try $ restore $ runPeerWithDriver driver (toPeer peer) (startDState driver)
    case result of
      Left ex -> do
        closeConnection $ Just ex
        throwIO ex
      Right (a, _) -> do
        closeConnection Nothing
        pure a

runConnector :: (MonadUnliftIO m, BinaryMessage ps) => Connector ps pr peer m -> peer m a -> m a
runConnector Connector{..} peer = flip runConnection peer =<< openConnection

runConnectorTraced
  :: (MonadUnliftIO m, BinaryMessage ps, MonadEvent r s m, HasSpanContext r)
  => InjectSelector (s' ps) s
  -> ConnectorTraced ps pr peer r s' m
  -> peer m a
  -> m a
runConnectorTraced inj ConnectorTraced{..} peer = flip (runConnectionTraced inj) peer =<< openConnectionTraced

runSomeConnector :: MonadUnliftIO m => SomeConnector pr peer m -> peer m a -> m a
runSomeConnector (SomeConnector connector) = runConnector connector

runSomeConnectorTraced
  :: (MonadUnliftIO m, MonadEvent r s m, HasSpanContext r)
  => SomeConnectorTraced pr peer r s m
  -> peer m a
  -> m a
runSomeConnectorTraced (SomeConnectorTraced inj connector) = runConnectorTraced inj connector

renderTcpClientSelectorOTel :: forall ps. OTelProtocol ps => RenderSelectorOTel (TcpClientSelector ps)
renderTcpClientSelectorOTel = \case
  Connect -> OTelRendered
    { eventName = "tcp/connect " <> protocolName (Proxy @ps)
    , eventKind = Producer
    , renderField = \addr ->
        ("net.protocol.name", toAttribute $ protocolName (Proxy @ps)) : addrInfoToAttributes addr
    }
  ClientPeer addr sel -> case renderTypedProtocolSelectorOTel sel of
    OTelRendered{..} -> OTelRendered
      { renderField = \f ->
          (("net.protocol.name", toAttribute $ protocolName (Proxy @ps)) : addrInfoToAttributes addr)
            <> renderField f
      , ..
      }
  CloseClient -> OTelRendered
    { eventName = "tcp/close"
    , eventKind = Producer
    , renderField = \case
    }

renderTcpServerSelectorOTel :: forall ps. OTelProtocol ps => RenderSelectorOTel (TcpServerSelector ps)
renderTcpServerSelectorOTel = \case
  Connected -> OTelRendered
    { eventName = "tcp/connected " <> protocolName (Proxy @ps)
    , eventKind = Consumer
    , renderField = \case
        ConnectedAddr addr -> ("net.protocol.name", toAttribute $ protocolName (Proxy @ps)) : addrInfoToAttributes addr
        ConnectedPeer peer -> sockAddrToAttributes True peer
    }
  ServerPeer addr peer sel -> case renderTypedProtocolSelectorOTel sel of
    OTelRendered{..} -> OTelRendered
      { renderField = \f ->
          (("net.protocol.name", toAttribute $ protocolName (Proxy @ps)) : addrInfoToAttributes addr)
            <> sockAddrToAttributes True peer
            <> renderField f
      , ..
      }
  CloseServer -> OTelRendered
    { eventName = "tcp/close"
    , eventKind = Producer
    , renderField = \case
    }

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
