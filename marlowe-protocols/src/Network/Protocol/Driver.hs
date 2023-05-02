{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Network.Protocol.Driver
  where

import Control.Concurrent.Component
import Control.Concurrent.STM (newEmptyTMVar, newTQueue, readTMVar, readTQueue, tryPutTMVar)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Monad.Event.Class
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Functor (void)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Channel (Channel(..), socketAsChannel)
import Network.Protocol.Codec (BinaryMessage, DeserializeError, binaryCodec)
import Network.Protocol.Connection
  ( Connection(..)
  , ConnectionSource(..)
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
  , SocketOption(ReuseAddr)
  , SocketType(..)
  , accept
  , addrAddress
  , close
  , connect
  , defaultHints
  , getAddrInfo
  , gracefulClose
  , hostAddress6ToTuple
  , hostAddressToTuple
  , listen
  , openSocket
  , setCloseOnExecIfNeeded
  , setSocketOption
  , withFdSocket
  , withSocketsDo
  )
import Network.Socket.Address (bind)
import Network.TypedProtocol (Message, PeerHasAgency, PeerRole(..), SomeMessage(..), runPeerWithDriver)
import Network.TypedProtocol.Codec (Codec(..), DecodeStep(..))
import Network.TypedProtocol.Driver (Driver(..))
import Numeric (showHex)
import Observe.Event (Event(finalize, reference), InjectSelector, NewEventArgs(..), addField, injectSelector)
import Observe.Event.Backend (simpleNewEventArgs)
import Observe.Event.Render.OpenTelemetry
import OpenTelemetry.Trace.Core (Attribute, SpanKind(..), toAttribute)
import UnliftIO
  (MonadIO, MonadUnliftIO, atomically, bracket, bracketOnError, catch, concurrently, mask, throwIO, try, withRunInIO)

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

type InjectProtocolSelector s = forall ps. InjectSelector (TypedProtocolsSelector ps) (s ps)

data TcpClientSelector ps f where
  Connect :: TcpClientSelector ps ConnectField
  ClientPeer
    :: HostName
    -> PortNumber
    -> AddrInfo
    -> TypedProtocolsSelector ps f
    -> TcpClientSelector ps f

data ConnectField
  = ConnectHost HostName
  | ConnectPort PortNumber

tcpClientTraced
  :: (MonadIO m, MonadEvent r s m)
  => InjectSelector (TcpClientSelector (Handshake ps)) s
  -> HostName
  -> PortNumber
  -> ToPeerTraced client ps 'AsClient st r m
  -> ConnectorTraced ps 'AsClient client r TcpClientSelector m
tcpClientTraced inj host port toPeer = ConnectorTraced
  $ withInjectEventFields inj Connect [ConnectHost host, ConnectPort port] \ev -> do
    addr <- liftIO $ head <$> getAddrInfo
      (Just defaultHints { addrSocketType = Stream })
      (Just host)
      (Just $ show port)
    socket <- liftIO $ openSocket addr
    liftIO $ connect socket $ addrAddress addr
    pure ConnectionTraced
      { closeConnection = \_ -> liftIO $ close socket
      , channel = socketAsChannel socket
      , openRef = reference ev
      , injectProtocolSelector = injectSelector $ ClientPeer host port addr
      , ..
      }

runConnectionTraced
  :: (MonadUnliftIO m, BinaryMessage ps, MonadEvent r s m, HasSpanContext r)
  => InjectSelector (s' ps) s
  -> ConnectionTraced ps pr peer r s' m
  -> peer m a
  -> m a
runConnectionTraced inj ConnectionTraced{..} peer = do
  let driver = mkDriverTraced channel
  mask \restore -> do
    result <- try $ restore $ runPeerWithDriverTraced (composeInjectSelector inj injectProtocolSelector) openRef driver (toPeer peer) (startDStateTraced driver)
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

runSomeConnector :: MonadUnliftIO m => SomeConnector pr peer r m -> peer m a -> m a
runSomeConnector (SomeConnector connector) = runConnector connector

runSomeConnectorTraced
  :: (MonadUnliftIO m, MonadEvent r s m, HasSpanContext r)
  => SomeConnectorTraced pr peer r s m
  -> peer m a
  -> m a
runSomeConnectorTraced (SomeConnectorTraced inj connector) = runConnectorTraced inj connector

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
