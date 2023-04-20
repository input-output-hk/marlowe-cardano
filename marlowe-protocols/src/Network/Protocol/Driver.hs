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

module Network.Protocol.Driver
  where

import Control.Concurrent.Component
import Control.Concurrent.STM (newEmptyTMVar, newTQueue, readTMVar, readTQueue, tryPutTMVar)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Monad (join)
import Control.Monad.Event.Class
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
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
import Network.Protocol.Connection (Connection(..), ConnectionSource(..), Connector(..), SomeConnector(..), ToPeer)
import Network.Protocol.Handshake.Server
  ( HandshakeServerSelector
  , handshakeServerPeer
  , renderHandshakeServerSelectorOTel
  , simpleHandshakeServer
  , traceHandshakeServer
  )
import Network.Protocol.Handshake.Types (Handshake, HasSignature, signature)
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
import Observe.Event (Event(finalize, reference), InjectSelector, NewEventArgs(..), addField)
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

tcpServer :: (MonadIO m', MonadIO m) => Component m (TcpServerDependencies ps server m') (ConnectionSource ps server m')
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

data TcpServerSelector server f where
  SetupServer :: TcpServerSelector server SetupServerField
  ServerResolve :: TcpServerSelector server AddrInfo
  ServerBind :: TcpServerSelector server Void
  ServerClose :: TcpServerSelector server Void
  ServerAccept :: TcpServerSelector server SockAddr
  ServerCloseConnection :: TcpServerSelector server SockAddr
  LiftServer :: HandshakeServerSelector server f -> TcpServerSelector server (LiftServerField f)

injectServer :: InjectSelector (HandshakeServerSelector server) (TcpServerSelector server)
injectServer sel withInjField = withInjField (LiftServer sel) LiftServerApp

data LiftServerField f
  = LiftServerHost AddrInfo
  | LiftServerPeer SockAddr
  | LiftServerApp f

data SetupServerField
  = ServerHost HostName
  | ServerPort PortNumber

tcpServerTrace
  :: forall ps server r s t m m'
   . ( MonadUnliftIO m'
     , MonadUnliftIO m
     , MonadInjectEvent r (TcpServerSelector s) t m
     , MonadInjectEvent r (TcpServerSelector s) t m'
     , HasSignature ps
     , MonadFail m'
     )
  => (forall a. InjectSelector s t -> r -> server m' a -> server m' a)
  -> Component m (TcpServerDependencies ps server m') (ConnectionSource (Handshake ps) server m')
tcpServerTrace traceServer = component \TcpServerDependencies{..} -> do
  socketQueue <- newTQueue
  pure
    ( withRunInIO \runInIO -> withSocketsDo $ runInIO do
        join $ withEventFields (SetupServer @s) [ServerHost host, ServerPort port] \setupEv -> do
          let hints = defaultHints { addrSocketType = Stream, addrFlags = [AI_PASSIVE] }
          addr <- withEvent (ServerResolve @s) \ev -> do
            addr <- liftIO $ head <$> getAddrInfo (Just hints) (Just host) (Just $ show port)
            addField ev addr
            pure addr
          -- pure / join is so that the server loop is not nested within the
          -- setup span
          socket <- open setupEv addr
          pure $ bracket (pure socket) (close' setupEv) $ loop setupEv socketQueue
    , ConnectionSource do
        (acceptEv, socket, closeConnection) <- readTQueue socketQueue
        pure $ Connector $ pure Connection
          { closeConnection = atomically . closeConnection
          , channel = socketAsChannel socket
          , toPeer = handshakeServerPeer toPeer
              . traceHandshakeServer traceServer (composeInjectSelector inject $ injectServer @s) (reference acceptEv)
              . simpleHandshakeServer (signature $ Proxy @ps)
          }
    )
    where
      open setupEv addr = withEvent (ServerBind @s) \_ ->
        bracketOnError (liftIO $ openSocket addr) (close' setupEv) \socket -> liftIO do
          setSocketOption socket ReuseAddr 1
          withFdSocket socket setCloseOnExecIfNeeded
          bind socket $ addrAddress addr
          listen socket 1024
          pure socket

      close' setupEv socket = withEventArgs (simpleNewEventArgs $ ServerClose @s) { newEventCauses = [reference setupEv] } $ const $ liftIO $ close socket

      loop setupEv socketQueue socket = join $ withEventArgs (simpleNewEventArgs $ ServerAccept @s) { newEventCauses = [reference setupEv] } \ev ->
        bracketOnError (liftIO $ accept socket) (close' setupEv . fst) \(conn, peer) -> do
          addField ev peer
          -- pure / join is so that ServerAccept doesn't become the parent
          -- event of everything in the calls to loop and serve
          pure
            $ fmap fst
            $ concurrently (loop setupEv socketQueue socket)
            $ serve ev socketQueue conn peer `catch` (gracefulClose' ev conn peer . Just)

      gracefulClose' acceptEv conn peer ex =
        withEventArgs (simpleNewEventArgs (ServerCloseConnection @s)) { newEventCauses = [reference acceptEv] } \ev -> do
          addField ev peer
          liftIO $ gracefulClose conn 5000
          finalize ev ex

      serve acceptEv socketQueue conn peer = do
        closeTMVar <- atomically do
          closeTMVar <- newEmptyTMVar
          writeTQueue socketQueue (acceptEv, conn, void . tryPutTMVar closeTMVar)
          pure closeTMVar
        ex <- atomically $ readTMVar closeTMVar
        gracefulClose' acceptEv conn peer ex

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

runSomeConnector :: MonadUnliftIO m => SomeConnector pr peer m -> peer m a -> m a
runSomeConnector (SomeConnector connector) = runConnector connector

renderTcpServerSelectorOTel
  :: Text
  -> RenderSelectorOTel server
  -> RenderSelectorOTel (TcpServerSelector server)
renderTcpServerSelectorOTel serviceName renderServer = \case
  SetupServer -> OTelRendered
    { eventName = "server.setup"
    , eventKind = Internal
    , renderField = \case
        ServerHost host -> [ ("net.host.name", fromString host) ]
        ServerPort port -> [ ("net.host.port", toAttribute @Int64 $ fromIntegral port) ]
    }
  ServerResolve -> OTelRendered
    { eventName = "server.tcp.resolve"
    , eventKind = Internal
    , renderField = addrInfoToAttributes serviceName
    }
  ServerBind -> OTelRendered
    { eventName = "server.tcp.bind"
    , eventKind = Internal
    , renderField = \case
    }
  ServerClose -> OTelRendered
    { eventName = "server.tcp.close"
    , eventKind = Internal
    , renderField = \case
    }
  ServerCloseConnection -> OTelRendered
    { eventName = "server.tcp.close_connection"
    , eventKind = Internal
    , renderField = sockAddrToAttributes True
    }
  ServerAccept -> OTelRendered
    { eventName = "server.tcp.accept"
    , eventKind = Internal
    , renderField = sockAddrToAttributes True
    }
  LiftServer sel ->
    let rendered = renderHandshakeServerSelectorOTel renderServer sel
     in rendered
      { eventName = case eventKind rendered of
          Server -> serviceName <> "/" <> eventName rendered
          _ -> eventName rendered
      , renderField = \case
          LiftServerHost addr -> addrInfoToAttributes serviceName addr
          LiftServerPeer peer -> sockAddrToAttributes True peer
          LiftServerApp f -> renderField rendered f
      }

addrInfoToAttributes :: Text -> AddrInfo -> [(T.Text, Attribute)]
addrInfoToAttributes serviceName AddrInfo{..} =
  ( "net.transport"
  , case addrSocketType of
    Stream -> "ip_tcp"
    Datagram -> "ip_udp"
    _ -> "other"
  )
  : ("rpc.system", "typed-protocols")
  : ("rpc.service", toAttribute serviceName)
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
