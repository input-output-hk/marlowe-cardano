{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Driver where

import qualified Colog as C
import Colog.Monad (WithLog)
import Control.Concurrent.Component
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy (Proxy))
import Network.Channel (socketAsChannel)
import Network.Protocol.Codec (BinaryMessage (..))
import Network.Protocol.Connection (Connection (..), Connector (..), ServerSource (..), ToPeer)
import qualified Network.Protocol.Driver.Untyped as Untyped
import Network.Protocol.Handshake.Client (handshakeClientPeer, simpleHandshakeClient)
import Network.Protocol.Handshake.Server (handshakeServerPeer, simpleHandshakeServer)
import Network.Protocol.Handshake.Types (HasSignature, signature)
import Network.Protocol.Peer.Trace
import Network.Run.TCP (runTCPServer)
import Network.Socket (
  AddrInfo (..),
  HostName,
  PortNumber,
  SocketType (..),
  addrAddress,
  close,
  connect,
  defaultHints,
  getAddrInfo,
  openSocket,
 )
import Network.TypedProtocol (Message, PeerHasAgency, PeerRole (..), SomeMessage (..), runPeerWithDriver)
import Network.TypedProtocol.Driver (Driver (..))
import UnliftIO (MonadIO, MonadUnliftIO, finally, throwIO, withRunInIO)

mkDriver
  :: forall ps m
   . (MonadIO m, BinaryMessage ps)
  => Untyped.Driver m
  -> Driver ps (Maybe ByteString) m
mkDriver Untyped.Driver{..} = Driver{..}
  where
    sendMessage
      :: forall (pr :: PeerRole) (st :: ps) (st' :: ps)
       . PeerHasAgency pr st
      -> Message ps st st'
      -> m ()
    sendMessage = fmap sendSuccessMessage . putMessage

    recvMessage
      :: forall (pr :: PeerRole) (st :: ps)
       . PeerHasAgency pr st
      -> Maybe ByteString
      -> m (SomeMessage st, Maybe ByteString)
    recvMessage tok trailing = either throwIO pure =<< recvMessageUntyped trailing (getMessage tok)

    startDState :: Maybe ByteString
    startDState = Nothing

hoistDriver :: (forall x. m x -> n x) -> Driver ps dState m -> Driver ps dState n
hoistDriver f Driver{..} =
  Driver
    { sendMessage = fmap f . sendMessage
    , recvMessage = fmap f . recvMessage
    , ..
    }

data TcpServerDependencies ps server m = forall (st :: ps).
  TcpServerDependencies
  { host :: HostName
  , port :: PortNumber
  , serverSource :: ServerSource server m ()
  , toPeer :: ToPeer server ps 'AsServer st m
  }

tcpServer
  :: forall m ps env server
   . (MonadUnliftIO m, BinaryMessage ps, HasSignature ps, MonadFail m, WithLog env C.Message m)
  => String
  -> Component m (TcpServerDependencies ps server m) ()
tcpServer name = component_ (name <> "-tcp-server") \TcpServerDependencies{..} ->
  withRunInIO \runInIO -> runTCPServer (Just host) (show port) $ runComponent_ $ hoistComponent runInIO $ component_ (name <> "-tcp-worker") \socket -> runResourceT do
    server <- getServer serverSource
    let handshakeServer = simpleHandshakeServer (signature $ Proxy @ps) server
    let peer = peerTracedToPeer $ handshakeServerPeer toPeer handshakeServer
    let untypedDriver = Untyped.mkDriver $ socketAsChannel socket
    let driver = mkDriver untypedDriver
    lift $ fst <$> runPeerWithDriver driver peer (startDState driver)

tcpClient
  :: forall client ps st m
   . (MonadUnliftIO m, BinaryMessage ps, MonadFail m, HasSignature ps)
  => HostName
  -> PortNumber
  -> ToPeer client ps 'AsClient st m
  -> Connector client m
tcpClient host port toPeer = Connector $ liftIO $ do
  addr <-
    head
      <$> getAddrInfo
        (Just defaultHints{addrSocketType = Stream})
        (Just host)
        (Just $ show port)
  socket <- openSocket addr
  connect socket $ addrAddress addr
  pure
    Connection
      { runConnection = \client -> do
          let untypedDriver = Untyped.mkDriver $ socketAsChannel socket
          let driver = mkDriver untypedDriver
          let handshakeClient = simpleHandshakeClient (signature $ Proxy @ps) client
          let peer = peerTracedToPeer $ handshakeClientPeer toPeer handshakeClient
          fst <$> runPeerWithDriver driver peer (startDState driver) `finally` liftIO (close socket)
      }
