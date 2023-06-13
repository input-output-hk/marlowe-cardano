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
import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy(Proxy))
import Network.Channel (Channel(..), socketAsChannel)
import Network.Protocol.Codec (BinaryMessage, DeserializeError, binaryCodec)
import Network.Protocol.Connection (Connection(..), Connector(..), Socket(..), ToPeer)
import Network.Protocol.Handshake.Client (handshakeClientPeer, simpleHandshakeClient)
import Network.Protocol.Handshake.Server (handshakeServerPeer, simpleHandshakeServer)
import Network.Protocol.Handshake.Types (HasSignature, signature)
import Network.Protocol.Peer.Trace
import Network.Run.TCP (runTCPServer)
import Network.Socket
  ( AddrInfo(..)
  , HostName
  , PortNumber
  , SocketType(..)
  , addrAddress
  , close
  , connect
  , defaultHints
  , getAddrInfo
  , openSocket
  )
import Network.TypedProtocol (Message, PeerHasAgency, PeerRole(..), SomeMessage(..), runPeerWithDriver)
import Network.TypedProtocol.Codec (Codec(..), DecodeStep(..))
import Network.TypedProtocol.Driver (Driver(..))
import UnliftIO (MonadIO, MonadUnliftIO, finally, throwIO, withRunInIO)

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
  , serverSocket :: Socket server m ()
  , toPeer :: ToPeer server ps 'AsServer st m
  }

tcpServer
  :: forall m ps env server
   . (MonadUnliftIO m, BinaryMessage ps, HasSignature ps, MonadFail m, WithLog env C.Message m)
  => String
  -> Component m (TcpServerDependencies ps server m) ()
tcpServer name = component_ (name <> "-tcp-server") \TcpServerDependencies{..} ->
  withRunInIO \runInIO -> runTCPServer (Just host) (show port) \socket -> runInIO $ runResourceT do
    server <- getServer serverSocket
    let driver = mkDriver $ socketAsChannel socket
    let handshakeServer = simpleHandshakeServer (signature $ Proxy @ps) server
    let peer = peerTracedToPeer $ handshakeServerPeer toPeer handshakeServer
    lift $ fst <$> runPeerWithDriver driver peer (startDState driver)

tcpClient
  :: forall client ps st m
   . (MonadUnliftIO m, BinaryMessage ps, MonadFail m, HasSignature ps)
  => HostName
  -> PortNumber
  -> ToPeer client ps 'AsClient st m
  -> Connector client m
tcpClient host port toPeer = Connector $ liftIO $ do
  addr <- head <$> getAddrInfo
    (Just defaultHints { addrSocketType = Stream })
    (Just host)
    (Just $ show port)
  socket <- openSocket addr
  connect socket $ addrAddress addr
  pure Connection
    { runConnection = \client -> do
        let driver = mkDriver $ socketAsChannel socket
        let handshakeClient = simpleHandshakeClient (signature $ Proxy @ps) client
        let peer = peerTracedToPeer $ handshakeClientPeer toPeer handshakeClient
        fst <$> runPeerWithDriver driver peer (startDState driver) `finally` liftIO (close socket)
    }
