{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Protocol.Driver
  where

import Control.Concurrent.Component
import Control.Concurrent.STM (STM, TQueue, atomically, newEmptyTMVar, newTQueue, readTMVar, readTQueue, tryPutTMVar)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (SomeException)
import Control.Exception.Lifted (mask, throwIO, try)
import Control.Monad ((<=<))
import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.Cleanup (MonadCleanup)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (Value)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Base16 (encodeBase16)
import Data.Functor (void)
import qualified Data.Text.Lazy as T
import Data.Void (Void)
import Network.Channel
  ( Channel(..)
  , ChannelSelector
  , STMChannel(STMChannel)
  , channelPair
  , getChannelSelectorConfig
  , hoistChannel
  , logChannel
  , socketAsChannel
  )
import Network.Protocol.Codec (BinaryMessage, DeserializeError, binaryCodec)
import Network.Protocol.Peer (PeerSelector, logPeer)
import qualified Network.Protocol.Peer as Peer
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
import Network.TypedProtocol (Message, Peer(..), PeerHasAgency, PeerRole(..), SomeMessage(..), runPeerWithDriver)
import Network.TypedProtocol.Codec (AnyMessageAndAgency(AnyMessageAndAgency), Codec(..), DecodeStep(..))
import Network.TypedProtocol.Driver (Driver(..))
import Observe.Event (failEvent, finalize, narrowEventBackend, newEvent, subEventBackend)
import Observe.Event.Backend (EventBackend)
import Observe.Event.Component
  (GetSelectorConfig, SelectorConfig(..), SomeJSON(..), absurdFieldConfig, prependKey, singletonFieldConfigWith)

class MessageToJSON ps where
  messageToJSON :: PeerHasAgency pr (st :: ps) -> Message ps st st' -> Value

data MessageWithAgency ps st st' = forall pr. MessageWithAgency (PeerHasAgency pr st) (Message ps st st')

mkDriver
  :: forall ps m
   . (MonadBase IO m, BinaryMessage ps)
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

type ToPeer peer protocol pr st m = forall a. peer m a -> Peer protocol pr st m a

data TcpServerDependencies ps server m = forall (st :: ps). TcpServerDependencies
  { host :: HostName
  , port :: PortNumber
  , toPeer :: ToPeer server ps 'AsServer st m
  }

tcpServer :: MonadBase IO m => Component m (TcpServerDependencies ps server m) (ConnectionSource ps server m)
tcpServer = component \TcpServerDependencies{..} -> do
  socketQueue <- newTQueue
  pure
    ( liftBase $ runTCPServer (Just host) (show port) \socket -> do
        closeTMVar <- atomically do
          closeTMVar <- newEmptyTMVar
          writeTQueue socketQueue (socket, void $ tryPutTMVar closeTMVar ())
          pure closeTMVar
        atomically $ readTMVar closeTMVar
    , ConnectionSource do
        (socket, closeConnection) <- readTQueue socketQueue
        pure $ Connector \server -> pure Connection
          { closeConnection = \_ -> liftBase $ atomically closeConnection
          , channel = hoistChannel liftBase $ socketAsChannel socket
          , peer = toPeer server
          }
    )

tcpClient
  :: MonadBase IO m
  => HostName
  -> PortNumber
  -> ToPeer client ps 'AsClient st m
  -> Connector ps 'AsClient client m
tcpClient host port toPeer = Connector \client -> do
  addr <- liftBase $ head <$> getAddrInfo
    (Just defaultHints { addrSocketType = Stream })
    (Just host)
    (Just $ show port)
  socket <- liftBase $ openSocket addr
  liftBase $ connect socket $ addrAddress addr
  pure Connection
    { closeConnection = \_ -> liftBase $ close socket
    , channel = hoistChannel liftBase $ socketAsChannel socket
    , peer = toPeer client
    }

newtype Connector ps pr peer m = Connector
  { connectPeer :: forall a. peer m a -> m (Connection ps pr m a)
  }

type ClientConnector ps = Connector ps 'AsClient
type ServerConnector ps = Connector ps 'AsServer

data SomeConnector pr peer m =
  forall ps. BinaryMessage ps => SomeConnector (Connector ps pr peer m)

type SomeClientConnector = SomeConnector 'AsClient
type SomeServerConnector = SomeConnector 'AsServer

newtype ConnectionSource ps server m = ConnectionSource
  { acceptConnector :: STM (Connector ps 'AsServer server m)
  }

data SomeConnectionSource server m =
  forall ps. BinaryMessage ps => SomeConnectionSource (ConnectionSource ps server m)

data ConnectorSelector ps f where
  Session :: ConnectorSelector ps Void
  ConnectionSelector :: ConnectionSelector ps f -> ConnectorSelector ps f

getConnectorSelectorConfig
  :: MessageToJSON ps
  => Bool
  -> Bool
  -> GetSelectorConfig (ConnectorSelector ps)
getConnectorSelectorConfig channelEnabled peerEnabled = \case
  Session -> SelectorConfig "session" True absurdFieldConfig
  ConnectionSelector sel -> prependKey "connection" $ getConnectionSelectorConfig channelEnabled peerEnabled sel

logConnector
  :: (MonadBaseControl IO m, MonadCleanup m)
  => EventBackend m r (ConnectorSelector ps)
  -> Connector ps pr peer m
  -> Connector ps pr peer m
logConnector eventBackend Connector{..} = Connector
  { connectPeer = \p -> do
      ev <- newEvent eventBackend Session
      Connection{..} <- connectPeer p
      pure $ logConnection (subEventBackend ConnectionSelector ev) Connection
        { closeConnection = \mError -> do
            case mError of
              Nothing -> finalize ev
              Just ex -> failEvent ev ex
            closeConnection mError
        , ..
        }
  }

logConnectionSource
  :: (MonadBaseControl IO m, MonadCleanup m)
  => EventBackend m r (ConnectorSelector ps)
  -> ConnectionSource ps server m
  -> ConnectionSource ps server m
logConnectionSource eventBackend ConnectionSource{..} = ConnectionSource
  { acceptConnector = logConnector eventBackend <$> acceptConnector
  }

data Connection ps pr m a = forall (st :: ps). Connection
  { closeConnection :: Maybe SomeException -> m ()
  , channel :: Channel m ByteString
  , peer :: Peer ps pr st m a
  }

data ConnectionSelector ps f where
  ChannelSelector :: ChannelSelector ByteString f -> ConnectionSelector ps f
  PeerSelector :: PeerSelector ps f -> ConnectionSelector ps f

getConnectionSelectorConfig
  :: MessageToJSON ps
  => Bool
  -> Bool
  -> GetSelectorConfig (ConnectionSelector ps)
getConnectionSelectorConfig channelEnabled peerEnabled = \case
  ChannelSelector sel -> prependKey "channel" $ getChannelSelectorConfig (T.toStrict . encodeBase16) channelEnabled sel
  PeerSelector sel -> prependKey "peer" $ getPeerSelectorConfig peerEnabled sel

getPeerSelectorConfig :: MessageToJSON ps => Bool -> GetSelectorConfig (PeerSelector ps)
getPeerSelectorConfig defaultEnabled = \case
  Peer.Send -> SelectorConfig "send" defaultEnabled $ singletonFieldConfigWith
    (\(AnyMessageAndAgency tok msg) -> SomeJSON $ messageToJSON tok msg)
    "message"
    True
  Peer.Recv -> SelectorConfig "recv" defaultEnabled $ singletonFieldConfigWith
    (\(AnyMessageAndAgency tok msg) -> SomeJSON $ messageToJSON tok msg)
    "message"
    True

logConnection
  :: (MonadBaseControl IO m, MonadCleanup m)
  => EventBackend m r (ConnectionSelector ps)
  -> Connection ps pr m a
  -> Connection ps pr m a
logConnection eventBackend Connection{..} = Connection
  { channel = logChannel (narrowEventBackend ChannelSelector eventBackend) channel
  , peer = logPeer (narrowEventBackend PeerSelector eventBackend) peer
  , ..
  }

acceptSomeConnector :: MonadBaseControl IO m => SomeConnectionSource server m -> m (SomeServerConnector server m)
acceptSomeConnector (SomeConnectionSource ConnectionSource{..}) = liftBase $ SomeConnector <$> atomically acceptConnector

runConnection :: (MonadBaseControl IO m, BinaryMessage ps) => Connection ps peer m a -> m a
runConnection Connection{..} = do
  let driver = mkDriver channel
  mask \restore -> do
    result <- try $ restore $ runPeerWithDriver driver peer (startDState driver)
    case result of
      Left ex -> do
        closeConnection $ Just ex
        throwIO ex
      Right (a, _) -> do
        closeConnection Nothing
        pure a

runConnector :: (MonadBaseControl IO m, BinaryMessage ps) => Connector ps pr peer m -> peer m a -> m a
runConnector Connector{..} = runConnection <=< connectPeer

runSomeConnector :: MonadBaseControl IO m => SomeConnector pr peer m -> peer m a -> m a
runSomeConnector (SomeConnector connector) = runConnector connector

stmConnectionSource
  :: MonadBase IO m
  => TQueue (STMChannel ByteString)
  -> ToPeer server ps 'AsServer st m
  -> ConnectionSource ps server m
stmConnectionSource queue toPeer = ConnectionSource do
  channel <- readTQueue queue
  pure $ stmServerConnector channel toPeer

stmServerConnector
  :: MonadBase IO m
  => STMChannel ByteString
  -> ToPeer client ps 'AsServer st m
  -> ServerConnector ps client m
stmServerConnector (STMChannel channel closeChannel) toPeer = Connector \server -> pure Connection
  { closeConnection = const $ liftBase $ atomically closeChannel
  , peer = toPeer server
  , channel = hoistChannel (liftBase . atomically) channel
  }

stmClientConnector
  :: MonadBase IO m
  => TQueue (STMChannel ByteString)
  -> ToPeer client ps 'AsClient st m
  -> ClientConnector ps client m
stmClientConnector queue toPeer = Connector \client -> do
  STMChannel channel closeChannel <- liftBase $ atomically do
    (clientChannel, serverChannel) <- channelPair
    writeTQueue queue serverChannel
    pure clientChannel
  pure Connection
    { closeConnection = \_ -> liftBase $ atomically closeChannel
    , channel = hoistChannel (liftBase . atomically) channel
    , peer = toPeer client
    }

data ClientServerPair ps server client m = ClientServerPair
  { connectionSource :: ConnectionSource ps server m
  , clientConnector :: ClientConnector ps client m
  }

data ClientServerPairSelector ps f where
  ClientEvent :: ConnectorSelector ps f -> ClientServerPairSelector ps f
  ServerEvent :: ConnectorSelector ps f -> ClientServerPairSelector ps f

getClientServerPairSelectorConfig
  :: MessageToJSON ps
  => Bool
  -> Bool
  -> GetSelectorConfig (ClientServerPairSelector ps)
getClientServerPairSelectorConfig channelEnabled peerEnabled = \case
  ClientEvent sel -> prependKey "client" $ getConnectorSelectorConfig channelEnabled peerEnabled sel
  ServerEvent sel -> prependKey "server" $ getConnectorSelectorConfig channelEnabled peerEnabled sel

logClientServerPair
  :: forall ps server client m r
   . (MonadBaseControl IO m, MonadCleanup m)
  => EventBackend m r (ClientServerPairSelector ps)
  -> ClientServerPair ps server client m
  -> ClientServerPair ps server client m
logClientServerPair eventBackend ClientServerPair{..} = ClientServerPair
  { connectionSource = logConnectionSource (narrowEventBackend ServerEvent eventBackend) connectionSource
  , clientConnector = logConnector (narrowEventBackend ClientEvent eventBackend) clientConnector
  }

clientServerPair
  :: forall ps server client m st
   . MonadBaseControl IO m
  => ToPeer server ps 'AsServer st m
  -> ToPeer client ps 'AsClient st m
  -> STM (ClientServerPair ps server client m)
clientServerPair serverToPeer clientToPeer = do
  serverChannelQueue <- newTQueue
  let
  pure ClientServerPair
    { connectionSource = stmConnectionSource serverChannelQueue serverToPeer
    , clientConnector = stmClientConnector serverChannelQueue clientToPeer
    }
