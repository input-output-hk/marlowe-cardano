{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Connection
  where

import Control.Applicative (Alternative(empty), (<|>))
import Control.Concurrent.STM (STM, TQueue, newTQueue, readTQueue, writeTQueue)
import Control.Exception (SomeException)
import Data.ByteString.Lazy (ByteString)
import Network.Channel (Channel(..), STMChannel(..), channelPair, hoistChannel)
import Network.Protocol.Codec (BinaryMessage)
import Network.Protocol.Peer.Trace
import Network.TypedProtocol
import Observe.Event (InjectSelector)
import UnliftIO (MonadIO, MonadUnliftIO, atomically)

type ToPeer peer protocol pr st m = forall a. peer m a -> PeerTraced protocol pr st m a

newtype Connector ps pr peer m = Connector
  { openConnection :: m (Connection ps pr peer m)
  }

newtype ConnectorTraced ps pr peer r s m = ConnectorTraced
  { openConnectionTraced :: m (ConnectionTraced ps pr peer r s m)
  }

ihoistConnector
  :: (Functor m, Functor n)
  => (forall p q a. Functor p => (forall x. p x -> q x) -> peer p a -> peer q a)
  -> (forall x. m x -> n x)
  -> (forall x. n x -> m x)
  -> Connector ps pr peer m
  -> Connector ps pr peer n
ihoistConnector hoistPeer' f f' Connector{..} = Connector $ f $ ihoistConnection hoistPeer' f f' <$> openConnection

ihoistConnectorTraced
  :: (Functor m, Functor n)
  => (forall p q a. Functor p => (forall x. p x -> q x) -> peer p a -> peer q a)
  -> (forall x. m x -> n x)
  -> (forall x. n x -> m x)
  -> ConnectorTraced ps pr peer r s m
  -> ConnectorTraced ps pr peer r s n
ihoistConnectorTraced hoistPeer' f f' ConnectorTraced{..} =
  ConnectorTraced $ f $ ihoistConnectionTraced hoistPeer' f f' <$> openConnectionTraced

type ClientConnector ps = Connector ps 'AsClient
type ServerConnector ps = Connector ps 'AsServer

type ClientConnectorTraced ps = ConnectorTraced ps 'AsClient
type ServerConnectorTraced ps = ConnectorTraced ps 'AsServer

data SomeConnector pr peer m =
  forall ps. BinaryMessage ps => SomeConnector (Connector ps pr peer m)

data SomeConnectorTraced pr peer r s m =
  forall ps s'. BinaryMessage ps => SomeConnectorTraced
    (InjectSelector (s' ps) s)
    (ConnectorTraced ps pr peer r s' m)

type SomeClientConnector = SomeConnector 'AsClient
type SomeServerConnector = SomeConnector 'AsServer

type SomeClientConnectorTraced = SomeConnectorTraced 'AsClient
type SomeServerConnectorTraced = SomeConnectorTraced 'AsServer

newtype ConnectionSource ps server m = ConnectionSource
  { acceptConnector :: STM (Connector ps 'AsServer server m)
  }

newtype ConnectionSourceTraced ps server r s m = ConnectionSourceTraced
  { acceptConnectorTraced :: STM (ConnectorTraced ps 'AsServer server r s m)
  }

instance Semigroup (ConnectionSource ps server m) where
  (ConnectionSource source1) <> (ConnectionSource source2) = ConnectionSource $ source1 <|> source2

instance Monoid (ConnectionSource ps server m) where
  mempty = ConnectionSource empty

instance Semigroup (ConnectionSourceTraced ps server r s m) where
  (ConnectionSourceTraced source1) <> (ConnectionSourceTraced source2) = ConnectionSourceTraced $ source1 <|> source2

instance Monoid (ConnectionSourceTraced ps server r s m) where
  mempty = ConnectionSourceTraced empty

data SomeConnectionSource server m =
  forall ps. BinaryMessage ps => SomeConnectionSource (ConnectionSource ps server m)

data SomeConnectionSourceTraced server r s m =
  forall ps s'. BinaryMessage ps => SomeConnectionSourceTraced
    (InjectSelector (s' ps) s)
    (ConnectionSourceTraced ps server r s' m)

data Connection ps pr peer m = forall (st :: ps). Connection
  { closeConnection :: Maybe SomeException -> m ()
  , channel :: Channel m ByteString
  , toPeer :: ToPeer peer ps pr st m
  }

data ConnectionTraced ps pr peer r s m = forall (st :: ps). ConnectionTraced
  { closeConnection :: Maybe SomeException -> m ()
  , channel :: Channel m ByteString
  , toPeer :: ToPeer  peer ps pr st m
  , openRef :: r
  , injectProtocolSelector :: forall ps'. InjectSelector (TypedProtocolsSelector ps') (s ps')
  , injectDriverSelector :: forall ps'. InjectSelector (DriverSelector ps') (s ps')
  }

ihoistConnectionTraced
  :: (Functor m, Functor n)
  => (forall p q a. Functor p => (forall x. p x -> q x) -> peer p a -> peer q a)
  -> (forall x. m x -> n x)
  -> (forall x. n x -> m x)
  -> ConnectionTraced ps pr peer r s m
  -> ConnectionTraced ps pr peer r s n
ihoistConnectionTraced hoistPeer' f f' ConnectionTraced{..} = ConnectionTraced
  { closeConnection = f . closeConnection
  , channel = hoistChannel f channel
  , toPeer = hoistPeerTraced f . toPeer . hoistPeer' f'
  , ..
  }

ihoistConnection
  :: (Functor m, Functor n)
  => (forall p q a. Functor p => (forall x. p x -> q x) -> peer p a -> peer q a)
  -> (forall x. m x -> n x)
  -> (forall x. n x -> m x)
  -> Connection ps pr peer m
  -> Connection ps pr peer n
ihoistConnection hoistPeer' f f' Connection{..} = Connection
  { closeConnection = f . closeConnection
  , channel = hoistChannel f channel
  , toPeer = hoistPeerTraced f . toPeer . hoistPeer' f'
  , ..
  }

acceptSomeConnector
  :: MonadUnliftIO m
  => SomeConnectionSource server m
  -> m (SomeServerConnector server m)
acceptSomeConnector (SomeConnectionSource ConnectionSource{..}) =
  SomeConnector <$> atomically acceptConnector

acceptSomeConnectorTraced
  :: MonadUnliftIO m
  => SomeConnectionSourceTraced server r s m
  -> m (SomeServerConnectorTraced server r s m)
acceptSomeConnectorTraced (SomeConnectionSourceTraced inj ConnectionSourceTraced{..}) =
  SomeConnectorTraced inj <$> atomically acceptConnectorTraced

stmConnectionSource
  :: (MonadIO m, Monoid r)
  => TQueue (STMChannel ByteString)
  -> ToPeer server ps 'AsServer st m
  -> ConnectionSourceTraced ps server r STMConnectorSelector m
stmConnectionSource queue toPeer = ConnectionSourceTraced do
  channel <- readTQueue queue
  pure $ stmServerConnector channel toPeer

stmServerConnector
  :: (Monoid r, MonadIO m)
  => STMChannel ByteString
  -> ToPeer client ps 'AsServer st m
  -> ServerConnectorTraced ps client r STMConnectorSelector m
stmServerConnector (STMChannel channel closeChannel) toPeer = ConnectorTraced $ pure ConnectionTraced
  { closeConnection = const $ atomically closeChannel
  , channel = hoistChannel atomically channel
  , openRef = mempty
  , injectProtocolSelector = \s f -> f (Protocol s) id
  , injectDriverSelector = \s f -> f (DriverSelector s) id
  , ..
  }

stmClientConnector
  :: (MonadIO m, Monoid r)
  => TQueue (STMChannel ByteString)
  -> ToPeer client ps 'AsClient st m
  -> ClientConnectorTraced ps client r STMConnectorSelector m
stmClientConnector queue toPeer = ConnectorTraced do
  STMChannel channel closeChannel <- atomically do
    (clientChannel, serverChannel) <- channelPair
    writeTQueue queue serverChannel
    pure clientChannel
  pure ConnectionTraced
    { closeConnection = \_ -> atomically closeChannel
    , channel = hoistChannel atomically channel
    , openRef = mempty
    , injectProtocolSelector = \s f -> f (Protocol s) id
    , injectDriverSelector = \s f -> f (DriverSelector s) id
    , ..
    }

data DriverSelector ps f where
  SendMessage :: PeerHasAgency pr st -> Message ps st st' -> DriverSelector ps ()
  RecvMessage :: PeerHasAgency pr (st :: ps) -> DriverSelector ps (RecvMessageField ps st)

data RecvMessageField ps st where
  RecvMessageStateBeforeSpan :: Maybe ByteString -> RecvMessageField ps st
  RecvMessageStateBeforeMessage :: Maybe ByteString -> RecvMessageField ps st
  RecvMessageStateAfterMessage :: Maybe ByteString -> RecvMessageField ps st
  RecvMessageMessage :: Message ps st st' -> RecvMessageField ps st

data STMConnectorSelector ps f where
  Protocol :: TypedProtocolsSelector ps f -> STMConnectorSelector ps f
  DriverSelector :: DriverSelector ps f -> STMConnectorSelector ps f

data ClientServerPair ps server client r m = ClientServerPair
  { connectionSource :: ConnectionSourceTraced ps server r STMConnectorSelector m
  , clientConnector :: ClientConnectorTraced ps client r STMConnectorSelector m
  }

clientServerPair
  :: forall ps server client r m st
   . (MonadUnliftIO m, Monoid r)
  => ToPeer server ps 'AsServer st m
  -> ToPeer client ps 'AsClient st m
  -> STM (ClientServerPair ps server client r m)
clientServerPair serverToPeer clientToPeer = do
  serverChannelQueue <- newTQueue
  let
  pure ClientServerPair
    { connectionSource = stmConnectionSource serverChannelQueue serverToPeer
    , clientConnector = stmClientConnector serverChannelQueue clientToPeer
    }

tracedConnectionSourceToConnectionSource
  :: Functor m
  => ConnectionSourceTraced ps peer r s m
  -> ConnectionSource ps peer m
tracedConnectionSourceToConnectionSource ConnectionSourceTraced{..} =
  ConnectionSource $ tracedConnectorToConnector <$> acceptConnectorTraced

tracedConnectorToConnector
  :: Functor m
  => ConnectorTraced ps pr peer r s m
  -> Connector ps pr peer m
tracedConnectorToConnector ConnectorTraced{..} =
  Connector $ tracedConnectionToConnection <$> openConnectionTraced

tracedConnectionToConnection
  :: ConnectionTraced ps pr peer r s m
  -> Connection ps pr peer m
tracedConnectionToConnection ConnectionTraced{..} = Connection{..}
