{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
import Network.Protocol.Peer (hoistPeer)
import Network.Protocol.Peer.Trace
import Network.TypedProtocol
import Observe.Event (InjectSelector)
import UnliftIO (MonadIO, MonadUnliftIO, atomically)

type ToPeer peer protocol pr st m = forall a. peer m a -> Peer protocol pr st m a

type ToPeerTraced peer protocol pr st r m = forall a. peer m a -> PeerTraced protocol pr st r m a

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

type ClientConnector ps = Connector ps 'AsClient
type ServerConnector ps = Connector ps 'AsServer

type ClientConnectorTraced ps = ConnectorTraced ps 'AsClient
type ServerConnectorTraced ps = ConnectorTraced ps 'AsServer

data SomeConnector pr peer r m =
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

data SomeConnectionSource server r m =
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
  , toPeer :: ToPeerTraced  peer ps pr st r m
  , openRef :: r
  , injectProtocolSelector :: forall ps'. InjectSelector (TypedProtocolsSelector ps') (s ps')
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
  , toPeer = hoistPeer f . toPeer . hoistPeer' f'
  , ..
  }

acceptSomeConnector
  :: MonadUnliftIO m
  => SomeConnectionSource server r m
  -> m (SomeServerConnector server r m)
acceptSomeConnector (SomeConnectionSource ConnectionSource{..}) =
  SomeConnector <$> atomically acceptConnector

acceptSomeConnectorTraced
  :: MonadUnliftIO m
  => SomeConnectionSourceTraced server r s m
  -> m (SomeServerConnectorTraced server r s m)
acceptSomeConnectorTraced (SomeConnectionSourceTraced inj ConnectionSourceTraced{..}) =
  SomeConnectorTraced inj <$> atomically acceptConnectorTraced

stmConnectionSource
  :: MonadIO m
  => TQueue (STMChannel ByteString)
  -> ToPeer server ps 'AsServer st m
  -> ConnectionSource ps server m
stmConnectionSource queue toPeer = ConnectionSource do
  channel <- readTQueue queue
  pure $ stmServerConnector channel toPeer

stmServerConnector
  :: MonadIO m
  => STMChannel ByteString
  -> ToPeer client ps 'AsServer st m
  -> ServerConnector ps client m
stmServerConnector (STMChannel channel closeChannel) toPeer = Connector $ pure Connection
  { closeConnection = const $ atomically closeChannel
  , channel = hoistChannel atomically channel
  , ..
  }

stmClientConnector
  :: MonadIO m
  => TQueue (STMChannel ByteString)
  -> ToPeer client ps 'AsClient st m
  -> ClientConnector ps client m
stmClientConnector queue toPeer = Connector do
  STMChannel channel closeChannel <- atomically do
    (clientChannel, serverChannel) <- channelPair
    writeTQueue queue serverChannel
    pure clientChannel
  pure Connection
    { closeConnection = \_ -> atomically closeChannel
    , channel = hoistChannel atomically channel
    , ..
    }

data ClientServerPair ps server client m = ClientServerPair
  { connectionSource :: ConnectionSource ps server m
  , clientConnector :: ClientConnector ps client m
  }

clientServerPair
  :: forall ps server client m st
   . MonadUnliftIO m
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
