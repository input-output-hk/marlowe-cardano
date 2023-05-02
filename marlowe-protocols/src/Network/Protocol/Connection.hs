{-# LANGUAGE DataKinds #-}
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

type ToPeer peer protocol pr st r m = forall a. peer m a -> PeerTraced protocol pr st r m a

newtype Connector ps pr peer r m = Connector
  { openConnection :: m (Connection ps pr peer r m)
  }

ihoistConnector
  :: (Functor m, Functor n)
  => (forall p q a. Functor p => (forall x. p x -> q x) -> peer p a -> peer q a)
  -> (forall x. m x -> n x)
  -> (forall x. n x -> m x)
  -> Connector ps pr peer r m
  -> Connector ps pr peer r n
ihoistConnector hoistPeer' f f' Connector{..} = Connector $ f $ ihoistConnection hoistPeer' f f' <$> openConnection

type ClientConnector ps = Connector ps 'AsClient
type ServerConnector ps = Connector ps 'AsServer

data SomeConnector pr peer r m =
  forall ps. BinaryMessage ps => SomeConnector (Connector ps pr peer r m)

data SomeConnectorTraced pr peer r s m =
  forall ps. BinaryMessage ps => SomeConnectorTraced
    (InjectSelector (TypedProtocolsSelector ps) s)
    (Connector ps pr peer r m)

type SomeClientConnector = SomeConnector 'AsClient
type SomeServerConnector = SomeConnector 'AsServer

type SomeClientConnectorTraced = SomeConnectorTraced 'AsClient
type SomeServerConnectorTraced = SomeConnectorTraced 'AsServer

newtype ConnectionSource ps server r m = ConnectionSource
  { acceptConnector :: STM (Connector ps 'AsServer server r m)
  }

instance Semigroup (ConnectionSource ps server r m) where
  (ConnectionSource source1) <> (ConnectionSource source2) = ConnectionSource $ source1 <|> source2

instance Monoid (ConnectionSource ps server r m) where
  mempty = ConnectionSource empty

data SomeConnectionSource server r m =
  forall ps. BinaryMessage ps => SomeConnectionSource (ConnectionSource ps server r m)

data SomeConnectionSourceTraced server r s m =
  forall ps. BinaryMessage ps => SomeConnectionSourceTraced
    (InjectSelector (TypedProtocolsSelector ps) s)
    (ConnectionSource ps server r m)

data Connection ps pr peer r m = forall (st :: ps). Connection
  { closeConnection :: Maybe SomeException -> m ()
  , channel :: Channel m ByteString
  , toPeer :: ToPeer peer ps pr st r m
  , openRef :: r
  }

ihoistConnection
  :: (Functor m, Functor n)
  => (forall p q a. Functor p => (forall x. p x -> q x) -> peer p a -> peer q a)
  -> (forall x. m x -> n x)
  -> (forall x. n x -> m x)
  -> Connection ps pr peer r m
  -> Connection ps pr peer r n
ihoistConnection hoistPeer' f f' Connection{..} = Connection
  { closeConnection = f . closeConnection
  , channel = hoistChannel f channel
  , toPeer = hoistPeerTraced f . toPeer . hoistPeer' f'
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
acceptSomeConnectorTraced (SomeConnectionSourceTraced inj ConnectionSource{..}) =
  SomeConnectorTraced inj <$> atomically acceptConnector

stmConnectionSource
  :: MonadIO m
  => r
  -> TQueue (STMChannel ByteString)
  -> ToPeer server ps 'AsServer st r m
  -> ConnectionSource ps server r m
stmConnectionSource r queue toPeer = ConnectionSource do
  channel <- readTQueue queue
  pure $ stmServerConnector r channel toPeer

stmServerConnector
  :: MonadIO m
  => r
  -> STMChannel ByteString
  -> ToPeer client ps 'AsServer st r m
  -> ServerConnector ps client r m
stmServerConnector openRef (STMChannel channel closeChannel) toPeer = Connector $ pure Connection
  { closeConnection = const $ atomically closeChannel
  , channel = hoistChannel atomically channel
  , ..
  }

stmClientConnector
  :: MonadIO m
  => r
  -> TQueue (STMChannel ByteString)
  -> ToPeer client ps 'AsClient st r m
  -> ClientConnector ps client r m
stmClientConnector openRef queue toPeer = Connector do
  STMChannel channel closeChannel <- atomically do
    (clientChannel, serverChannel) <- channelPair
    writeTQueue queue serverChannel
    pure clientChannel
  pure Connection
    { closeConnection = \_ -> atomically closeChannel
    , channel = hoistChannel atomically channel
    , ..
    }

data ClientServerPair ps server client r m = ClientServerPair
  { connectionSource :: ConnectionSource ps server r m
  , clientConnector :: ClientConnector ps client r m
  }

clientServerPair
  :: forall ps server client r m st
   . MonadUnliftIO m
  => r
  -> ToPeer server ps 'AsServer st r m
  -> ToPeer client ps 'AsClient st r m
  -> STM (ClientServerPair ps server client r m)
clientServerPair r serverToPeer clientToPeer = do
  serverChannelQueue <- newTQueue
  let
  pure ClientServerPair
    { connectionSource = stmConnectionSource r serverChannelQueue serverToPeer
    , clientConnector = stmClientConnector r serverChannelQueue clientToPeer
    }
