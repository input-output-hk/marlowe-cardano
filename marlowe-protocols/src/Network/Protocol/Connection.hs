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
import Control.Monad.Event.Class (MonadInjectEvent, composeInjectSelector, withEvent)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Base16 (encodeBase16)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text.Lazy as T
import Data.Void (Void)
import Network.Channel
  (Channel(..), ChannelSelector, STMChannel(..), channelPair, getChannelSelectorConfig, hoistChannel, logChannel)
import qualified Network.Channel as Channel
import Network.Protocol.Codec (BinaryMessage)
import Network.Protocol.Peer (PeerSelector, getPeerSelectorConfig, hoistPeer, logPeer)
import qualified Network.Protocol.Peer as Peer
import Network.TypedProtocol
import Observe.Event.Backend (InjectSelector)
import Observe.Event.Component
  (GetSelectorConfig, SelectorConfig(..), SelectorLogConfig, absurdFieldConfig, getDefaultLogConfig, prependKey)
import Observe.Event.Explicit (finalize, injectSelector)
import Observe.Event.Network.Protocol (MessageToJSON)
import UnliftIO (MonadIO, MonadUnliftIO, atomically)

type ToPeer peer protocol pr st m = forall a. peer m a -> Peer protocol pr st m a

newtype Connector ps pr peer m = Connector
  { openConnection :: m (Connection ps pr peer m)
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

data SomeConnector pr peer m =
  forall ps. BinaryMessage ps => SomeConnector (Connector ps pr peer m)

type SomeClientConnector = SomeConnector 'AsClient
type SomeServerConnector = SomeConnector 'AsServer

newtype ConnectionSource ps server m = ConnectionSource
  { acceptConnector :: STM (Connector ps 'AsServer server m)
  }

instance Semigroup (ConnectionSource ps server m) where
  (ConnectionSource source1) <> (ConnectionSource source2) = ConnectionSource $ source1 <|> source2

instance Monoid (ConnectionSource ps server m) where
  mempty = ConnectionSource empty

data SomeConnectionSource server m =
  forall ps. BinaryMessage ps => SomeConnectionSource (ConnectionSource ps server m)

data ConnectorSelector ps f where
  Connect :: ConnectorSelector ps Void
  ConnectionSelector :: ConnectionSelector ps f -> ConnectorSelector ps f

getDefaultConnectorLogConfig
  :: GetSelectorConfig s
  -> (forall f. ConnectorSelector ps f -> s f)
  -> Map Text SelectorLogConfig
getDefaultConnectorLogConfig getConfig inject = fold
  [ getDefaultLogConfig getConfig $ inject Connect
  , getDefaultLogConfig getConfig $ inject $ ConnectionSelector $ ChannelSelector Channel.Send
  , getDefaultLogConfig getConfig $ inject $ ConnectionSelector $ ChannelSelector Channel.Recv
  , getDefaultLogConfig getConfig $ inject $ ConnectionSelector $ PeerSelector Peer.Send
  , getDefaultLogConfig getConfig $ inject $ ConnectionSelector $ PeerSelector Peer.Recv
  , getDefaultLogConfig getConfig $ inject $ ConnectionSelector Close
  ]

getConnectorSelectorConfig
  :: MessageToJSON ps
  => Bool -- ^ Enable channel messages
  -> Bool -- ^ Enable peer messages
  -> GetSelectorConfig (ConnectorSelector ps)
getConnectorSelectorConfig channelEnabled peerEnabled = \case
  Connect -> SelectorConfig "connect" True absurdFieldConfig
  ConnectionSelector sel -> prependKey "connection" $ getConnectionSelectorConfig channelEnabled peerEnabled sel

logConnector
  :: MonadInjectEvent r s s m
  => InjectSelector (ConnectorSelector ps) s
  -> Connector ps pr peer m
  -> Connector ps pr peer m
logConnector inject Connector{..} = Connector
  { openConnection = inject Connect \s _ -> withEvent s \_ ->
      logConnection (composeInjectSelector inject $ injectSelector ConnectionSelector) <$> openConnection
  }

logConnectionSource
  :: MonadInjectEvent r s s m
  => InjectSelector (ConnectorSelector ps) s
  -> ConnectionSource ps server m
  -> ConnectionSource ps server m
logConnectionSource inject ConnectionSource{..} = ConnectionSource
  { acceptConnector = logConnector inject <$> acceptConnector
  }

data Connection ps pr peer m = forall (st :: ps). Connection
  { closeConnection :: Maybe SomeException -> m ()
  , channel :: Channel m ByteString
  , toPeer :: forall a. peer m a -> Peer ps pr st m a
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
  }

data ConnectionSelector ps f where
  ChannelSelector :: ChannelSelector ByteString f -> ConnectionSelector ps f
  PeerSelector :: PeerSelector ps f -> ConnectionSelector ps f
  Close :: ConnectionSelector ps Void

getConnectionSelectorConfig
  :: MessageToJSON ps
  => Bool
  -> Bool
  -> GetSelectorConfig (ConnectionSelector ps)
getConnectionSelectorConfig channelEnabled peerEnabled = \case
  ChannelSelector sel -> prependKey "channel" $ getChannelSelectorConfig (T.toStrict . encodeBase16) channelEnabled sel
  PeerSelector sel -> prependKey "peer" $ getPeerSelectorConfig peerEnabled sel
  Close -> SelectorConfig "close" True absurdFieldConfig

logConnection
  :: MonadInjectEvent r s s m
  => InjectSelector (ConnectionSelector ps) s
  -> Connection ps pr peer m
  -> Connection ps pr peer m
logConnection inject Connection{..} = Connection
  { channel = logChannel (composeInjectSelector inject $ injectSelector ChannelSelector) channel
  , toPeer = logPeer (composeInjectSelector inject $ injectSelector PeerSelector) . toPeer
  , closeConnection = \mError -> do
      inject Close \s _ -> withEvent s $ flip finalize mError
      closeConnection mError
  }

acceptSomeConnector :: MonadUnliftIO m => SomeConnectionSource server m -> m (SomeServerConnector server m)
acceptSomeConnector (SomeConnectionSource ConnectionSource{..}) = SomeConnector <$> atomically acceptConnector

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
  :: forall ps server client m r s
   . MonadInjectEvent r s s m
  => InjectSelector (ClientServerPairSelector ps) s
  -> ClientServerPair ps server client m
  -> ClientServerPair ps server client m
logClientServerPair inject ClientServerPair{..} = ClientServerPair
  { connectionSource = logConnectionSource (composeInjectSelector inject $ injectSelector ServerEvent) connectionSource
  , clientConnector = logConnector (composeInjectSelector inject $ injectSelector ClientEvent) clientConnector
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
