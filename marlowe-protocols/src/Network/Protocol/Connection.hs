{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Connection where

import Control.Applicative (Alternative(empty), (<|>))
import Control.Concurrent.STM (STM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT, transResourceT)
import Data.ByteString.Lazy (ByteString)
import Network.Protocol.Peer.Trace
import Network.TypedProtocol (Message, PeerHasAgency)
import UnliftIO (MonadUnliftIO, atomically)

type ToPeer peer protocol pr st m = forall a. peer m a -> PeerTraced protocol pr st m a

newtype Connector peer m = Connector
  { openConnection :: m (Connection peer m)
  }

ihoistConnector
  :: (Functor m, Functor n)
  => (forall p q a. Functor p => (forall x. p x -> q x) -> peer p a -> peer q a)
  -> (forall x. m x -> n x)
  -> (forall x. n x -> m x)
  -> Connector peer m
  -> Connector peer n
ihoistConnector hoistPeer' f f' Connector{..} = Connector $ f $ ihoistConnection hoistPeer' f f' <$> openConnection

newtype ConnectionSource server m = ConnectionSource
  { acceptConnectorSTM :: STM (Connector server m)
  }

instance Semigroup (ConnectionSource server m) where
  (ConnectionSource source1) <> (ConnectionSource source2) = ConnectionSource $ source1 <|> source2

instance Monoid (ConnectionSource server m) where
  mempty = ConnectionSource empty

newtype Connection peer m = Connection
  { runConnection :: forall a. peer m a -> m a
  }

ihoistConnection
  :: Functor n
  => (forall p q a. Functor p => (forall x. p x -> q x) -> peer p a -> peer q a)
  -> (forall x. m x -> n x)
  -> (forall x. n x -> m x)
  -> Connection peer m
  -> Connection peer n
ihoistConnection hoistPeer' f f' Connection{..} = Connection
  { runConnection = f . runConnection . hoistPeer' f'
  , ..
  }

runConnector :: Monad m => Connector peer m -> peer m a -> m a
runConnector Connector{..} peer = flip runConnection peer =<< openConnection

acceptConnector :: MonadIO m => ConnectionSource peer m -> m (Connector peer m)
acceptConnector = atomically . acceptConnectorSTM

data DriverSelector ps f where
  SendMessage :: PeerHasAgency pr st -> Message ps st st' -> DriverSelector ps ()
  RecvMessage :: PeerHasAgency pr (st :: ps) -> DriverSelector ps (RecvMessageField ps st)

data RecvMessageField ps st where
  RecvMessageStateBeforeSpan :: Maybe ByteString -> RecvMessageField ps st
  RecvMessageStateBeforeMessage :: Maybe ByteString -> RecvMessageField ps st
  RecvMessageStateAfterMessage :: Maybe ByteString -> RecvMessageField ps st
  RecvMessageMessage :: Message ps st st' -> RecvMessageField ps st

newtype Socket server m a = Socket
  { getServer :: ResourceT m (server m a)
  } deriving Functor

hoistSocket
  :: Functor m
  => (forall p q x. Functor p => (forall y. p y -> q y) -> server p x -> server q x)
  -> (forall x. m x -> n x)
  -> Socket server m a
  -> Socket server n a
hoistSocket hoistServer f Socket{..} = Socket
  { getServer = transResourceT (f . fmap (hoistServer f)) getServer
  , ..
  }

socketConnector
  :: MonadUnliftIO m
  => (forall x y. server m x -> client m y -> m (x, y))
  -> Socket server m a
  -> Connector client m
socketConnector serveClient socket = Connector $ runResourceT do
  server <- getServer socket
  pure $ Connection $ fmap snd . serveClient server
