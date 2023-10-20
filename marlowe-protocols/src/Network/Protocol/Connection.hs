{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Connection where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (runResourceT, transResourceT)
import Control.Monad.Trans.Resource.Internal (ResourceT (..))
import Data.ByteString (ByteString)
import Network.Protocol.Peer.Trace
import Network.TypedProtocol (Message, PeerHasAgency)
import Observe.Event (InjectSelector)
import UnliftIO (MonadUnliftIO)

type ToPeer peer protocol pr st m = forall a. peer m a -> PeerTraced protocol pr st m a

newtype Connector peer m = Connector
  { openConnection :: m (Connection peer m)
  }

ihoistConnector
  :: (Functor m, Functor n)
  => (forall p q a. (Functor p) => (forall x. p x -> q x) -> peer p a -> peer q a)
  -> (forall x. m x -> n x)
  -> (forall x. n x -> m x)
  -> Connector peer m
  -> Connector peer n
ihoistConnector hoistPeer' f f' Connector{..} = Connector $ f $ ihoistConnection hoistPeer' f f' <$> openConnection

newtype Connection peer m = Connection
  { runConnection :: forall a. peer m a -> m a
  }

ihoistConnection
  :: (Functor n)
  => (forall p q a. (Functor p) => (forall x. p x -> q x) -> peer p a -> peer q a)
  -> (forall x. m x -> n x)
  -> (forall x. n x -> m x)
  -> Connection peer m
  -> Connection peer n
ihoistConnection hoistPeer' f f' Connection{..} =
  Connection
    { runConnection = f . runConnection . hoistPeer' f'
    , ..
    }

runConnector :: (Monad m) => Connector peer m -> peer m a -> m a
runConnector Connector{..} peer = flip runConnection peer =<< openConnection

data DriverSelector ps f where
  SendMessage :: PeerHasAgency pr st -> Message ps st st' -> DriverSelector ps ()
  RecvMessage :: PeerHasAgency pr (st :: ps) -> DriverSelector ps (RecvMessageField ps st)

data RecvMessageField ps st where
  RecvMessageStateBeforeSpan :: Maybe ByteString -> RecvMessageField ps st
  RecvMessageStateBeforeMessage :: Maybe ByteString -> RecvMessageField ps st
  RecvMessageStateAfterMessage :: Maybe ByteString -> RecvMessageField ps st
  RecvMessageMessage :: Message ps st st' -> RecvMessageField ps st

newtype ServerSourceTraced server r sub root m a = ServerSourceTraced
  { getServerTraced :: InjectSelector sub root -> ResourceT m (server m a)
  }
  deriving (Functor)

newtype ServerSource server m a = ServerSource
  { getServer :: ResourceT m (server m a)
  }
  deriving (Functor)

resourceTServerSource
  :: (Monad m)
  => (forall p q x. (Functor p) => (forall y. p y -> q y) -> server p x -> server q x)
  -> ResourceT m (server (ResourceT m) a)
  -> ServerSource server m a
resourceTServerSource hoistServer (ResourceT getServer) = ServerSource $ ResourceT \releaseMap -> do
  server <- getServer releaseMap
  pure $ hoistServer (\(ResourceT m) -> m releaseMap) server

hoistServerSource
  :: (Functor m)
  => (forall p q x. (Functor p) => (forall y. p y -> q y) -> server p x -> server q x)
  -> (forall x. m x -> n x)
  -> ServerSource server m a
  -> ServerSource server n a
hoistServerSource hoistServer f ServerSource{..} =
  ServerSource
    { getServer = transResourceT (f . fmap (hoistServer f)) getServer
    , ..
    }

directConnector
  :: (MonadUnliftIO m)
  => (forall x y. server m x -> client m y -> m (x, y))
  -> ServerSource server m a
  -> Connector client m
directConnector serveClient serverSource = Connector $ pure $ Connection \client -> runResourceT do
  server <- getServer serverSource
  lift $ snd <$> serveClient server client
