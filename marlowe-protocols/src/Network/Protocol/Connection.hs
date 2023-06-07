{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Connection where

import Control.Applicative (Alternative(empty), (<|>))
import Control.Concurrent.STM (STM, newEmptyTMVar)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy (ByteString)
import Network.Protocol.Peer.Trace
import Network.TypedProtocol (Message, PeerHasAgency)
import UnliftIO
  ( MonadUnliftIO
  , SomeException
  , TQueue
  , atomically
  , catch
  , newTQueue
  , putTMVar
  , readTMVar
  , readTQueue
  , throwIO
  , writeTQueue
  )

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

data ClientWithResponseChannel client m where
  ClientWithResponseChannel
    :: client m a
    -> (Either SomeException a -> STM ())
    -> ClientWithResponseChannel client m

stmConnectionSource
  :: MonadUnliftIO m
  => TQueue (ClientWithResponseChannel client m)
  -> (forall a b. server m b -> client m a -> m (a, b))
  -> ConnectionSource server m
stmConnectionSource queue serveClient = ConnectionSource do
  ClientWithResponseChannel client sendResult <- readTQueue queue
  pure $ stmServerConnector client sendResult serveClient

stmServerConnector
  :: MonadUnliftIO m
  => client m x
  -> (Either SomeException x -> STM ())
  -> (forall a b. server m b -> client m a -> m (a, b))
  -> Connector server m
stmServerConnector client sendResult serveClient = Connector $ pure Connection
  { runConnection = \server -> do
      (a, b) <- serveClient server client `catch` \ex -> do
        atomically $ sendResult $ Left ex
        throwIO ex
      atomically $ sendResult $ Right a
      pure b
  }

stmClientConnector
  :: MonadUnliftIO m
  => TQueue (ClientWithResponseChannel client m)
  -> Connector client m
stmClientConnector queue = Connector $ pure Connection
  { runConnection = \client -> do
      readResult <- atomically do
        resultVar <- newEmptyTMVar
        writeTQueue queue $ ClientWithResponseChannel client $ putTMVar resultVar
        pure $ readTMVar resultVar
      either throwIO pure =<< atomically readResult
  }

data DriverSelector ps f where
  SendMessage :: PeerHasAgency pr st -> Message ps st st' -> DriverSelector ps ()
  RecvMessage :: PeerHasAgency pr (st :: ps) -> DriverSelector ps (RecvMessageField ps st)

data RecvMessageField ps st where
  RecvMessageStateBeforeSpan :: Maybe ByteString -> RecvMessageField ps st
  RecvMessageStateBeforeMessage :: Maybe ByteString -> RecvMessageField ps st
  RecvMessageStateAfterMessage :: Maybe ByteString -> RecvMessageField ps st
  RecvMessageMessage :: Message ps st st' -> RecvMessageField ps st

data ClientServerPair server client m = ClientServerPair
  { connectionSource :: ConnectionSource server m
  , clientConnector :: Connector client m
  }

clientServerPair
  :: forall server client m
   . MonadUnliftIO m
  => (forall a b. server m b -> client m a -> m (a, b))
  -> STM (ClientServerPair server client m)
clientServerPair serveClient = do
  queue <- newTQueue
  let
  pure ClientServerPair
    { connectionSource = stmConnectionSource queue serveClient
    , clientConnector = stmClientConnector queue
    }
