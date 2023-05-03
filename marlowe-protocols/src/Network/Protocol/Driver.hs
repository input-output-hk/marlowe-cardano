{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Protocol.Driver
  where

import Control.Concurrent.Component
import Control.Concurrent.STM (newEmptyTMVar, newTQueue, readTMVar, readTQueue, tryPutTMVar)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Functor (void)
import Network.Channel (Channel(..), socketAsChannel)
import Network.Protocol.Codec (BinaryMessage, DeserializeError, binaryCodec)
import Network.Protocol.Connection (Connection(..), ConnectionSource(..), Connector(..), SomeConnector(..), ToPeer)
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
import UnliftIO (MonadIO, MonadUnliftIO, atomically, mask, throwIO, try)

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
  , toPeer :: ToPeer server ps 'AsServer st m
  }

tcpServer :: (MonadIO m', MonadIO m) => Component m (TcpServerDependencies ps server m') (ConnectionSource ps server m')
tcpServer = component \TcpServerDependencies{..} -> do
  socketQueue <- newTQueue
  pure
    ( liftIO $ runTCPServer (Just host) (show port) \socket -> do
        closeTMVar <- atomically do
          closeTMVar <- newEmptyTMVar
          writeTQueue socketQueue (socket, void $ tryPutTMVar closeTMVar ())
          pure closeTMVar
        atomically $ readTMVar closeTMVar
    , ConnectionSource do
        (socket, closeConnection) <- readTQueue socketQueue
        pure $ Connector $ pure Connection
          { closeConnection = \_ -> atomically closeConnection
          , channel = socketAsChannel socket
          , ..
          }
    )

tcpClient
  :: MonadIO m
  => HostName
  -> PortNumber
  -> ToPeer client ps 'AsClient st m
  -> Connector ps 'AsClient client m
tcpClient host port toPeer = Connector $ liftIO $ do
  addr <- head <$> getAddrInfo
    (Just defaultHints { addrSocketType = Stream })
    (Just host)
    (Just $ show port)
  socket <- openSocket addr
  connect socket $ addrAddress addr
  pure Connection
    { closeConnection = \_ -> liftIO $ close socket
    , channel = socketAsChannel socket
    , ..
    }

runConnection :: (MonadUnliftIO m, BinaryMessage ps) => Connection ps pr peer m -> peer m a -> m a
runConnection Connection{..} peer = do
  let driver = mkDriver channel
  mask \restore -> do
    result <- try $ restore $ runPeerWithDriver driver (toPeer peer) (startDState driver)
    case result of
      Left ex -> do
        closeConnection $ Just ex
        throwIO ex
      Right (a, _) -> do
        closeConnection Nothing
        pure a

runConnector :: (MonadUnliftIO m, BinaryMessage ps) => Connector ps pr peer m -> peer m a -> m a
runConnector Connector{..} peer = flip runConnection peer =<< openConnection

runSomeConnector :: MonadUnliftIO m => SomeConnector pr peer m -> peer m a -> m a
runSomeConnector (SomeConnector connector) = runConnector connector
