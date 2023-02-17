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
import Control.Concurrent.STM (STM, atomically, newEmptyTMVar, newTQueue, readTMVar, readTQueue, tryPutTMVar)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (SomeException)
import Control.Exception.Lifted (bracket, mask, throwIO, try)
import Control.Monad ((<=<))
import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.Cleanup (MonadCleanup)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (Value, toJSON)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Base16 (encodeBase16)
import Data.Functor (void)
import qualified Data.Text.Lazy as T
import Data.Void (Void)
import Network.Channel
  (Channel(..), ChannelSelector, channelPair, getChannelSelectorConfig, hoistChannel, logChannel, socketAsChannel)
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
import Observe.Event
  (addField, failEvent, finalize, narrowEventBackend, newEvent, subEventBackend, withEvent, withSubEvent)
import Observe.Event.Backend (EventBackend, noopEventBackend)
import Observe.Event.Component
  ( FieldConfig(..)
  , GetSelectorConfig
  , SelectorConfig(..)
  , SomeJSON(..)
  , absurdFieldConfig
  , prependKey
  , singletonFieldConfigWith
  )

class MessageToJSON ps where
  messageToJSON :: PeerHasAgency pr (st :: ps) -> Message ps st st' -> Value

data DriverSelector ps f where
  Send :: DriverSelector ps (SendField ps)
  Recv :: DriverSelector ps (RecvField ps)

deriving instance Show (DriverSelector ps f)

data MessageWithAgency ps st st' = forall pr. MessageWithAgency (PeerHasAgency pr st) (Message ps st st')

data SendField ps where
  SendMessage :: PeerHasAgency pr st -> Message ps (st :: ps) (st' :: ps) -> SendField ps

data RecvField ps where
  StateBefore :: Maybe ByteString -> RecvField ps
  StateAfter :: Maybe ByteString -> RecvField ps
  RecvMessage :: PeerHasAgency pr st -> Message ps (st :: ps) (st' :: ps) -> RecvField ps

getDriverSelectorConfig :: MessageToJSON ps => Bool -> GetSelectorConfig (DriverSelector ps)
getDriverSelectorConfig defaultEnabled = \case
  Send -> SelectorConfig "send" defaultEnabled FieldConfig
    { fieldKey = const "message"
    , fieldDefaultEnabled = const True
    , toSomeJSON = \case
        SendMessage pa msg -> SomeJSON $ messageToJSON pa msg
    }
  Recv -> SelectorConfig "recv" defaultEnabled FieldConfig
    { fieldKey = \case
        StateBefore _ -> "state-before"
        StateAfter _ -> "state-after"
        RecvMessage _ _ -> "message"
    , fieldDefaultEnabled = \case
        StateBefore _ -> False
        StateAfter _ -> False
        RecvMessage _ _ -> True
    , toSomeJSON = \case
        StateAfter state -> SomeJSON $ toBase16JSON state
        StateBefore state -> SomeJSON $ toBase16JSON state
        RecvMessage pa msg -> SomeJSON $ messageToJSON pa msg
    }

toBase16JSON :: Maybe ByteString -> Value
toBase16JSON = toJSON . fmap encodeBase16

logDriver
  :: forall ps r m
   . MonadCleanup m
  => EventBackend m r (DriverSelector ps)
  -> Driver ps (Maybe ByteString) m
  -> Driver ps (Maybe ByteString) m
logDriver eventBackend Driver{..} = Driver{..}
  { sendMessage = \pa message -> withEvent eventBackend Send \ev -> do
      addField ev $ SendMessage pa message
      sendMessage pa message
  , recvMessage = \pa stateBefore -> withEvent eventBackend Recv \ev -> do
      addField ev $ StateBefore stateBefore
      (SomeMessage msg, stateAfter) <- recvMessage pa stateBefore
      addField ev $ RecvMessage pa msg
      addField ev $ StateAfter stateAfter
      pure (SomeMessage msg, stateAfter)
  }

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

type RunClient m client = forall a. client m a -> m a
newtype RunServer m server = RunServer (forall a. server m a -> m a)

type ToPeer machine protocol peer st m = forall a. machine m a -> Peer protocol peer st m a

runClientPeerOverSocket
  :: (MonadCleanup m, MonadBaseControl IO m, BinaryMessage protocol)
  => HostName
  -> PortNumber
  -> ToPeer client protocol peer st m
  -> RunClient m client
runClientPeerOverSocket = runClientPeerOverSocketWithLogging $ noopEventBackend ()

data ConnectSocketDriverSelector ps f where
  ClientSession :: ConnectSocketDriverSelector ps Void
  Connect :: ConnectSocketDriverSelector ps Void
  ClientDriverEvent :: DriverSelector ps f -> ConnectSocketDriverSelector ps f

runClientPeerOverSocketWithLogging
  :: (MonadBaseControl IO m, MonadCleanup m, BinaryMessage protocol)
  => EventBackend m r (ConnectSocketDriverSelector protocol)
  -> HostName
  -> PortNumber
  -> ToPeer client protocol peer st m
  -> RunClient m client
runClientPeerOverSocketWithLogging eventBackend host port = runClientPeerWithLoggingGeneral
  eventBackend
  ( liftBase do
      addr <- head <$> getAddrInfo
        (Just defaultHints { addrSocketType = Stream })
        (Just host)
        (Just $ show port)
      socket <- liftBase $ openSocket addr
      connect socket $ addrAddress addr
      pure socket
  )
  (liftBase . close)
  (hoistChannel liftBase . socketAsChannel)

runClientPeerWithLoggingGeneral
  :: (MonadBaseControl IO m, MonadCleanup m, BinaryMessage protocol)
  => EventBackend m r (ConnectSocketDriverSelector protocol)
  -> m channel
  -> (channel -> m ())
  -> (channel -> Channel m ByteString)
  -> ToPeer client protocol peer st m
  -> RunClient m client
runClientPeerWithLoggingGeneral eventBackend openChannel closeChannel mkChannel toPeer client =
  withEvent eventBackend ClientSession \ev ->
    runClientPeerGeneral
      (withSubEvent ev Connect $ const openChannel)
      closeChannel
      (logDriver (subEventBackend ClientDriverEvent ev) . mkDriver . mkChannel)
      toPeer
      client

runClientPeerGeneral
  :: MonadBaseControl IO m
  => m channel
  -> (channel -> m ())
  -> (channel -> Driver ps dState m)
  -> ToPeer client ps peer st m
  -> RunClient m client
runClientPeerGeneral openChannel closeChannel mkDriver' toPeer client =
  bracket openChannel closeChannel \ch ->
    let driver = mkDriver' ch
     in fst <$> runPeerWithDriver driver (toPeer client) (startDState driver)

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
        pure $ MakeServerConnection \server -> pure Connection
          { closeConnection = \_ -> liftBase $ atomically closeConnection
          , channel = hoistChannel liftBase $ socketAsChannel socket
          , peer = toPeer server
          }
    )

newtype MakeServerConnection ps server m = MakeServerConnection
  { runMakeServerConnection :: forall a. server m a -> m (Connection ps 'AsServer m a)
  }

newtype ConnectionSource ps server m = ConnectionSource
  { acceptConnection :: STM (MakeServerConnection ps server m)
  }

data ConnectionSourceSelector ps f where
  Session :: ConnectionSourceSelector ps Void
  ConnectionSelector :: ConnectionSelector ps f -> ConnectionSourceSelector ps f

getConnectionSourceSelectorConfig
  :: MessageToJSON ps
  => Bool
  -> Bool
  -> GetSelectorConfig (ConnectionSourceSelector ps)
getConnectionSourceSelectorConfig channelEnabled peerEnabled = \case
  Session -> SelectorConfig "session" True absurdFieldConfig
  ConnectionSelector sel -> prependKey "session" $ getConnectionSelectorConfig channelEnabled peerEnabled sel

logConnectionSource
  :: (MonadBaseControl IO m, MonadCleanup m)
  => EventBackend m r (ConnectionSourceSelector ps)
  -> ConnectionSource ps server m
  -> ConnectionSource ps server m
logConnectionSource eventBackend ConnectionSource{..} = ConnectionSource
  { acceptConnection = do
      MakeServerConnection{..} <- acceptConnection
      pure MakeServerConnection
        { runMakeServerConnection = \server -> do
            ev <- newEvent eventBackend Session
            Connection{..} <- runMakeServerConnection server
            pure $ logConnection (subEventBackend ConnectionSelector ev) Connection
              { closeConnection = \mError -> do
                  case mError of
                    Nothing -> finalize ev
                    Just ex -> failEvent ev ex
                  closeConnection mError
              , ..
              }
        }
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

awaitConnection :: (MonadBaseControl IO m, BinaryMessage ps) => ConnectionSource ps server m -> m (RunServer m server)
awaitConnection ConnectionSource{..} = do
  MakeServerConnection{..} <- liftBase $ atomically acceptConnection
  pure $ RunServer $ runConnection <=< runMakeServerConnection

runConnection :: (MonadBaseControl IO m, BinaryMessage ps) => Connection ps machine m a -> m a
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

data SocketDriverConfigOptions = SocketDriverConfigOptions
  { enableConnected :: Bool
  , enableDisconnected :: Bool
  , enableServerDriverEvent :: Bool
  }

getConnectSocketDriverSelectorConfig
  :: MessageToJSON ps
  => SocketDriverConfigOptions
  -> GetSelectorConfig (ConnectSocketDriverSelector ps)
getConnectSocketDriverSelectorConfig SocketDriverConfigOptions{..} = \case
  Connect -> SelectorConfig "connect" enableConnected absurdFieldConfig
  ClientSession -> SelectorConfig "session" enableDisconnected absurdFieldConfig
  ClientDriverEvent sel -> getDriverSelectorConfig enableServerDriverEvent sel

data ClientServerPair m server client = ClientServerPair
  { acceptRunServer :: m (RunServer m server)
  , runClient :: RunClient m client
  }

clientServerPair
  :: forall ps server client m st r
   . (MonadBaseControl IO m, MonadCleanup m, BinaryMessage ps)
  => EventBackend m r (ConnectionSourceSelector ps)
  -> EventBackend m r (ConnectSocketDriverSelector ps)
  -> ToPeer server ps 'AsServer st m
  -> ToPeer client ps 'AsClient st m
  -> STM (ClientServerPair m server client)
clientServerPair serverEventBackend clientEventBackend serverToPeer clientToPeer = do
  serverChannelQueue <- newTQueue
  let
    acceptRunServer :: m (RunServer m server)
    acceptRunServer = awaitConnection
      $ logConnectionSource serverEventBackend
      $ ConnectionSource do
        (channel, closeChannel) <- readTQueue serverChannelQueue
        pure $ MakeServerConnection \server -> pure Connection
          { closeConnection = const $ liftBase $ atomically closeChannel
          , peer = serverToPeer server
          , channel = hoistChannel (liftBase . atomically) channel
          }

    runClient :: RunClient m client
    runClient = runClientPeerWithLoggingGeneral
      clientEventBackend
      ( liftBase $ atomically do
          (clientChannel, serverChannel) <- channelPair
          writeTQueue serverChannelQueue serverChannel
          pure clientChannel
      )
      (liftBase . atomically . snd)
      (hoistChannel (liftBase . atomically) . fst)
      clientToPeer

  pure ClientServerPair{..}
