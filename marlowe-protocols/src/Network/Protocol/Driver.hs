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
import Control.Exception (Exception, SomeException)
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
  :: forall ps failure bytes m
   . (MonadBase IO m, Exception failure)
  => Codec ps failure m bytes
  -> Channel m bytes
  -> Driver ps (Maybe bytes) m
mkDriver Codec{..} Channel{..} = Driver{..}
  where
    sendMessage
      :: forall (pr :: PeerRole) (st :: ps) (st' :: ps)
       . PeerHasAgency pr st
      -> Message ps st st'
      -> m ()
    sendMessage tok = send . encode tok

    recvMessage
      :: forall (pr :: PeerRole) (st :: ps)
       . PeerHasAgency pr st
      -> Maybe bytes
      -> m (SomeMessage st, Maybe bytes)
    recvMessage tok trailing = decodeChannel trailing =<< decode tok

    decodeChannel
      :: Maybe bytes
      -> DecodeStep bytes failure m a
      -> m (a, Maybe bytes)
    decodeChannel _ (DecodeDone a trailing)     = pure (a, trailing)
    decodeChannel _ (DecodeFail failure)        = throwIO failure
    decodeChannel Nothing (DecodePartial next)  = recv >>= next >>= decodeChannel Nothing
    decodeChannel trailing (DecodePartial next) = next trailing >>= decodeChannel Nothing

    startDState :: Maybe bytes
    startDState = Nothing

type RunClient m client = forall a. client m a -> m a
newtype RunServer m server = RunServer (forall a. server m a -> m a)

type ToPeer machine protocol peer st m = forall a. machine m a -> Peer protocol peer st m a

runClientPeerOverSocket
  :: (MonadCleanup m, MonadBaseControl IO m, Exception ex)
  => HostName
  -> PortNumber
  -> Codec protocol ex m ByteString
  -> ToPeer client protocol peer st m
  -> RunClient m client
runClientPeerOverSocket = runClientPeerOverSocketWithLogging $ noopEventBackend ()

data ConnectSocketDriverSelector ps f where
  ClientSession :: ConnectSocketDriverSelector ps Void
  Connect :: ConnectSocketDriverSelector ps Void
  ClientDriverEvent :: DriverSelector ps f -> ConnectSocketDriverSelector ps f

runClientPeerOverSocketWithLogging
  :: (MonadBaseControl IO m, MonadCleanup m, Exception ex)
  => EventBackend m r (ConnectSocketDriverSelector protocol)
  -> HostName
  -> PortNumber
  -> Codec protocol ex m ByteString
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
  :: (MonadBaseControl IO m, MonadCleanup m, Exception ex)
  => EventBackend m r (ConnectSocketDriverSelector protocol)
  -> m channel
  -> (channel -> m ())
  -> (channel -> Channel m ByteString)
  -> Codec protocol ex m ByteString
  -> ToPeer client protocol peer st m
  -> RunClient m client
runClientPeerWithLoggingGeneral eventBackend openChannel closeChannel mkChannel codec toPeer client =
  withEvent eventBackend ClientSession \ev ->
    runClientPeerGeneral
      (withSubEvent ev Connect $ const openChannel)
      closeChannel
      (logDriver (subEventBackend ClientDriverEvent ev) . mkDriver codec . mkChannel)
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

data TcpServerDependencies ps server ex m = forall (st :: ps). TcpServerDependencies
  { host :: HostName
  , port :: PortNumber
  , codec :: Codec ps ex m ByteString
  , toPeer :: ToPeer server ps 'AsServer st m
  }

tcpServer
  :: MonadBase IO m
  => Component m (TcpServerDependencies ps server ex m) (ConnectionSource ps server ex m ByteString)
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
          , connectionCodec = codec
          }
    )

newtype MakeServerConnection ps server ex m bytes = MakeServerConnection
  { runMakeServerConnection :: forall a. server m a -> m (Connection ps 'AsServer ex m bytes a)
  }

newtype ConnectionSource ps server ex m bytes = ConnectionSource
  { acceptConnection :: STM (MakeServerConnection ps server ex m bytes)
  }

data ConnectionSourceSelector ps bytes f where
  Session :: ConnectionSourceSelector ps bytes Void
  ConnectionSelector :: ConnectionSelector ps bytes f -> ConnectionSourceSelector ps bytes f

getConnectionSourceSelectorConfig
  :: MessageToJSON ps
  => Bool
  -> Bool
  -> GetSelectorConfig (ConnectionSourceSelector ps ByteString)
getConnectionSourceSelectorConfig channelEnabled peerEnabled = \case
  Session -> SelectorConfig "session" True absurdFieldConfig
  ConnectionSelector sel -> prependKey "session" $ getConnectionSelectorConfig channelEnabled peerEnabled sel

logConnectionSource
  :: (MonadBaseControl IO m, MonadCleanup m)
  => EventBackend m r (ConnectionSourceSelector ps bytes)
  -> ConnectionSource ps server ex m bytes
  -> ConnectionSource ps server ex m bytes
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

data Connection ps pr ex m bytes a = forall (st :: ps). Connection
  { closeConnection :: Maybe SomeException -> m ()
  , channel :: Channel m bytes
  , connectionCodec :: Codec ps ex m bytes
  , peer :: Peer ps pr st m a
  }

data ConnectionSelector ps bytes f where
  ChannelSelector :: ChannelSelector bytes f -> ConnectionSelector ps bytes f
  PeerSelector :: PeerSelector ps f -> ConnectionSelector ps bytes f

getConnectionSelectorConfig
  :: MessageToJSON ps
  => Bool
  -> Bool
  -> GetSelectorConfig (ConnectionSelector ps ByteString)
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
  => EventBackend m r (ConnectionSelector ps bytes)
  -> Connection ps pr ex m bytes a
  -> Connection ps pr ex m bytes a
logConnection eventBackend Connection{..} = Connection
  { channel = logChannel (narrowEventBackend ChannelSelector eventBackend) channel
  , peer = logPeer (narrowEventBackend PeerSelector eventBackend) peer
  , ..
  }

awaitConnection
  :: (MonadBaseControl IO m, Exception ex)
  => ConnectionSource ps server ex m bytes
  -> m (RunServer m server)
awaitConnection ConnectionSource{..} = do
  MakeServerConnection{..} <- liftBase $ atomically acceptConnection
  pure $ RunServer $ runConnection <=< runMakeServerConnection

runConnection
  :: (MonadBaseControl IO m, Exception ex)
  => Connection ps machine ex m bytes a
  -> m a
runConnection Connection{..} = do
  let driver = mkDriver connectionCodec channel
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
  :: forall ps ex server client m st r
   . (MonadBaseControl IO m, MonadCleanup m, Exception ex)
  => EventBackend m r (ConnectionSourceSelector ps ByteString)
  -> EventBackend m r (ConnectSocketDriverSelector ps)
  -> Codec ps ex m ByteString
  -> ToPeer server ps 'AsServer st m
  -> ToPeer client ps 'AsClient st m
  -> STM (ClientServerPair m server client)
clientServerPair serverEventBackend clientEventBackend codec serverToPeer clientToPeer = do
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
          , connectionCodec = codec
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
      codec
      clientToPeer

  pure ClientServerPair{..}
