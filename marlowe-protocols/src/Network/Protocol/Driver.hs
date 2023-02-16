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

import Control.Concurrent.STM (STM, atomically, newTQueue, readTQueue)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Exception (Exception)
import Control.Exception.Lifted (bracket, bracketOnError, finally, throwIO)
import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.Cleanup (MonadCleanup)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (Value, toJSON)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Base16 (encodeBase16)
import Data.Void (Void)
import Network.Channel (Channel(..), channelPair, hoistChannel, socketAsChannel)
import Network.Socket (AddrInfo, SockAddr, Socket, addrAddress, close, connect, gracefulClose, openSocket)
import Network.Socket.Address (accept)
import Network.TypedProtocol (Message, Peer(..), PeerHasAgency, PeerRole, SomeMessage(..), runPeerWithDriver)
import Network.TypedProtocol.Codec (Codec(..), DecodeStep(..))
import Network.TypedProtocol.Driver (Driver(..))
import Observe.Event (addField, subEventBackend, withEvent, withSubEvent)
import Observe.Event.Backend (EventBackend, noopEventBackend)
import Observe.Event.Component (FieldConfig(..), GetSelectorConfig, SelectorConfig(..), SomeJSON(..), absurdFieldConfig)

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
  => AddrInfo
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
  -> AddrInfo
  -> Codec protocol ex m ByteString
  -> ToPeer client protocol peer st m
  -> RunClient m client
runClientPeerOverSocketWithLogging eventBackend addr = runClientPeerWithLoggingGeneral
  eventBackend
  ( liftBase do
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

acceptRunServerPeerOverSocket
  :: (MonadBaseControl IO m, MonadCleanup m, Exception ex)
  => Socket
  -> Codec protocol ex m ByteString
  -> ToPeer server protocol peer st m
  -> m (RunServer m server)
acceptRunServerPeerOverSocket = acceptRunServerPeerOverSocketWithLogging $ noopEventBackend ()

data AcceptSocketDriverSelector ps f where
  ServerSession :: AcceptSocketDriverSelector ps Void
  Connected :: AcceptSocketDriverSelector ps Void
  ServerDriverEvent :: DriverSelector ps f -> AcceptSocketDriverSelector ps f

data SocketDriverConfigOptions = SocketDriverConfigOptions
  { enableConnected :: Bool
  , enableDisconnected :: Bool
  , enableServerDriverEvent :: Bool
  }

getAcceptSocketDriverSelectorConfig
  :: MessageToJSON ps
  => SocketDriverConfigOptions
  -> GetSelectorConfig (AcceptSocketDriverSelector ps)
getAcceptSocketDriverSelectorConfig SocketDriverConfigOptions{..} = \case
  Connected -> SelectorConfig "connected" enableConnected absurdFieldConfig
  ServerSession -> SelectorConfig "session" enableDisconnected absurdFieldConfig
  ServerDriverEvent sel -> getDriverSelectorConfig enableServerDriverEvent sel

getConnectSocketDriverSelectorConfig
  :: MessageToJSON ps
  => SocketDriverConfigOptions
  -> GetSelectorConfig (ConnectSocketDriverSelector ps)
getConnectSocketDriverSelectorConfig SocketDriverConfigOptions{..} = \case
  Connect -> SelectorConfig "connect" enableConnected absurdFieldConfig
  ClientSession -> SelectorConfig "session" enableDisconnected absurdFieldConfig
  ClientDriverEvent sel -> getDriverSelectorConfig enableServerDriverEvent sel

acceptRunServerPeerOverSocketWithLogging
  :: forall server protocol st ex m r peer
   . (MonadBaseControl IO m, MonadCleanup m, Exception ex)
  => EventBackend m r (AcceptSocketDriverSelector protocol)
  -> Socket
  -> Codec protocol ex m ByteString
  -> ToPeer server protocol peer st m
  -> m (RunServer m server)
acceptRunServerPeerOverSocketWithLogging eventBackend socket = acceptRunServerPeerWithLogging'
  eventBackend
  (liftBase $ fst <$> accept @SockAddr socket)
  (liftBase . close)
  (liftBase . flip gracefulClose 5000)
  (hoistChannel liftBase . socketAsChannel)

acceptRunServerPeerWithLogging'
  :: forall server protocol st ex m channel r peer
   . (MonadBaseControl IO m, MonadCleanup m, Exception ex)
  => EventBackend m r (AcceptSocketDriverSelector protocol)
  -> m channel
  -> (channel -> m ())
  -> (channel -> m ())
  -> (channel -> Channel m ByteString)
  -> Codec protocol ex m ByteString
  -> ToPeer server protocol peer st m
  -> m (RunServer m server)
acceptRunServerPeerWithLogging' eventBackend acceptChannel closeChannelOnError closeChannel mkChannel codec toPeer =
  bracketOnError acceptChannel closeChannelOnError \ch -> do
    let
      runServer :: server m a -> m a
      runServer server = withEvent eventBackend ServerSession \ev -> do
        withSubEvent ev Connected $ const $ pure ()
        let
          driver = logDriver (subEventBackend ServerDriverEvent ev)
            $ mkDriver codec $ mkChannel ch
        fst <$> runPeerWithDriver driver (toPeer server) (startDState driver)
    pure $ RunServer \server -> runServer server `finally` closeChannel ch

data ClientServerPair m server client = ClientServerPair
  { acceptRunServer :: m (RunServer m server)
  , runClient :: RunClient m client
  }

clientServerPair
  :: forall protocol ex server client serverPeer clientPeer m st r
   . (MonadBaseControl IO m, MonadCleanup m, Exception ex)
  => EventBackend m r (AcceptSocketDriverSelector protocol)
  -> EventBackend m r (ConnectSocketDriverSelector protocol)
  -> Codec protocol ex m ByteString
  -> ToPeer server protocol serverPeer st m
  -> ToPeer client protocol clientPeer st m
  -> STM (ClientServerPair m server client)
clientServerPair serverEventBackend clientEventBackend codec serverToPeer clientToPeer = do
  serverChannelQueue <- newTQueue
  let
    acceptRunServer :: m (RunServer m server)
    acceptRunServer = acceptRunServerPeerWithLogging'
      serverEventBackend
      (liftBase $ atomically $ readTQueue serverChannelQueue)
      (liftBase . atomically . snd)
      (liftBase . atomically . snd)
      (hoistChannel (liftBase . atomically) . fst)
      codec
      serverToPeer

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


hoistPeer :: Functor m => (forall x. m x -> n x) -> Peer protocol pr st m a -> Peer protocol pr st n a
hoistPeer f = \case
  Effect m -> Effect $ f $ hoistPeer f <$> m
  Done na a -> Done na a
  Yield wa msg peer -> Yield wa msg $ hoistPeer f peer
  Await ta handle -> Await ta $ hoistPeer f . handle
