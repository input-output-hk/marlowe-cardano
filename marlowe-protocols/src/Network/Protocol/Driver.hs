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
import Control.Exception.Lifted (SomeException, bracketOnError, mask, throw, try)
import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.Cleanup (MonadCleanup)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (Value, toJSON)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Base16 (encodeBase16)
import Data.Void (Void)
import Network.Channel (Channel(..), channelPair, hoistChannel, socketAsChannel)
import Network.Socket (AddrInfo, SockAddr, Socket, addrAddress, close, connect, openSocket)
import Network.Socket.Address (accept)
import Network.TypedProtocol (Message, Peer(..), PeerHasAgency, PeerRole, SomeMessage(..), runPeerWithDriver)
import Network.TypedProtocol.Codec (Codec(..), DecodeStep(..))
import Network.TypedProtocol.Driver (Driver(..))
import Observe.Event (addField, addParent, narrowEventBackend, reference, subEventBackend, withEvent)
import Observe.Event.Backend (EventBackend, noopEventBackend)
import Observe.Event.BackendModification (modifyEventBackend, setAncestor)
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
   . Monad m
  => (forall a. failure -> m a)
  -> Codec ps failure m bytes
  -> Channel m bytes
  -> Driver ps (Maybe bytes) m
mkDriver throwImpl Codec{..} Channel{..} = Driver{..}
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
    decodeChannel _ (DecodeFail failure)        = throwImpl failure
    decodeChannel Nothing (DecodePartial next)  = recv >>= next >>= decodeChannel Nothing
    decodeChannel trailing (DecodePartial next) = next trailing >>= decodeChannel Nothing

    startDState :: Maybe bytes
    startDState = Nothing

type RunClient m client = forall a. client m a -> m a
newtype RunServer m server = RunServer (forall a. server m a -> m a)

type ToPeer machine protocol peer st m = forall a. machine m a -> Peer protocol peer st m a

runClientPeerOverSocket
  :: (MonadCleanup m, MonadBaseControl IO m)
  => (forall x. ex -> m x)
  -> AddrInfo
  -> Codec protocol ex m ByteString
  -> ToPeer client protocol peer st m
  -> RunClient m client
runClientPeerOverSocket = runClientPeerOverSocketWithLogging $ noopEventBackend ()

data ConnectSocketDriverSelector ps f where
  Connect :: ConnectSocketDriverSelector ps Void
  Disconnect :: ConnectSocketDriverSelector ps Void
  ClientDriverEvent :: DriverSelector ps f -> ConnectSocketDriverSelector ps f

runClientPeerOverSocketWithLogging
  :: (MonadBaseControl IO m, MonadCleanup m)
  => EventBackend m r (ConnectSocketDriverSelector protocol)
  -> (forall x. ex -> m x)
  -> AddrInfo
  -> Codec protocol ex m ByteString
  -> ToPeer client protocol peer st m
  -> RunClient m client
runClientPeerOverSocketWithLogging eventBackend throwImpl addr codec toPeer client =
  mask \restore -> do
    (ref, sock) <- open
    let
      channel = hoistChannel liftBase $ socketAsChannel sock
      eventBackend' = narrowEventBackend ClientDriverEvent
        $ modifyEventBackend (setAncestor ref) eventBackend
      driver = logDriver eventBackend' $ mkDriver throwImpl codec channel
      peer = toPeer client
    result <- try @_ @SomeException
      $ restore
      $ fst <$> runPeerWithDriver driver peer (startDState driver)
    withEvent eventBackend Disconnect \ev -> do
      addParent ev ref
      liftBase $ close sock
      either throw pure result
  where
    open = bracketOnError (liftBase $ openSocket addr) (liftBase . close) \sock ->
      withEvent eventBackend Connect \ev -> do
        liftBase (connect sock $ addrAddress addr)
        pure (reference ev, sock)

acceptRunServerPeerOverSocket
  :: (MonadBaseControl IO m, MonadCleanup m)
  => (forall x. ex -> m x)
  -> Socket
  -> Codec protocol ex m ByteString
  -> ToPeer server protocol peer st m
  -> m (RunServer m server)
acceptRunServerPeerOverSocket = acceptRunServerPeerOverSocketWithLogging $ noopEventBackend ()

data AcceptSocketDriverSelector ps f where
  Connected :: AcceptSocketDriverSelector ps Void
  Disconnected :: AcceptSocketDriverSelector ps Void
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
  Disconnected -> SelectorConfig "disconnected" enableDisconnected absurdFieldConfig
  ServerDriverEvent sel -> getDriverSelectorConfig enableServerDriverEvent sel

getConnectSocketDriverSelectorConfig
  :: MessageToJSON ps
  => SocketDriverConfigOptions
  -> GetSelectorConfig (ConnectSocketDriverSelector ps)
getConnectSocketDriverSelectorConfig SocketDriverConfigOptions{..} = \case
  Connect -> SelectorConfig "connect" enableConnected absurdFieldConfig
  Disconnect -> SelectorConfig "disconnect" enableDisconnected absurdFieldConfig
  ClientDriverEvent sel -> getDriverSelectorConfig enableServerDriverEvent sel

acceptRunServerPeerOverSocketWithLogging
  :: (MonadBaseControl IO m, MonadCleanup m)
  => EventBackend m r (AcceptSocketDriverSelector protocol)
  -> (forall x. ex -> m x)
  -> Socket
  -> Codec protocol ex m ByteString
  -> ToPeer server protocol peer st m
  -> m (RunServer m server)
acceptRunServerPeerOverSocketWithLogging eventBackend throwImpl socket codec toPeer = do
  (conn, _ :: SockAddr) <- liftBase $ accept socket
  (eventBackend', ref) <- withEvent eventBackend Connected \ev -> do
    pure (subEventBackend ServerDriverEvent ev, reference ev)
  let
    driver = logDriver eventBackend'
      $ mkDriver throwImpl codec
      $ hoistChannel liftBase
      $ socketAsChannel conn
  pure $ RunServer \server -> mask \restore -> do
    let peer = toPeer server
    result <- try @_ @SomeException $ restore $ fst <$> runPeerWithDriver driver peer (startDState driver)
    withEvent eventBackend Disconnected \ev -> do
      addParent ev ref
      either throw pure result

data ClientServerPair m server client = ClientServerPair
  { acceptRunServer :: m (RunServer m server)
  , runClient :: RunClient m client
  }

clientServerPair
  :: forall protocol ex server client serverPeer clientPeer m st r
   . (MonadBaseControl IO m, MonadCleanup m)
  => EventBackend m r (AcceptSocketDriverSelector protocol)
  -> EventBackend m r (ConnectSocketDriverSelector protocol)
  -> (forall x. ex -> m x)
  -> Codec protocol ex m ByteString
  -> ToPeer server protocol serverPeer st m
  -> ToPeer client protocol clientPeer st m
  -> STM (ClientServerPair m server client)
clientServerPair serverEventBackend clientEventBackend throwImpl codec serverToPeer clientToPeer = do
  serverChannelQueue <- newTQueue
  let
    acceptRunServer = do
      (channel, closeAction) <- liftBase $ atomically $ readTQueue serverChannelQueue
      (eventBackend', ref) <- withEvent serverEventBackend Connected \ev -> do
        pure (subEventBackend ServerDriverEvent ev, reference ev)
      let
        driver = logDriver eventBackend'
          $ mkDriver throwImpl codec
          $ hoistChannel (liftBase . atomically) channel
      pure $ RunServer \server -> mask \restore -> do
        let peer = serverToPeer server
        result <- try @_ @SomeException $ restore $ fst <$> runPeerWithDriver driver peer (startDState driver)
        withEvent serverEventBackend Disconnected \ev -> do
          addParent ev ref
          liftBase $ atomically closeAction
          either throw pure result
    runClient :: RunClient m client
    runClient client = mask \restore -> do
      (ref, (channel, closeAction)) <- withEvent clientEventBackend Connect \ev -> liftBase $ atomically do
        (clientChannel, serverChannel) <- channelPair
        writeTQueue serverChannelQueue serverChannel
        pure (reference ev, clientChannel)
      let
        eventBackend' = narrowEventBackend ClientDriverEvent
          $ modifyEventBackend (setAncestor ref) clientEventBackend
        driver = logDriver eventBackend'
          $ mkDriver throwImpl codec
          $ hoistChannel (liftBase . atomically) channel
        peer = clientToPeer client
      result <- try @_ @SomeException
        $ restore
        $ fst <$> runPeerWithDriver driver peer (startDState driver)
      withEvent clientEventBackend Disconnect \ev -> do
        addParent ev ref
        liftBase $ atomically closeAction
        either throw pure result
  pure ClientServerPair{..}


hoistPeer :: Functor m => (forall x. m x -> n x) -> Peer protocol pr st m a -> Peer protocol pr st n a
hoistPeer f = \case
  Effect m -> Effect $ f $ hoistPeer f <$> m
  Done na a -> Done na a
  Yield wa msg peer -> Yield wa msg $ hoistPeer f peer
  Await ta handle -> Await ta $ hoistPeer f . handle
