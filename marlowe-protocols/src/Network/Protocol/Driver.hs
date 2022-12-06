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

import Control.Exception.Lifted (SomeException, bracketOnError, mask, throw, try)
import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.Cleanup (MonadCleanup)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (Value, toJSON)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Base16 (encodeBase16)
import Data.Void (Void, absurd)
import GHC.Show (showSpace)
import Network.Channel (Channel(..), hoistChannel, socketAsChannel)
import Network.Socket (AddrInfo, SockAddr, Socket, addrAddress, close, connect, openSocket)
import Network.Socket.Address (accept)
import Network.TypedProtocol (Message, Peer(..), PeerHasAgency, PeerRole, SomeMessage(..), runPeerWithDriver)
import Network.TypedProtocol.Codec (Codec(..), DecodeStep(..))
import Network.TypedProtocol.Driver (Driver(..))
import Observe.Event (addField, addParent, narrowEventBackend, reference, subEventBackend, withEvent)
import Observe.Event.Backend (EventBackend, noopEventBackend)
import Observe.Event.BackendModification (modifyEventBackend, setAncestor)
import Observe.Event.Render.JSON (DefaultRenderFieldJSON(..), DefaultRenderSelectorJSON(..))

class MessageToJSON ps where
  messageToJSON :: PeerHasAgency pr (st :: ps) -> Message ps st st' -> Value

class ShowMessage ps where
  showsPrecMessage :: Int -> PeerHasAgency pr (st :: ps) -> Message ps st st' -> ShowS

data DriverSelector ps f where
  Send :: DriverSelector ps (SendField ps)
  Recv :: DriverSelector ps (RecvField ps)

deriving instance Show (DriverSelector ps f)

data MessageWithAgency ps st st' = forall pr. MessageWithAgency (PeerHasAgency pr st) (Message ps st st')

data SendField ps where
  SendMessage :: PeerHasAgency pr st -> Message ps (st :: ps) (st' :: ps) -> SendField ps

instance ShowMessage ps => Show (SendField ps) where
  showsPrec p (SendMessage pa msg) = showParen (p >= 11)
    ( showString "SendMessage"
    . showSpace
    . showsPrecMessage 11 pa msg
    )

data RecvField ps where
  StateBefore :: Maybe ByteString -> RecvField ps
  StateAfter :: Maybe ByteString -> RecvField ps
  RecvMessage :: PeerHasAgency pr st -> Message ps (st :: ps) (st' :: ps) -> RecvField ps

instance ShowMessage ps => Show (RecvField ps) where
  showsPrec p = showParen (p >= 11) . \case
    RecvMessage pa msg ->
      ( showString "RecvMessage"
      . showSpace
      . showsPrecMessage 11 pa msg
      )
    StateBefore state ->
      ( showString "StateBefore"
      . showSpace
      . showsPrec 11 (encodeBase16 <$> state)
      )
    StateAfter state ->
      ( showString "StateAfter"
      . showSpace
      . showsPrec 11 (encodeBase16 <$> state)
      )

instance MessageToJSON ps => DefaultRenderSelectorJSON (DriverSelector ps) where
  defaultRenderSelectorJSON = \case
    Send -> ("send", defaultRenderFieldJSON)
    Recv -> ("recv", defaultRenderFieldJSON)

instance MessageToJSON ps => DefaultRenderFieldJSON (SendField ps) where
  defaultRenderFieldJSON = \case
    SendMessage pa msg -> ("message", messageToJSON pa msg)

instance MessageToJSON ps => DefaultRenderFieldJSON (RecvField ps) where
  defaultRenderFieldJSON = \case
    StateAfter state -> ("stateAfter", toBase16JSON state)
    StateBefore state -> ("stateBefore", toBase16JSON state)
    RecvMessage pa msg -> ("message", messageToJSON pa msg)

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

instance MessageToJSON ps => DefaultRenderSelectorJSON (ConnectSocketDriverSelector ps) where
  defaultRenderSelectorJSON = \case
    Connect -> ("connect", \addr -> ("address", toJSON $ show addr))
    Disconnect -> ("disconnect", absurd)
    ClientDriverEvent ev -> defaultRenderSelectorJSON ev

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
  Connected :: AcceptSocketDriverSelector ps SockAddr
  Disconnected :: AcceptSocketDriverSelector ps Void
  ServerDriverEvent :: DriverSelector ps f -> AcceptSocketDriverSelector ps f

instance MessageToJSON ps => DefaultRenderSelectorJSON (AcceptSocketDriverSelector ps) where
  defaultRenderSelectorJSON = \case
    Connected -> ("connected", \addr -> ("address", toJSON $ show addr))
    Disconnected -> ("disconnected", absurd)
    ServerDriverEvent ev -> defaultRenderSelectorJSON ev

acceptRunServerPeerOverSocketWithLogging
  :: (MonadBaseControl IO m, MonadCleanup m)
  => EventBackend m r (AcceptSocketDriverSelector protocol)
  -> (forall x. ex -> m x)
  -> Socket
  -> Codec protocol ex m ByteString
  -> ToPeer server protocol peer st m
  -> m (RunServer m server)
acceptRunServerPeerOverSocketWithLogging eventBackend throwImpl socket codec toPeer = do
  (conn, addr) <- liftBase $ accept socket
  (eventBackend', ref) <- withEvent eventBackend Connected \ev -> do
    addField ev addr
    pure (subEventBackend ServerDriverEvent ev, reference ev)
  let
    driver = logDriver eventBackend'
      $ mkDriver throwImpl codec
      $ hoistChannel liftBase
      $ socketAsChannel conn
  pure $ RunServer \server -> do
    result <- try @_ @SomeException do
      let peer = toPeer server
      fst <$> runPeerWithDriver driver peer (startDState driver)
    withEvent eventBackend Disconnected \ev -> do
      addParent ev ref
      either throw pure result

hoistPeer :: Functor m => (forall x. m x -> n x) -> Peer protocol pr st m a -> Peer protocol pr st n a
hoistPeer f = \case
  Effect m -> Effect $ f $ hoistPeer f <$> m
  Done na a -> Done na a
  Yield wa msg peer -> Yield wa msg $ hoistPeer f peer
  Await ta handle -> Await ta $ hoistPeer f . handle
