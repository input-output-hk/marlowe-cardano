{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Peer.Monad.TCP where

import qualified Colog as C
import Control.Concurrent.Component
import Control.Monad ((<=<))
import Control.Monad.Event.Class (MonadEvent (localBackend), composeInjectSelector, withInjectEvent)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Resource (runResourceT)
import Data.String (IsString (..))
import qualified Data.Text as T
import Data.Void (Void)
import GHC.IO (mkUserError)
import Network.Channel (socketAsChannel)
import Network.Protocol.Codec (BinaryMessage)
import Network.Protocol.Connection (
  Connection (..),
  ConnectionTraced (..),
  Connector (..),
  ConnectorTraced (..),
  ServerSourceTraced (..),
 )
import Network.Protocol.Driver (mkDriver, rethrowErrors)
import Network.Protocol.Driver.Trace (ConnectedField (..), addrInfoToAttributes, sockAddrToAttributes)
import qualified Network.Protocol.Driver.Untyped as Untyped
import Network.Protocol.Peer.Monad (Channel (..), ClientT, PeerT, ServerT, SomeMessageAndChannel (..), runPeerT')
import Network.Run.TCP (runTCPServer)
import Network.Socket (
  AddrInfo (..),
  AddrInfoFlag (..),
  HostName,
  PortNumber,
  SockAddr,
  Socket,
  SocketType (..),
  connect,
  defaultHints,
  getAddrInfo,
  getPeerName,
  openSocket,
 )
import Network.TypedProtocol (Driver, Protocol (..), SomeMessage (..))
import qualified Network.TypedProtocol as Driver
import Observe.Event (InjectSelector, addField, finalize, injectSelector, reference)
import Observe.Event.Backend (setInitialCauseEventBackend)
import Observe.Event.Render.OpenTelemetry (OTelRendered (..), RenderSelectorOTel)
import OpenTelemetry.Trace.Core (SpanKind (..))
import UnliftIO (MonadIO (..), MonadUnliftIO (..), atomicModifyIORef, mask, newIORef, throwIO, try)

tcpChannel
  :: forall pr ps st m
   . (MonadIO m, BinaryMessage ps)
  => Socket
  -> m (Channel pr ps st m, Untyped.Driver m)
tcpChannel socket = (,untypedDriver) <$> driverAsChannel (mkDriver untypedDriver)
  where
    untypedDriver = Untyped.mkDriver $ socketAsChannel socket

driverAsChannel :: forall pr ps st state m. (MonadIO m) => Driver ps state m -> m (Channel pr ps st m)
driverAsChannel driver = go $ Driver.startDState driver
  where
    go :: state -> forall st'. m (Channel pr ps st' m)
    go state = do
      usedRef <- newIORef False
      let enforceOnce :: m a -> m a
          enforceOnce m =
            atomicModifyIORef usedRef (True,) >>= \case
              False -> m
              True -> throwIO $ mkUserError "This channel has already been used!"
      pure
        Channel
          { sendMessage = \tok msg -> enforceOnce do
              Driver.sendMessage driver tok msg
              go state
          , recvMessage = \tok -> enforceOnce do
              (SomeMessage msg, state') <- Driver.recvMessage driver tok state
              channel' <- go state'
              pure $ SomeMessageAndChannel msg channel'
          }

runPeerTOverSocket
  :: (MonadUnliftIO m, BinaryMessage ps)
  => NobodyHasAgency j
  -> Socket
  -> PeerT pr ps i j m a
  -> m (Untyped.Driver m, m a)
runPeerTOverSocket tok socket peer = do
  (channel, untypedDriver) <- tcpChannel socket
  pure (untypedDriver, runPeerT' tok peer channel)

type ClientTConnector ps i j = Connector (ClientT ps i j)

type ClientTConnectorTraced ps i j = ConnectorTraced (ClientT ps i j)

data TcpClientSelector sub f where
  Connect :: String -> TcpClientSelector sub AddrInfo
  ClientPeer
    :: String
    -> AddrInfo
    -> sub f
    -> TcpClientSelector sub f
  CloseClient :: TcpClientSelector ps Void

tcpClientPeerT
  :: (MonadUnliftIO m, BinaryMessage ps)
  => NobodyHasAgency j
  -> HostName
  -> PortNumber
  -> ClientTConnector ps i j m
tcpClientPeerT tok host port = Connector $ liftIO $ do
  addr <-
    head
      <$> getAddrInfo
        (Just defaultHints{addrSocketType = Stream})
        (Just host)
        (Just $ show port)
  socket <- openSocket addr
  connect socket $ addrAddress addr
  pure $ Connection $ snd <=< runPeerTOverSocket tok socket

tcpClientPeerTTraced
  :: (MonadUnliftIO m, MonadEvent r root m, BinaryMessage ps)
  => String
  -> NobodyHasAgency j
  -> InjectSelector (TcpClientSelector sub) root
  -> HostName
  -> PortNumber
  -> ClientTConnectorTraced ps i j sub root m
tcpClientPeerTTraced name tok inj host port = ConnectorTraced $
  withInjectEvent inj (Connect name) \ev -> do
    addr <-
      liftIO $
        head
          <$> getAddrInfo
            (Just defaultHints{addrSocketType = Stream})
            (Just host)
            (Just $ show port)
    addField ev addr
    socket <- liftIO $ openSocket addr
    liftIO $ connect socket $ addrAddress addr
    pure $ ConnectionTraced \mkPeer -> localBackend (setInitialCauseEventBackend [reference ev]) do
      snd
        =<< runPeerTOverSocket
          tok
          socket
          (mkPeer $ composeInjectSelector inj $ injectSelector $ ClientPeer name addr)

type ServerTSource ps i j = ServerSourceTraced (ServerT ps i j)

data TcpServerPeerTDependencies ps sub root m = forall (i :: ps) (j :: ps).
  TcpServerPeerTDependencies
  { host :: HostName
  , port :: PortNumber
  , nobodyHasAgency :: NobodyHasAgency j
  , serverSource :: ServerTSource ps i j sub root m ()
  }

data TcpServerSelector sub f where
  Connected :: String -> TcpServerSelector sub ConnectedField
  ServerPeer
    :: String
    -> AddrInfo
    -> SockAddr
    -> sub f
    -> TcpServerSelector sub f
  CloseServer :: TcpServerSelector ps Void

tcpServerPeerT
  :: (MonadUnliftIO m, BinaryMessage ps, C.WithLog env C.Message m, MonadEvent r root m)
  => String
  -> InjectSelector (TcpServerSelector sub) root
  -> Component m (TcpServerPeerTDependencies ps sub root m) ()
tcpServerPeerT name inj = component_ (name <> "-tcp-server") \TcpServerPeerTDependencies{..} -> do
  C.logInfo $ T.pack $ name <> " server starting on port " <> show port
  withRunInIO \runInIO ->
    runTCPServer (Just host) (show port) $
      runComponent_ $
        hoistComponent runInIO $
          component_ (name <> "-tcp-worker") \socket -> runResourceT do
            (pName, server, r) <- withInjectEvent inj (Connected name) \ev -> do
              addr <-
                liftIO $
                  head
                    <$> getAddrInfo
                      (Just defaultHints{addrSocketType = Stream, addrFlags = [AI_PASSIVE]})
                      (Just host)
                      (Just $ show port)
              addField ev $ ConnectedAddr addr
              pName <- liftIO $ getPeerName socket
              addField ev $ ConnectedPeer pName
              server <-
                getServerTraced serverSource $
                  composeInjectSelector inj $
                    injectSelector (ServerPeer name addr pName)
              lift $
                C.logInfo $
                  T.pack $
                    name <> " server received new connection from " <> show pName
              pure (pName, server, reference ev)
            lift $ localBackend (setInitialCauseEventBackend [r]) do
              mask \restore -> do
                result <- try $ restore $ uncurry rethrowErrors =<< runPeerTOverSocket nobodyHasAgency socket server
                withInjectEvent inj CloseServer \ev -> do
                  C.logInfo $ T.pack $ name <> " server disconnected from " <> show pName
                  case result of
                    Left ex -> do
                      finalize ev $ Just ex
                      throwIO ex
                    Right a -> pure a

renderTcpServerSelectorOTel :: RenderSelectorOTel sub -> RenderSelectorOTel (TcpServerSelector sub)
renderTcpServerSelectorOTel renderSub = \case
  Connected name ->
    OTelRendered
      { eventName = "tcp/connected " <> T.pack name
      , eventKind = Consumer
      , renderField = \case
          ConnectedAddr addr -> ("net.protocol.name", fromString name) : addrInfoToAttributes addr
          ConnectedPeer peer -> sockAddrToAttributes True peer
      }
  ServerPeer name addr peer sel -> case renderSub sel of
    OTelRendered{..} ->
      OTelRendered
        { renderField = \f ->
            (("net.protocol.name", fromString name) : addrInfoToAttributes addr)
              <> sockAddrToAttributes True peer
              <> renderField f
        , ..
        }
  CloseServer ->
    OTelRendered
      { eventName = "tcp/close"
      , eventKind = Producer
      , renderField = \case {}
      }

renderTcpClientSelectorOTel :: RenderSelectorOTel sub -> RenderSelectorOTel (TcpClientSelector sub)
renderTcpClientSelectorOTel renderSub = \case
  Connect name ->
    OTelRendered
      { eventName = "tcp/connect " <> T.pack name
      , eventKind = Consumer
      , renderField = \addr -> ("net.protocol.name", fromString name) : addrInfoToAttributes addr
      }
  ClientPeer name addr sel -> case renderSub sel of
    OTelRendered{..} ->
      OTelRendered
        { renderField = \f -> (("net.protocol.name", fromString name) : addrInfoToAttributes addr) <> renderField f
        , ..
        }
  CloseClient ->
    OTelRendered
      { eventName = "tcp/close"
      , eventKind = Producer
      , renderField = \case {}
      }
