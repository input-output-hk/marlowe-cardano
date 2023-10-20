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
import qualified Data.Text as T
import Data.Void (Void)
import GHC.IO (mkUserError)
import Network.Channel (socketAsChannel)
import Network.Protocol.Codec (BinaryMessage)
import Network.Protocol.Connection (Connection (..), Connector (..), ServerSourceTraced (..))
import Network.Protocol.Driver (mkDriver, rethrowErrors)
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

type ServerTSource ps i j = ServerSourceTraced (ServerT ps i j)

data TcpServerPeerTDependencies ps r sub root m = forall (i :: ps) (j :: ps).
  TcpServerPeerTDependencies
  { host :: HostName
  , port :: PortNumber
  , nobodyHasAgency :: NobodyHasAgency j
  , serverSource :: ServerTSource ps i j r sub root m ()
  }

data TcpServerSelector sub f where
  Connected :: TcpServerSelector sub ConnectedField
  ServerPeer
    :: AddrInfo
    -> SockAddr
    -> sub f
    -> TcpServerSelector sub f
  CloseServer :: TcpServerSelector ps Void

data ConnectedField
  = ConnectedAddr AddrInfo
  | ConnectedPeer SockAddr

tcpServerPeerT
  :: (MonadUnliftIO m, BinaryMessage ps, C.WithLog env C.Message m, MonadEvent r root m)
  => String
  -> InjectSelector (TcpServerSelector sub) root
  -> Component m (TcpServerPeerTDependencies ps r sub root m) ()
tcpServerPeerT name inj = component_ (name <> "-tcp-server") \TcpServerPeerTDependencies{..} -> do
  C.logInfo $ T.pack $ name <> " server starting on port " <> show port
  withRunInIO \runInIO ->
    runTCPServer (Just host) (show port) $
      runComponent_ $
        hoistComponent runInIO $
          component_ (name <> "-tcp-worker") \socket -> runResourceT do
            (pName, server, r) <- withInjectEvent inj Connected \ev -> do
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
                    injectSelector (ServerPeer addr pName)
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
