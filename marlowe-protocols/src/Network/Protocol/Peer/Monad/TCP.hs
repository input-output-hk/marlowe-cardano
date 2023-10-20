{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Peer.Monad.TCP where

import qualified Colog as C
import Control.Concurrent.Component
import Control.Monad ((<=<))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Resource (runResourceT)
import GHC.IO (mkUserError)
import Network.Channel (socketAsChannel)
import Network.Protocol.Codec (BinaryMessage)
import Network.Protocol.Connection (Connection (..), Connector (..), ServerSource (..))
import Network.Protocol.Driver (mkDriver, rethrowErrors)
import qualified Network.Protocol.Driver.Untyped as Untyped
import Network.Protocol.Peer.Monad (Channel (..), ClientT, PeerT, ServerT, SomeMessageAndChannel (..), runPeerT')
import Network.Run.TCP (runTCPServer)
import Network.Socket (
  AddrInfo (..),
  HostName,
  PortNumber,
  Socket,
  SocketType (..),
  connect,
  defaultHints,
  getAddrInfo,
  openSocket,
 )
import Network.TypedProtocol (Driver, Protocol (..), SomeMessage (..))
import qualified Network.TypedProtocol as Driver
import UnliftIO (MonadIO (..), MonadUnliftIO (..), atomicModifyIORef, newIORef, throwIO)

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

tcpClientPeerT
  :: (MonadUnliftIO m, BinaryMessage ps)
  => NobodyHasAgency j
  -> HostName
  -> PortNumber
  -> Connector (ClientT ps i j) m
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

type ServerTSource ps i j = ServerSource (ServerT ps i j)

data TcpServerPeerTDependencies ps m = forall (i :: ps) (j :: ps).
  TcpServerPeerTDependencies
  { host :: HostName
  , port :: PortNumber
  , nobodyHasAgency :: NobodyHasAgency j
  , serverSource :: ServerTSource ps i j m ()
  }

tcpServerPeerT
  :: (MonadUnliftIO m, BinaryMessage ps, C.WithLog env C.Message m)
  => String
  -> Component m (TcpServerPeerTDependencies ps m) ()
tcpServerPeerT name = component_ (name <> "-tcp-server") \TcpServerPeerTDependencies{..} ->
  withRunInIO \runInIO ->
    runTCPServer (Just host) (show port) $
      runComponent_ $
        hoistComponent runInIO $
          component_ (name <> "-tcp-worker") \socket -> runResourceT do
            server <- getServer serverSource
            lift $ uncurry rethrowErrors =<< runPeerTOverSocket nobodyHasAgency socket server
