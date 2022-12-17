

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}


module Language.Marlowe.Runtime.Client.Run
  ( runChainSeekClient
  , runClientWithConfig
  , runJobClient
  , runMarloweSyncClient
  , runQueryClient
  ) where


import Control.Exception (Exception, bracket, bracketOnError, throwIO)
import Control.Monad.Trans.Control (liftBaseWith)
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Data.ByteString.Lazy (ByteString)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient, hoistMarloweSyncClient, marloweSyncClientPeer)
import Language.Marlowe.Protocol.Sync.Codec (codecMarloweSync)
import Language.Marlowe.Runtime.ChainSync.Api (RuntimeChainSeekClient, WithGenesis(Genesis))
import Language.Marlowe.Runtime.Client.Types (Client(..), Config(..), RunClient, Services(..))
import Network.Channel (socketAsChannel)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer, hoistChainSeekClient)
import Network.Protocol.ChainSeek.Codec (codecChainSeek)
import Network.Protocol.Driver (mkDriver)
import Network.Protocol.Job.Client (JobClient, hoistJobClient, jobClientPeer)
import Network.Protocol.Job.Codec (codecJob)
import Network.Protocol.Query.Client (QueryClient, hoistQueryClient, queryClientPeer)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Socket
  (AddrInfo, SocketType(..), addrAddress, addrSocketType, close, connect, defaultHints, getAddrInfo, openSocket)
import Network.TypedProtocol (Driver(startDState), Peer, PeerRole(..), runPeerWithDriver)
import Network.TypedProtocol.Codec (Codec)


runChainSeekClient
  :: (Services IO -> RuntimeChainSeekClient IO a -> IO a)
  -> RuntimeChainSeekClient Client a
  -> Client a
runChainSeekClient seek client =
  do
    services <- Client ask
    liftBaseWith $ \runInBase -> seek services $ hoistChainSeekClient runInBase client


runJobClient
  :: (Services IO -> JobClient q IO a -> IO a)
  -> JobClient q Client a
  -> Client a
runJobClient job client =
  do
    services <- Client ask
    liftBaseWith $ \runInBase -> job services $ hoistJobClient runInBase client


runQueryClient
  :: (Services IO -> QueryClient q IO a -> IO a)
  -> QueryClient q Client a
  -> Client a
runQueryClient query client =
  do
    services <- Client ask
    liftBaseWith $ \runInBase -> query services $ hoistQueryClient runInBase client


runMarloweSyncClient
  :: (Services IO -> MarloweSyncClient IO a -> IO a)
  -> MarloweSyncClient Client a
  -> Client a
runMarloweSyncClient sync client =
  do
    services <- Client ask
    liftBaseWith $ \runInBase -> sync services $ hoistMarloweSyncClient runInBase client


runClientWithConfig
  :: Config
  -> Client a
  -> IO a
runClientWithConfig Config{..} client = do
  chainSeekAddr <- resolve chainSeekHost chainSeekPort
  syncCommandAddr <- resolve chainSeekHost chainSeekCommandPort
  historyJobAddr <- resolve historyHost historyCommandPort
  historyQueryAddr <- resolve historyHost historyQueryPort
  historySyncAddr <- resolve historyHost historySyncPort
  discoveryQueryAddr <- resolve discoveryHost discoveryQueryPort
  txJobAddr <- resolve txHost txCommandPort
  runReaderT (runClient client) Services
    { runSyncClient = runClientPeerOverSocket chainSeekAddr codecChainSeek (chainSeekClientPeer Genesis)
    , runSyncCommandClient = runClientPeerOverSocket syncCommandAddr codecJob jobClientPeer
    , runHistoryJobClient = runClientPeerOverSocket historyJobAddr codecJob jobClientPeer
    , runHistoryQueryClient = runClientPeerOverSocket historyQueryAddr codecQuery queryClientPeer
    , runHistorySyncClient = runClientPeerOverSocket historySyncAddr codecMarloweSync marloweSyncClientPeer
    , runTxJobClient = runClientPeerOverSocket txJobAddr codecJob jobClientPeer
    , runDiscoveryQueryClient = runClientPeerOverSocket discoveryQueryAddr codecQuery queryClientPeer
    }
  where
    resolve host port =
      head <$> getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just host) (Just $ show port)


-- | Run a client as a typed protocols peer over a socket.
runClientPeerOverSocket
  :: Exception ex
  => AddrInfo -- ^ Socket address to connect to
  -> Codec protocol ex IO ByteString -- ^ A codec for the protocol
  -> (forall a. client IO a -> Peer protocol 'AsClient st IO a) -- ^ Interpret the client as a protocol peer
  -> RunClient IO client
runClientPeerOverSocket addr codec clientToPeer client = bracket open close $ \socket -> do
  let channel = socketAsChannel socket
  let driver = mkDriver throwIO codec channel
  let peer = clientToPeer client
  fst <$> runPeerWithDriver driver peer (startDState driver)
  where
    open = bracketOnError (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      pure sock
