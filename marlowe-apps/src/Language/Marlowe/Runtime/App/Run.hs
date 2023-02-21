{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}


module Language.Marlowe.Runtime.App.Run
  ( runChainSeekClient
  , runClientWithConfig
  , runJobClient
  , runMarloweHeaderSyncClient
  , runMarloweSyncClient
  , runQueryClient
  ) where


import Control.Monad.Trans.Control (liftBaseWith)
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Language.Marlowe.Protocol.HeaderSync.Client
  (MarloweHeaderSyncClient, hoistMarloweHeaderSyncClient, marloweHeaderSyncClientPeer)
import Language.Marlowe.Protocol.Query.Client (MarloweQueryClient, hoistMarloweQueryClient, marloweQueryClientPeer)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient, hoistMarloweSyncClient, marloweSyncClientPeer)
import Language.Marlowe.Runtime.App.Types (Client(..), Config(..), Services(..))
import Language.Marlowe.Runtime.ChainSync.Api (RuntimeChainSeekClient, WithGenesis(Genesis))
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer, hoistChainSeekClient)
import Network.Protocol.Driver (runConnector, tcpClient)
import Network.Protocol.Handshake.Client (handshakeClientConnector)
import Network.Protocol.Job.Client (JobClient, hoistJobClient, jobClientPeer)


runQueryClient
  :: (Services IO -> MarloweQueryClient IO a -> IO a)
  -> MarloweQueryClient Client a
  -> Client a
runQueryClient query client =
  do
    services <- Client ask
    liftBaseWith $ \runInBase -> query services $ hoistMarloweQueryClient runInBase client


runJobClient
  :: (Services IO -> JobClient q IO a -> IO a)
  -> JobClient q Client a
  -> Client a
runJobClient job client =
  do
    services <- Client ask
    liftBaseWith $ \runInBase -> job services $ hoistJobClient runInBase client


runChainSeekClient
  :: (Services IO -> RuntimeChainSeekClient IO a -> IO a)
  -> RuntimeChainSeekClient Client a
  -> Client a
runChainSeekClient seek client =
  do
    services <- Client ask
    liftBaseWith $ \runInBase -> seek services $ hoistChainSeekClient runInBase client


runMarloweSyncClient
  :: (Services IO -> MarloweSyncClient IO a -> IO a)
  -> MarloweSyncClient Client a
  -> Client a
runMarloweSyncClient sync client =
  do
    services <- Client ask
    liftBaseWith $ \runInBase -> sync services $ hoistMarloweSyncClient runInBase client


runMarloweHeaderSyncClient
  :: (Services IO -> MarloweHeaderSyncClient IO a -> IO a)
  -> MarloweHeaderSyncClient Client a
  -> Client a
runMarloweHeaderSyncClient sync client =
  do
    services <- Client ask
    liftBaseWith $ \runInBase -> sync services $ hoistMarloweHeaderSyncClient runInBase client


runClientWithConfig
  :: Config
  -> Client a
  -> IO a
runClientWithConfig Config{..} client = runReaderT (runClient client) Services
  { runChainSeekCommandClient = runConnector $ handshakeClientConnector $ tcpClient chainSeekHost chainSeekCommandPort jobClientPeer
  , runChainSeekSyncClient = runConnector $ handshakeClientConnector $ tcpClient chainSeekHost chainSeekSyncPort (chainSeekClientPeer Genesis)
  , runSyncSyncClient = runConnector $ handshakeClientConnector $ tcpClient syncHost syncSyncPort marloweSyncClientPeer
  , runSyncHeaderClient = runConnector $ handshakeClientConnector $ tcpClient syncHost syncHeaderPort marloweHeaderSyncClientPeer
  , runSyncQueryClient = runConnector $ handshakeClientConnector $ tcpClient syncHost syncQueryPort marloweQueryClientPeer
  , runTxCommandClient = runConnector $ handshakeClientConnector $ tcpClient txHost txCommandPort jobClientPeer
  }
