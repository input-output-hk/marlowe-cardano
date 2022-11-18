{-# LANGUAGE RankNTypes #-}
module Language.Marlowe.Runtime.Discovery
  where

import Control.Concurrent.Async (Concurrently(Concurrently, runConcurrently))
import Control.Concurrent.STM (STM)
import Data.Void (Void)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Discovery.Chain
import Language.Marlowe.Runtime.Discovery.QueryServer
import Language.Marlowe.Runtime.Discovery.Store
import Language.Marlowe.Runtime.Discovery.SyncServer
import Network.Protocol.Driver (RunClient)
import Numeric.Natural (Natural)

data DiscoveryDependencies = DiscoveryDependencies
  { acceptRunSyncServer :: IO (RunSyncServer IO)
  , acceptRunQueryServer :: IO (RunQueryServer IO)
  , connectToChainSeek :: RunClient IO Chain.RuntimeChainSeekClient
  , pageSize :: Natural
  }

newtype Discovery = Discovery
  { runDiscovery :: IO Void
  }

mkDiscovery :: DiscoveryDependencies -> STM Discovery
mkDiscovery DiscoveryDependencies{..} = do
  DiscoveryChainClient{..} <- mkDiscoveryChainClient DiscoveryChainClientDependencies{..}
  DiscoveryStore{..} <- mkDiscoveryStore DiscoveryStoreDependencies{..}
  DiscoverySyncServer{..} <- mkDiscoverySyncServer DiscoverySyncServerDependencies{..}
  DiscoveryQueryServer{..} <- mkDiscoveryQueryServer DiscoveryQueryServerDependencies{..}
  pure
    $  Discovery
    $  runConcurrently
    $  Concurrently runDiscoveryChainClient
    *> Concurrently runDiscoveryStore
    *> Concurrently runDiscoverySyncServer
    *> Concurrently runDiscoveryQueryServer
