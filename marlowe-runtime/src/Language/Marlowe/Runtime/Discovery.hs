{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Marlowe.Runtime.Discovery
  where

import Control.Concurrent.Component
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Discovery.Chain
import Language.Marlowe.Runtime.Discovery.QueryServer
import Language.Marlowe.Runtime.Discovery.Store
import Language.Marlowe.Runtime.Discovery.SyncServer
import Network.Protocol.Driver (RunClient)
import Numeric.Natural (Natural)
import Observe.Event (EventBackend)
import Observe.Event.Backend (narrowEventBackend)
import Observe.Event.DSL (SelectorField(..), SelectorSpec(..))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))

compile $ SelectorSpec "discovery"
  [ ["chain", "client"] ≔ Inject ''DiscoveryChainClientSelector
  , "store" ≔ Inject ''DiscoveryStoreSelector
  , ["sync", "server"] ≔ Inject ''DiscoverySyncServerSelector
  , ["query", "server"] ≔ Inject ''DiscoveryQueryServerSelector
  ]

data DiscoveryDependencies r = DiscoveryDependencies
  { acceptRunSyncServer :: IO (RunSyncServer IO)
  , acceptRunQueryServer :: IO (RunQueryServer IO)
  , connectToChainSeek :: RunClient IO Chain.RuntimeChainSeekClient
  , pageSize :: Natural
  , eventBackend :: EventBackend IO r DiscoverySelector
  }

discovery :: Component IO (DiscoveryDependencies r) ()
discovery = proc DiscoveryDependencies
  { acceptRunSyncServer
  , acceptRunQueryServer
  , connectToChainSeek
  , pageSize
  , eventBackend = rootBackend
  } -> do
    chainEvents <- discoveryChainClient -<
      let eventBackend = narrowEventBackend ChainClient rootBackend in DiscoveryChainClientDependencies{..}
    DiscoveryStore{..} <- discoveryStore -<
      let eventBackend = narrowEventBackend Store rootBackend in DiscoveryStoreDependencies{..}
    discoverySyncServer -<
      let eventBackend = narrowEventBackend SyncServer rootBackend in DiscoverySyncServerDependencies{..}
    discoveryQueryServer -<
      let eventBackend = narrowEventBackend QueryServer rootBackend in DiscoveryQueryServerDependencies{..}
