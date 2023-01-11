{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.Indexer
  where

import Control.Concurrent.Component
import Language.Marlowe.Runtime.ChainSync.Api (RuntimeChainSeekClient)
import Language.Marlowe.Runtime.Indexer.ChainSeekClient
import Language.Marlowe.Runtime.Indexer.Database (DatabaseQueries)
import Language.Marlowe.Runtime.Indexer.Store
import Network.Protocol.Driver (RunClient)
import Observe.Event (EventBackend, narrowEventBackend)

data MarloweIndexerSelector f where
  StoreEvent :: StoreSelector f -> MarloweIndexerSelector f

data MarloweIndexerDependencies r = MarloweIndexerDependencies
  { eventBackend :: EventBackend IO r MarloweIndexerSelector
  , databaseQueries :: DatabaseQueries IO
  , runChainSeekClient :: RunClient IO RuntimeChainSeekClient
  }

marloweIndexer :: Component IO (MarloweIndexerDependencies r) ()
marloweIndexer = proc MarloweIndexerDependencies{..} -> do
  pullEvent <- chainSeekClient -< ChainSeekClientDependencies{..}
  store -< StoreDependencies
    { databaseQueries
    , pullEvent
    , eventBackend = narrowEventBackend StoreEvent eventBackend
    }
