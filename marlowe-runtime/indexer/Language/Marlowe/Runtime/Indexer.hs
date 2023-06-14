{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.Indexer where

import Colog (Message, WithLog)
import Control.Arrow (returnA)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes
import Control.Concurrent.STM (atomically)
import Control.Monad.Event.Class (Inject, MonadEvent)
import Data.Set.NonEmpty (NESet)
import Data.Time (NominalDiffTime)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery, RuntimeChainSeekClient, ScriptHash)
import Language.Marlowe.Runtime.Indexer.ChainSeekClient
import Language.Marlowe.Runtime.Indexer.Database (DatabaseQueries)
import Language.Marlowe.Runtime.Indexer.Store
import Network.Protocol.Connection (SomeClientConnectorTraced)
import Network.Protocol.Driver.Trace (HasSpanContext)
import Network.Protocol.Query.Client (QueryClient)
import UnliftIO (MonadUnliftIO)

data MarloweIndexerSelector r f where
  StoreEvent :: StoreSelector f -> MarloweIndexerSelector r f
  ChainSeekClientEvent :: ChainSeekClientSelector r f -> MarloweIndexerSelector r f

data MarloweIndexerDependencies r s m = MarloweIndexerDependencies
  { databaseQueries :: DatabaseQueries m
  , chainSyncConnector :: SomeClientConnectorTraced RuntimeChainSeekClient r s m
  , chainSyncQueryConnector :: SomeClientConnectorTraced (QueryClient ChainSyncQuery) r s m
  , pollingInterval :: NominalDiffTime
  , marloweScriptHashes :: NESet ScriptHash
  , payoutScriptHashes :: NESet ScriptHash
  }

marloweIndexer
  :: ( MonadEvent r s m
     , MonadUnliftIO m
     , Inject StoreSelector s
     , Inject (ChainSeekClientSelector r) s
     , MonadFail m
     , HasSpanContext r
     , WithLog env Message m
     )
  => Component m (MarloweIndexerDependencies r s m) Probes
marloweIndexer = proc MarloweIndexerDependencies{..} -> do
  (connected, pullEvent) <- chainSeekClient -< ChainSeekClientDependencies {..}
  store -< StoreDependencies {..}
  returnA -< Probes
    { startup = pure True
    , liveness = atomically connected
    , readiness = pure True
    }
