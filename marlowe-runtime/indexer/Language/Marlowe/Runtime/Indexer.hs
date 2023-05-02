{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.Indexer
  where

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
import Network.Protocol.Peer.Trace (HasSpanContext)
import Network.Protocol.Query.Client (QueryClient)
import Observe.Event.Component (FieldConfig(..), GetSelectorConfig, SelectorConfig(..), SomeJSON(SomeJSON), prependKey)
import UnliftIO (MonadUnliftIO)

data MarloweIndexerSelector f where
  StoreEvent :: StoreSelector f -> MarloweIndexerSelector f
  ChainSeekClientEvent :: ChainSeekClientSelector f -> MarloweIndexerSelector f

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
     , Inject ChainSeekClientSelector s
     , MonadFail m
     , HasSpanContext r
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

getMarloweIndexerSelectorConfig :: GetSelectorConfig MarloweIndexerSelector
getMarloweIndexerSelectorConfig = \case
  StoreEvent sel -> prependKey "store" $ getStoreSelectorConfig sel
  ChainSeekClientEvent sel -> prependKey "chain-sync-client" $ getChainSeekClientSelectorConfig sel

getStoreSelectorConfig :: GetSelectorConfig StoreSelector
getStoreSelectorConfig = \case
  Save -> SelectorConfig "save" True FieldConfig
    { fieldKey = \case
        RollbackPoint _ -> "rollback-point"
        Stats _ -> "stats"
        LocalTip _ -> "local-tip"
        RemoteTip _ -> "remote-tip"
        InvalidCreateTxs _ -> "invalid-create-txs"
        InvalidApplyInputsTxs _ -> "invalid-apply-inputs-txs"
    , fieldDefaultEnabled = \case
        RollbackPoint _ -> True
        Stats _ -> True
        LocalTip _ -> True
        RemoteTip _ -> True
        InvalidCreateTxs _ -> True
        InvalidApplyInputsTxs _ -> True
    , toSomeJSON = \case
        RollbackPoint point -> SomeJSON point
        Stats stats -> SomeJSON stats
        LocalTip point -> SomeJSON point
        RemoteTip point -> SomeJSON point
        InvalidCreateTxs errs -> SomeJSON errs
        InvalidApplyInputsTxs errs -> SomeJSON errs
    }

getChainSeekClientSelectorConfig :: GetSelectorConfig ChainSeekClientSelector
getChainSeekClientSelectorConfig = \case
  LoadMarloweUTxO -> SelectorConfig "load-marlowe-utxo" True FieldConfig
    { fieldKey = const "marlowe-utxo"
    , fieldDefaultEnabled = const True
    , toSomeJSON = SomeJSON
    }
