{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.Indexer
  where

import Control.Concurrent.Component
import Data.Set.NonEmpty (NESet)
import Data.Time (NominalDiffTime)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery, RuntimeChainSeekClient, ScriptHash)
import Language.Marlowe.Runtime.Indexer.ChainSeekClient
import Language.Marlowe.Runtime.Indexer.Database (DatabaseQueries)
import Language.Marlowe.Runtime.Indexer.Store
import Network.Protocol.Driver (SomeClientConnector)
import Network.Protocol.Query.Client (QueryClient)
import Observe.Event (EventBackend, narrowEventBackend)
import Observe.Event.Component (FieldConfig(..), GetSelectorConfig, SelectorConfig(..), SomeJSON(SomeJSON), prependKey)

data MarloweIndexerSelector f where
  StoreEvent :: StoreSelector f -> MarloweIndexerSelector f
  ChainSeekClientEvent :: ChainSeekClientSelector f -> MarloweIndexerSelector f

data MarloweIndexerDependencies r = MarloweIndexerDependencies
  { eventBackend :: EventBackend IO r MarloweIndexerSelector
  , databaseQueries :: DatabaseQueries IO
  , chainSyncConnector :: SomeClientConnector RuntimeChainSeekClient IO
  , chainSyncQueryConnector :: SomeClientConnector (QueryClient ChainSyncQuery) IO
  , pollingInterval :: NominalDiffTime
  , marloweScriptHashes :: NESet ScriptHash
  , payoutScriptHashes :: NESet ScriptHash
  }

marloweIndexer :: Component IO (MarloweIndexerDependencies r) ()
marloweIndexer = proc MarloweIndexerDependencies{..} -> do
  pullEvent <- chainSeekClient -< ChainSeekClientDependencies
    { eventBackend = narrowEventBackend ChainSeekClientEvent eventBackend
    , ..
    }
  store -< StoreDependencies
    { eventBackend = narrowEventBackend StoreEvent eventBackend
    , ..
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
