{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Logging
  ( RootSelector(..)
  , getRootSelectorConfig
  ) where

import Cardano.Api (ChainPoint(..), ChainTip(..))
import Data.Aeson (Value(..), object, (.=))
import Language.Marlowe.Runtime.Cardano.Api
  (fromCardanoBlockHeader, fromCardanoBlockHeaderHash, fromCardanoBlockNo, fromCardanoSlotNo)
import Language.Marlowe.Runtime.ChainSync (ChainSyncSelector(..))
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand, ChainSyncQuery, RuntimeChainSeek)
import qualified Language.Marlowe.Runtime.ChainSync.NodeClient as NodeClient
import qualified Language.Marlowe.Runtime.ChainSync.Store as ChainStore
import Network.Protocol.Driver
  (AcceptSocketDriverSelector(..), AcceptSocketDriverSelectorConfigOptions(..), getAcceptSocketDriverSelectorConfig)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Query.Types (Query)
import Observe.Event.Component
  ( ConfigWatcherSelector(..)
  , FieldConfig(..)
  , GetSelectorConfig
  , SelectorConfig(..)
  , SomeJSON(..)
  , absurdFieldConfig
  , prependKey
  , singletonFieldConfig
  , singletonFieldConfigWith
  )

data RootSelector f where
  ChainSeekServer :: AcceptSocketDriverSelector RuntimeChainSeek f -> RootSelector f
  QueryServer :: AcceptSocketDriverSelector (Query ChainSyncQuery) f -> RootSelector f
  JobServer :: AcceptSocketDriverSelector (Job ChainSyncCommand) f -> RootSelector f
  App :: ChainSyncSelector f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

-- TODO automate this boilerplate with Template Haskell
getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  ChainSeekServer sel -> prependKey "chain-seek" $ getAcceptSocketDriverSelectorConfig chainSeekConfig sel
  QueryServer sel -> prependKey "query" $ getAcceptSocketDriverSelectorConfig queryConfig sel
  JobServer sel -> prependKey "job" $ getAcceptSocketDriverSelectorConfig jobConfig sel
  App sel -> getChainSyncSelectorConfig sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True
    $ singletonFieldConfig "config" True

chainSeekConfig :: AcceptSocketDriverSelectorConfigOptions
chainSeekConfig = AcceptSocketDriverSelectorConfigOptions
  { enableConnected = True
  , enableDisconnected = True
  , enableServerDriverEvent = False
  }

queryConfig :: AcceptSocketDriverSelectorConfigOptions
queryConfig = AcceptSocketDriverSelectorConfigOptions
  { enableConnected = True
  , enableDisconnected = True
  , enableServerDriverEvent = True
  }

jobConfig :: AcceptSocketDriverSelectorConfigOptions
jobConfig = AcceptSocketDriverSelectorConfigOptions
  { enableConnected = True
  , enableDisconnected = True
  , enableServerDriverEvent = True
  }

getChainSyncSelectorConfig :: GetSelectorConfig ChainSyncSelector
getChainSyncSelectorConfig = \case
  NodeClientEvent sel -> prependKey "node-client" $ getNodeClientSelectorConfig sel
  ChainStoreEvent sel -> prependKey "chain-store" $ getChainStoreSelectorConfig sel

-- defaultLogConfig :: LogConfig
-- defaultLogConfig = LogConfig $ Just <$> Map.fromList
--   [ ("chain-seek.connected", mempty)
--   , ("chain-seek.disconnected", mempty)
--   , ("query.connected", mempty)
--   , ("query.disconnected", mempty)
--   , ("query.send", Map.singleton "message" True)
--   , ("query.recv", Map.singleton "message" True)
--   , ("job.connected", mempty)
--   , ("job.disconnected", mempty)
--   , ("job.send", Map.singleton "message" True)
--   , ("job.recv", Map.singleton "message" True)
--   , ("reload-log-config", Map.singleton "config" True)
--   , ("node-client.connect", mempty)
--   , ("node-client.intersect", mempty)
--   , ("node-client.intersect-found", Map.singleton "point" True)
--   , ("node-client.intersect-not-found", mempty)
--   , ("node-client.roll-backward", Map.fromList [("point", True), ("tip", True)])
--   , ("chain-store.check-genesis-block", mempty)
--   , ("chain-store.save", Map.fromList
--       [ ("rollback-point", True)
--       , ("block-count", True)
--       , ("local-tip", True)
--       , ("remote-tip", True)
--       , ("tx-count", True)
--       ]
--     )
--   ]

getNodeClientSelectorConfig :: GetSelectorConfig NodeClient.NodeClientSelector
getNodeClientSelectorConfig = \case
  NodeClient.Connect -> SelectorConfig "connect" True absurdFieldConfig
  NodeClient.Intersect -> SelectorConfig "intersect" True
    $ singletonFieldConfigWith (SomeJSON . fmap pointToJSON) "points" False
  NodeClient.IntersectFound -> SelectorConfig "intersect-found" True
    $ singletonFieldConfigWith (SomeJSON . pointToJSON) "point" True
  NodeClient.IntersectNotFound -> SelectorConfig "intersect-not-found" True absurdFieldConfig
  NodeClient.RollForward -> SelectorConfig "roll-forward" False FieldConfig
    { fieldKey = \case
        NodeClient.RollForwardBlock _ -> "block-header"
        NodeClient.RollForwardTip _ -> "tip"
        NodeClient.RollForwardNewCost _ -> "new-cost"
    , fieldDefaultEnabled = \case
        NodeClient.RollForwardBlock _ -> True
        NodeClient.RollForwardTip _ -> True
        NodeClient.RollForwardNewCost _ -> False
    , toSomeJSON = \case
        NodeClient.RollForwardBlock header -> SomeJSON $ fromCardanoBlockHeader header
        NodeClient.RollForwardTip tip -> SomeJSON $ tipToJSON tip
        NodeClient.RollForwardNewCost cost -> SomeJSON cost
    }
  NodeClient.RollBackward -> SelectorConfig "roll-backward" True FieldConfig
    { fieldKey = \case
        NodeClient.RollBackwardPoint _ -> "point"
        NodeClient.RollBackwardTip _ -> "tip"
        NodeClient.RollBackwardNewCost _ -> "new-cost"
    , fieldDefaultEnabled = \case
        NodeClient.RollBackwardPoint _ -> True
        NodeClient.RollBackwardTip _ -> True
        NodeClient.RollBackwardNewCost _ -> False
    , toSomeJSON = \case
        NodeClient.RollBackwardPoint point -> SomeJSON $ pointToJSON point
        NodeClient.RollBackwardTip tip -> SomeJSON $ tipToJSON tip
        NodeClient.RollBackwardNewCost cost -> SomeJSON cost
    }

getChainStoreSelectorConfig :: GetSelectorConfig ChainStore.ChainStoreSelector
getChainStoreSelectorConfig = \case
  ChainStore.CheckGenesisBlock -> SelectorConfig "check-genesis-block" True absurdFieldConfig
  ChainStore.Save -> SelectorConfig "save" True FieldConfig
    { fieldKey = \case
        ChainStore.RollbackPoint _ -> "rollback-point"
        ChainStore.BlockCount _ -> "block-count"
        ChainStore.LocalTip _ -> "local-tip"
        ChainStore.RemoteTip _ -> "remote-tip"
        ChainStore.TxCount _ -> "tx-count"
    , fieldDefaultEnabled = const True
    , toSomeJSON = \case
        ChainStore.RollbackPoint point -> SomeJSON $ pointToJSON point
        ChainStore.BlockCount count -> SomeJSON count
        ChainStore.LocalTip tip -> SomeJSON $ tipToJSON tip
        ChainStore.RemoteTip tip -> SomeJSON $ tipToJSON tip
        ChainStore.TxCount count -> SomeJSON count
    }

pointToJSON :: ChainPoint -> Value
pointToJSON = \case
  ChainPointAtGenesis -> String "genesis"
  ChainPoint slotNo hash -> object
    [ "slotNo" .= fromCardanoSlotNo slotNo
    , "hash" .= fromCardanoBlockHeaderHash hash
    ]

tipToJSON :: ChainTip -> Value
tipToJSON = \case
  ChainTipAtGenesis -> String "genesis"
  ChainTip slotNo hash blockNo -> object
    [ "slotNo" .= fromCardanoSlotNo slotNo
    , "hash" .= fromCardanoBlockHeaderHash hash
    , "blockNo" .= fromCardanoBlockNo blockNo
    ]
