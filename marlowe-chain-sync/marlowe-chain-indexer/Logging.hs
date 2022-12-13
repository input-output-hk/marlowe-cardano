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
import Language.Marlowe.Runtime.ChainIndexer (ChainIndexerSelector(..))
import qualified Language.Marlowe.Runtime.ChainIndexer.NodeClient as NodeClient
import qualified Language.Marlowe.Runtime.ChainIndexer.Store as ChainStore
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
  App :: ChainIndexerSelector f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

-- TODO automate this boilerplate with Template Haskell
getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  App sel -> getChainIndexerSelectorConfig sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True
    $ singletonFieldConfig "config" True

getChainIndexerSelectorConfig :: GetSelectorConfig ChainIndexerSelector
getChainIndexerSelectorConfig = \case
  NodeClientEvent sel -> prependKey "node-client" $ getNodeClientSelectorConfig sel
  ChainStoreEvent sel -> prependKey "chain-store" $ getChainStoreSelectorConfig sel

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
