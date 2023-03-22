{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.ChainIndexer
  ( ChainIndexerDependencies(..)
  , ChainIndexerSelector(..)
  , chainIndexer
  , getChainIndexerSelectorConfig
  ) where

import Cardano.Api (CardanoMode, ChainPoint(..), ChainTip(..), LocalNodeClientProtocolsInMode)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes
import Control.Concurrent.STM (atomically)
import Data.Aeson (Value(..), object, (.=))
import Data.Time (NominalDiffTime)
import Language.Marlowe.Runtime.Cardano.Api
  (fromCardanoBlockHeader, fromCardanoBlockHeaderHash, fromCardanoBlockNo, fromCardanoSlotNo)
import Language.Marlowe.Runtime.ChainIndexer.Database (DatabaseQueries(..))
import Language.Marlowe.Runtime.ChainIndexer.Genesis (GenesisBlock)
import Language.Marlowe.Runtime.ChainIndexer.NodeClient
  ( CostModel
  , NodeClient(..)
  , NodeClientDependencies(..)
  , NodeClientSelector(..)
  , RollBackwardField(..)
  , RollForwardField(..)
  , nodeClient
  )
import Language.Marlowe.Runtime.ChainIndexer.Store
  (ChainStoreDependencies(..), ChainStoreSelector(..), SaveField(..), chainStore)
import Observe.Event.Component
  ( FieldConfig(..)
  , GetSelectorConfig
  , SelectorConfig(..)
  , SomeJSON(..)
  , absurdFieldConfig
  , prependKey
  , singletonFieldConfig
  , singletonFieldConfigWith
  )
import Observe.Event.Explicit (EventBackend, injectSelector, narrowEventBackend)

data ChainIndexerSelector f where
  NodeClientEvent :: NodeClientSelector f -> ChainIndexerSelector f
  ChainStoreEvent :: ChainStoreSelector f -> ChainIndexerSelector f

data ChainIndexerDependencies r = ChainIndexerDependencies
  { connectToLocalNode       :: !(LocalNodeClientProtocolsInMode CardanoMode -> IO ())
  , maxCost                  :: !Int
  , costModel                :: !CostModel
  , databaseQueries          :: !(DatabaseQueries IO)
  , persistRateLimit         :: !NominalDiffTime
  , genesisBlock             :: !GenesisBlock
  , eventBackend          :: !(EventBackend IO r ChainIndexerSelector)
  , httpPort :: Int
  }

chainIndexer :: Component IO (ChainIndexerDependencies r) ()
chainIndexer = proc ChainIndexerDependencies{..} -> do
  let DatabaseQueries{..} = databaseQueries
  NodeClient{..} <- nodeClient -< NodeClientDependencies
    { connectToLocalNode
    , getIntersectionPoints
    , maxCost
    , costModel
    , eventBackend = narrowEventBackend (injectSelector NodeClientEvent) eventBackend
    }
  let rateLimit = persistRateLimit
  ready <- chainStore -< ChainStoreDependencies
    { commitRollback
    , commitBlocks
    , rateLimit
    , getChanges
    , getGenesisBlock
    , genesisBlock
    , commitGenesisBlock
    , eventBackend = narrowEventBackend (injectSelector ChainStoreEvent) eventBackend
    }
  probeServer -< ProbeServerDependencies
    { probes = Probes
        { liveness = atomically connected
        , startup = pure True
        , readiness = atomically ready
        }
    , port = httpPort
    }

getChainIndexerSelectorConfig :: GetSelectorConfig ChainIndexerSelector
getChainIndexerSelectorConfig = \case
  NodeClientEvent sel -> prependKey "node-client" $ getNodeClientSelectorConfig sel
  ChainStoreEvent sel -> prependKey "chain-store" $ getChainStoreSelectorConfig sel

getNodeClientSelectorConfig :: GetSelectorConfig NodeClientSelector
getNodeClientSelectorConfig = \case
  Connect -> SelectorConfig "connect" True absurdFieldConfig
  Intersect -> SelectorConfig "intersect" True
    $ singletonFieldConfigWith (SomeJSON . fmap pointToJSON) "points" False
  IntersectFound -> SelectorConfig "intersect-found" True
    $ singletonFieldConfigWith (SomeJSON . pointToJSON) "point" True
  IntersectNotFound -> SelectorConfig "intersect-not-found" True absurdFieldConfig
  RollForward -> SelectorConfig "roll-forward" False FieldConfig
    { fieldKey = \case
        RollForwardBlock _ -> "block-header"
        RollForwardTip _ -> "tip"
        RollForwardNewCost _ -> "new-cost"
    , fieldDefaultEnabled = \case
        RollForwardBlock _ -> True
        RollForwardTip _ -> True
        RollForwardNewCost _ -> False
    , toSomeJSON = \case
        RollForwardBlock header -> SomeJSON $ fromCardanoBlockHeader header
        RollForwardTip tip -> SomeJSON $ tipToJSON tip
        RollForwardNewCost cost -> SomeJSON cost
    }
  RollBackward -> SelectorConfig "roll-backward" True FieldConfig
    { fieldKey = \case
        RollBackwardPoint _ -> "point"
        RollBackwardTip _ -> "tip"
        RollBackwardNewCost _ -> "new-cost"
    , fieldDefaultEnabled = \case
        RollBackwardPoint _ -> True
        RollBackwardTip _ -> True
        RollBackwardNewCost _ -> False
    , toSomeJSON = \case
        RollBackwardPoint point -> SomeJSON $ pointToJSON point
        RollBackwardTip tip -> SomeJSON $ tipToJSON tip
        RollBackwardNewCost cost -> SomeJSON cost
    }

getChainStoreSelectorConfig :: GetSelectorConfig ChainStoreSelector
getChainStoreSelectorConfig = \case
  CheckGenesisBlock -> SelectorConfig "check-genesis-block" True
    $ singletonFieldConfig "genesis-block-exists" True
  Save -> SelectorConfig "save" True FieldConfig
    { fieldKey = \case
        RollbackPoint _ -> "rollback-point"
        BlockCount _ -> "block-count"
        LocalTip _ -> "local-tip"
        RemoteTip _ -> "remote-tip"
        TxCount _ -> "tx-count"
    , fieldDefaultEnabled = const True
    , toSomeJSON = \case
        RollbackPoint point -> SomeJSON $ pointToJSON point
        BlockCount count -> SomeJSON count
        LocalTip tip -> SomeJSON $ tipToJSON tip
        RemoteTip tip -> SomeJSON $ tipToJSON tip
        TxCount count -> SomeJSON count
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
