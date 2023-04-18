{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.ChainIndexer
  ( ChainIndexerDependencies(..)
  , ChainIndexerSelector(..)
  , chainIndexer
  , getChainIndexerSelectorConfig
  ) where

import Cardano.Api (CardanoMode, ChainPoint(..), ChainTip(..), LocalNodeClientProtocolsInMode)
import Control.Arrow (returnA)
import Control.Concurrent.Component.Probes
import Control.Concurrent.Component.UnliftIO
import Control.Concurrent.STM (atomically)
import Control.Monad.Event.Class (MonadEvent)
import Data.Aeson (Value(..), object, (.=))
import Data.Time (NominalDiffTime)
import Language.Marlowe.Runtime.Cardano.Api
  (fromCardanoBlockHeader, fromCardanoBlockHeaderHash, fromCardanoBlockNo, fromCardanoSlotNo)
import Language.Marlowe.Runtime.ChainIndexer.Database (DatabaseQueries(..))
import Language.Marlowe.Runtime.ChainIndexer.Genesis (GenesisBlock)
import Language.Marlowe.Runtime.ChainIndexer.NodeClient
  ( Changes(..)
  , CostModel
  , NodeClient(..)
  , NodeClientDependencies(..)
  , NodeClientSelector(..)
  , RollBackwardField(..)
  , RollForwardField(..)
  , nodeClient
  )
import Language.Marlowe.Runtime.ChainIndexer.Store
  (ChainStoreDependencies(..), ChainStoreSelector(..), CheckGenesisBlockField(..), chainStore)
import Observe.Event.Component
  (FieldConfig(..), GetSelectorConfig, SelectorConfig(..), SomeJSON(..), prependKey, singletonFieldConfigWith)
import UnliftIO (MonadUnliftIO)

data ChainIndexerSelector f where
  NodeClientEvent :: NodeClientSelector f -> ChainIndexerSelector f
  ChainStoreEvent :: ChainStoreSelector f -> ChainIndexerSelector f

data ChainIndexerDependencies m = ChainIndexerDependencies
  { connectToLocalNode       :: !(LocalNodeClientProtocolsInMode CardanoMode -> IO ())
  , maxCost                  :: !Int
  , costModel                :: !CostModel
  , databaseQueries          :: !(DatabaseQueries m)
  , persistRateLimit         :: !NominalDiffTime
  , genesisBlock             :: !GenesisBlock
  }

chainIndexer
  :: ( MonadUnliftIO m
     , MonadEvent r NodeClientSelector m
     , MonadEvent r ChainStoreSelector m
     )
  => Component m (ChainIndexerDependencies m) Probes
chainIndexer = proc ChainIndexerDependencies{..} -> do
  let DatabaseQueries{..} = databaseQueries
  NodeClient{..} <- nodeClient -< NodeClientDependencies
    { connectToLocalNode
    , getIntersectionPoints
    , maxCost
    , costModel
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
    }
  returnA -< Probes
    { liveness = atomically connected
    , startup = pure True
    , readiness = atomically ready
    }

getChainIndexerSelectorConfig :: GetSelectorConfig ChainIndexerSelector
getChainIndexerSelectorConfig = \case
  NodeClientEvent sel -> prependKey "node-client" $ getNodeClientSelectorConfig sel
  ChainStoreEvent sel -> prependKey "chain-store" $ getChainStoreSelectorConfig sel

getNodeClientSelectorConfig :: GetSelectorConfig NodeClientSelector
getNodeClientSelectorConfig = \case
  Intersect -> SelectorConfig "intersect" True
    $ singletonFieldConfigWith (SomeJSON . fmap pointToJSON) "points" False
  IntersectFound -> SelectorConfig "intersect-found" True
    $ singletonFieldConfigWith (uncurry tipsToJSON) "tips" True
  IntersectNotFound -> SelectorConfig "intersect-not-found" True
    $ singletonFieldConfigWith (SomeJSON . tipToJSON) "remote-tip" True
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

tipsToJSON :: ChainPoint -> ChainTip -> SomeJSON
tipsToJSON local remote = SomeJSON $ object
  [ "local" .= pointToJSON local
  , "remote" .= tipToJSON remote
  ]

getChainStoreSelectorConfig :: GetSelectorConfig ChainStoreSelector
getChainStoreSelectorConfig = \case
  CheckGenesisBlock -> SelectorConfig "check-genesis-block" True FieldConfig
    { fieldKey = \case
        Computed _ -> "computed"
        Saved _ -> "saved"
    , fieldDefaultEnabled = \case
        Computed _ -> True
        Saved _ -> False
    , toSomeJSON = \case
        Computed block -> SomeJSON $ show block
        Saved block -> SomeJSON $ show block
    }
  Save -> SelectorConfig "save" True FieldConfig
    { fieldKey = const "changes"
    , fieldDefaultEnabled = const True
    , toSomeJSON = \Changes{..} -> SomeJSON $ object
        [ "rollback-point" .= (pointToJSON <$> changesRollback)
        , "block-count" .= changesBlockCount
        , "local-tip" .= tipToJSON changesLocalTip
        , "remote-tip" .= tipToJSON changesTip
        , "tx-count" .= changesTxCount
        ]
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
