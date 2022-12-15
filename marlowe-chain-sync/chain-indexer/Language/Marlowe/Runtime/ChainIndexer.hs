{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.ChainIndexer
  ( ChainIndexerDependencies(..)
  , ChainIndexerSelector(..)
  , chainIndexer
  ) where

import Cardano.Api (CardanoMode, LocalNodeClientProtocolsInMode)
import Control.Concurrent.Component
import Data.Time (NominalDiffTime)
import Language.Marlowe.Runtime.ChainIndexer.Database (DatabaseQueries(..))
import Language.Marlowe.Runtime.ChainIndexer.Genesis (GenesisBlock)
import Language.Marlowe.Runtime.ChainIndexer.NodeClient
  (CostModel, NodeClient(..), NodeClientDependencies(..), NodeClientSelector, nodeClient)
import Language.Marlowe.Runtime.ChainIndexer.Store (ChainStoreDependencies(..), ChainStoreSelector, chainStore)
import Observe.Event (EventBackend, narrowEventBackend)

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
  }

chainIndexer :: Component IO (ChainIndexerDependencies r) ()
chainIndexer = proc ChainIndexerDependencies{..} -> do
  let DatabaseQueries{..} = databaseQueries
  NodeClient{..} <- nodeClient -< NodeClientDependencies
    { connectToLocalNode
    , getIntersectionPoints
    , maxCost
    , costModel
    , eventBackend = narrowEventBackend NodeClientEvent eventBackend
    }
  let rateLimit = persistRateLimit
  chainStore -< ChainStoreDependencies
    { commitRollback
    , commitBlocks
    , rateLimit
    , getChanges
    , getGenesisBlock
    , genesisBlock
    , commitGenesisBlock
    , eventBackend = narrowEventBackend ChainStoreEvent eventBackend
    }
