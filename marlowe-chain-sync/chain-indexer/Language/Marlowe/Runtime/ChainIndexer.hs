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
  ) where

import Cardano.Api (CardanoMode, LocalNodeClientProtocolsInMode)
import Control.Arrow (returnA)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes
import Control.Concurrent.STM (atomically)
import Control.Monad.Event.Class (Inject, MonadEvent)
import Data.Time (NominalDiffTime)
import Language.Marlowe.Runtime.ChainIndexer.Database (DatabaseQueries(..))
import Language.Marlowe.Runtime.ChainIndexer.Genesis (GenesisBlock)
import Language.Marlowe.Runtime.ChainIndexer.NodeClient
  (CostModel, NodeClient(..), NodeClientDependencies(..), NodeClientSelector(..), nodeClient)
import Language.Marlowe.Runtime.ChainIndexer.Store (ChainStoreDependencies(..), ChainStoreSelector(..), chainStore)
import UnliftIO (MonadUnliftIO)

data ChainIndexerSelector r f where
  NodeClientEvent :: NodeClientSelector f -> ChainIndexerSelector r f
  ChainStoreEvent :: ChainStoreSelector r f -> ChainIndexerSelector r f

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
     , MonadEvent r s m
     , Inject NodeClientSelector s
     , Inject (ChainStoreSelector r) s
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
