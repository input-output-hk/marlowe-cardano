{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RankNTypes            #-}

module Language.Marlowe.Runtime.ChainSync
  ( ChainSync (..)
  , ChainSyncDependencies (..)
  , mkChainSync
  ) where

import Cardano.Api (CardanoMode)
import qualified Cardano.Api as Cardano
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (STM)
import Control.Monad (unless)
import Data.Time (NominalDiffTime)
import Language.Marlowe.Runtime.ChainSync.Database (CommitGenesisBlock (..), DatabaseQueries (..), GetGenesisBlock (..))
import Language.Marlowe.Runtime.ChainSync.Genesis (GenesisBlock)
import Language.Marlowe.Runtime.ChainSync.NodeClient (Changes)
import Language.Marlowe.Runtime.ChainSync.QueryServer (ChainSyncQueryServer (..), ChainSyncQueryServerDependencies (..),
                                                       RunQueryServer, mkChainSyncQueryServer)
import Language.Marlowe.Runtime.ChainSync.Server (ChainSyncServer (..), ChainSyncServerDependencies (..),
                                                  RunChainSeekServer (..), mkChainSyncServer)
import Language.Marlowe.Runtime.ChainSync.Store (ChainStore (..), ChainStoreDependencies (..), mkChainStore)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)

data ChainSyncDependencies = ChainSyncDependencies
  { getChanges               :: !(STM Changes)
  , databaseQueries          :: !(DatabaseQueries IO)
  , persistRateLimit         :: !NominalDiffTime
  , genesisBlock             :: !GenesisBlock
  , acceptRunChainSeekServer :: IO (RunChainSeekServer IO)
  , acceptRunQueryServer     :: IO (RunQueryServer IO)
  , queryLocalNodeState      :: forall result. Maybe Cardano.ChainPoint -> Cardano.QueryInMode CardanoMode result -> IO (Either AcquireFailure result)
  }

newtype ChainSync = ChainSync { runChainSync :: IO () }

mkChainSync :: ChainSyncDependencies -> STM ChainSync
mkChainSync ChainSyncDependencies{..} = do
  let DatabaseQueries{..} = databaseQueries
  let rateLimit = persistRateLimit
  ChainStore{..} <- mkChainStore ChainStoreDependencies{..}
  ChainSyncServer{..} <- mkChainSyncServer ChainSyncServerDependencies{..}
  ChainSyncQueryServer{..} <- mkChainSyncQueryServer ChainSyncQueryServerDependencies{..}
  pure $ ChainSync do
    mDbGenesisBlock <- runGetGenesisBlock getGenesisBlock
    case mDbGenesisBlock of
      Just dbGenesisBlock -> unless (dbGenesisBlock == genesisBlock) do
        fail "Existing genesis block does not match computed genesis block"
      Nothing -> runCommitGenesisBlock commitGenesisBlock genesisBlock

    runChainStore
      `concurrently_` runChainSyncServer
      `concurrently_` runChainSyncQueryServer
