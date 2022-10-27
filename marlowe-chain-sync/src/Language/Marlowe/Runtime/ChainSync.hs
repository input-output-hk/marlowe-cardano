{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.ChainSync
  ( ChainSync(..)
  , ChainSyncDependencies(..)
  , mkChainSync
  ) where

import Cardano.Api (CardanoEra, CardanoMode, LocalNodeClientProtocolsInMode, Tx, TxValidationErrorInMode)
import qualified Cardano.Api as Cardano
import Control.Concurrent.STM (STM)
import Control.Monad (unless)
import Data.Time (NominalDiffTime)
import Language.Marlowe.Runtime.ChainSync.Database (CommitGenesisBlock(..), DatabaseQueries(..), GetGenesisBlock(..))
import Language.Marlowe.Runtime.ChainSync.Genesis (GenesisBlock)
import Language.Marlowe.Runtime.ChainSync.JobServer
  (ChainSyncJobServer(..), ChainSyncJobServerDependencies(..), RunJobServer, mkChainSyncJobServer)
import Language.Marlowe.Runtime.ChainSync.NodeClient
  (CostModel, NodeClient(..), NodeClientDependencies(..), mkNodeClient)
import Language.Marlowe.Runtime.ChainSync.QueryServer
  (ChainSyncQueryServer(..), ChainSyncQueryServerDependencies(..), RunQueryServer, mkChainSyncQueryServer)
import Language.Marlowe.Runtime.ChainSync.Server
  (ChainSyncServer(..), ChainSyncServerDependencies(..), RunChainSeekServer(..), mkChainSyncServer)
import Language.Marlowe.Runtime.ChainSync.Store (ChainStore(..), ChainStoreDependencies(..), mkChainStore)
import Language.Marlowe.Runtime.Logging.Colog.LogIO (LogIO, concurrentlyLogIO_, dieLogIO)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult)

data ChainSyncDependencies m = ChainSyncDependencies
  { connectToLocalNode       :: !(LocalNodeClientProtocolsInMode CardanoMode -> LogIO ())
  , maxCost                  :: !Int
  , costModel                :: !CostModel
  , databaseQueries          :: !(DatabaseQueries m)
  , persistRateLimit         :: !NominalDiffTime
  , genesisBlock             :: !GenesisBlock
  , acceptRunChainSeekServer :: m (RunChainSeekServer m)
  , acceptRunQueryServer     :: m (RunQueryServer m)
  , acceptRunJobServer     :: m (RunJobServer m)
  , queryLocalNodeState
      :: forall result
       . Maybe Cardano.ChainPoint
      -> Cardano.QueryInMode CardanoMode result
      -> m (Either AcquireFailure result)
  , submitTxToNodeLocal
      :: forall era
       . CardanoEra era
      -> Tx era
      -> m (SubmitResult (TxValidationErrorInMode CardanoMode))
  }

newtype ChainSync = ChainSync { runChainSync :: LogIO () }

mkChainSync :: ChainSyncDependencies LogIO -> STM ChainSync
mkChainSync ChainSyncDependencies{..} = do
  let DatabaseQueries{..} = databaseQueries
  NodeClient{..} <- mkNodeClient NodeClientDependencies{..}
  let rateLimit = persistRateLimit
  ChainStore{..} <- mkChainStore ChainStoreDependencies{..}
  ChainSyncServer{..} <- mkChainSyncServer ChainSyncServerDependencies{..}
  ChainSyncQueryServer{..} <- mkChainSyncQueryServer ChainSyncQueryServerDependencies{..}
  ChainSyncJobServer{..} <- mkChainSyncJobServer ChainSyncJobServerDependencies{..}
  pure $ ChainSync do
    do
      mDbGenesisBlock <- runGetGenesisBlock getGenesisBlock
      case mDbGenesisBlock of
        Just dbGenesisBlock -> unless (dbGenesisBlock == genesisBlock) do
          dieLogIO "Existing genesis block does not match computed genesis block"
        Nothing -> runCommitGenesisBlock commitGenesisBlock genesisBlock

    runNodeClient
      `concurrentlyLogIO_` runChainStore
      `concurrentlyLogIO_` runChainSyncServer
      `concurrentlyLogIO_` runChainSyncQueryServer
      `concurrentlyLogIO_` runChainSyncJobServer
