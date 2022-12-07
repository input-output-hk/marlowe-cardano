{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.ChainSync
  ( ChainSyncDependencies(..)
  , chainSync
  ) where

import Cardano.Api (CardanoEra, CardanoMode, LocalNodeClientProtocolsInMode, Tx, TxValidationErrorInMode)
import qualified Cardano.Api as Cardano
import Cardano.Api.Shelley (AcquiringFailure)
import Control.Concurrent.Component
import Data.Time (NominalDiffTime)
import Language.Marlowe.Runtime.ChainSync.Database (DatabaseQueries(..))
import Language.Marlowe.Runtime.ChainSync.Genesis (GenesisBlock)
import Language.Marlowe.Runtime.ChainSync.JobServer
  (ChainSyncJobServerDependencies(..), RunJobServer, chainSyncJobServer)
import Language.Marlowe.Runtime.ChainSync.NodeClient (CostModel, NodeClient(..), NodeClientDependencies(..), nodeClient)
import Language.Marlowe.Runtime.ChainSync.QueryServer
  (ChainSyncQueryServerDependencies(..), RunQueryServer, chainSyncQueryServer)
import Language.Marlowe.Runtime.ChainSync.Server (ChainSyncServerDependencies(..), RunChainSeekServer, chainSyncServer)
import Language.Marlowe.Runtime.ChainSync.Store (ChainStore(..), ChainStoreDependencies(..), chainStore)
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult)

data ChainSyncDependencies = ChainSyncDependencies
  { connectToLocalNode       :: !(LocalNodeClientProtocolsInMode CardanoMode -> IO ())
  , maxCost                  :: !Int
  , costModel                :: !CostModel
  , databaseQueries          :: !(DatabaseQueries IO)
  , persistRateLimit         :: !NominalDiffTime
  , genesisBlock             :: !GenesisBlock
  , acceptRunChainSeekServer :: IO (RunChainSeekServer IO)
  , acceptRunQueryServer     :: IO (RunQueryServer IO)
  , acceptRunJobServer     :: IO (RunJobServer IO)
  , queryLocalNodeState
      :: forall result
       . Maybe Cardano.ChainPoint
      -> Cardano.QueryInMode CardanoMode result
      -> IO (Either AcquiringFailure result)
  , submitTxToNodeLocal
      :: forall era
       . CardanoEra era
      -> Tx era
      -> IO (SubmitResult (TxValidationErrorInMode CardanoMode))
  }

chainSync :: Component IO ChainSyncDependencies ()
chainSync = proc ChainSyncDependencies{..} -> do
  let DatabaseQueries{..} = databaseQueries
  NodeClient{..} <- nodeClient -< NodeClientDependencies{..}
  let rateLimit = persistRateLimit
  ChainStore{..} <- chainStore -< ChainStoreDependencies{..}
  chainSyncServer -< ChainSyncServerDependencies{..}
  chainSyncQueryServer -< ChainSyncQueryServerDependencies{..}
  chainSyncJobServer -< ChainSyncJobServerDependencies{..}
