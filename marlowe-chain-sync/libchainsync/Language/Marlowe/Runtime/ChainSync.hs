{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync
  ( ChainSyncDependencies(..)
  , chainSync
  ) where

import Cardano.Api (CardanoEra, CardanoMode, Tx, TxValidationErrorInMode)
import qualified Cardano.Api as Cardano
import Cardano.Api.Shelley (AcquiringFailure)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand, ChainSyncQuery, RuntimeChainSeekServer)
import Language.Marlowe.Runtime.ChainSync.Database (DatabaseQueries(..))
import Language.Marlowe.Runtime.ChainSync.JobServer (ChainSyncJobServerDependencies(..), chainSyncJobServer)
import Language.Marlowe.Runtime.ChainSync.QueryServer (ChainSyncQueryServerDependencies(..), chainSyncQueryServer)
import Language.Marlowe.Runtime.ChainSync.Server (ChainSyncServerDependencies(..), chainSyncServer)
import Network.Protocol.Connection (SomeConnectionSource)
import Network.Protocol.Job.Server (JobServer)
import Network.Protocol.Query.Server (QueryServer)
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult)

data ChainSyncDependencies r = ChainSyncDependencies
  { databaseQueries :: DatabaseQueries IO
  , syncSource :: SomeConnectionSource RuntimeChainSeekServer IO
  , querySource :: SomeConnectionSource (QueryServer ChainSyncQuery) IO
  , jobSource :: SomeConnectionSource (JobServer ChainSyncCommand) IO
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
  , httpPort :: Int
  }

chainSync :: Component IO (ChainSyncDependencies r) ()
chainSync = proc ChainSyncDependencies{..} -> do
  let DatabaseQueries{..} = databaseQueries
  chainSyncServer -< ChainSyncServerDependencies{..}
  chainSyncQueryServer -< ChainSyncQueryServerDependencies{..}
  chainSyncJobServer -< ChainSyncJobServerDependencies{..}
  probeServer -< ProbeServerDependencies
    { probes = Probes
        { startup = pure True
        , liveness = pure True
        , readiness = pure True
        }
    , port = httpPort
    }
