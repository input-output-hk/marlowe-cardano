{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Colog (Message, WithLog)
import Control.Arrow (returnA)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes
import Control.Monad.Event.Class (MonadEvent)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand, ChainSyncQuery, RuntimeChainSeekServer)
import Language.Marlowe.Runtime.ChainSync.Database (DatabaseQueries(..))
import Language.Marlowe.Runtime.ChainSync.JobServer (ChainSyncJobServerDependencies(..), chainSyncJobServer)
import Language.Marlowe.Runtime.ChainSync.QueryServer (ChainSyncQueryServerDependencies(..), chainSyncQueryServer)
import Language.Marlowe.Runtime.ChainSync.Server (ChainSyncServerDependencies(..), chainSyncServer)
import Network.Protocol.Connection (SomeConnectionSourceTraced)
import Network.Protocol.Driver.Trace (HasSpanContext)
import Network.Protocol.Job.Server (JobServer)
import Network.Protocol.Query.Server (QueryServer)
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult)
import UnliftIO (MonadUnliftIO)

data ChainSyncDependencies r s m = ChainSyncDependencies
  { databaseQueries :: DatabaseQueries m
  , syncSource :: SomeConnectionSourceTraced RuntimeChainSeekServer r s m
  , querySource :: SomeConnectionSourceTraced (QueryServer ChainSyncQuery) r s m
  , jobSource :: SomeConnectionSourceTraced (JobServer ChainSyncCommand) r s m
  , queryLocalNodeState
      :: Maybe Cardano.ChainPoint
      -> forall result
       . Cardano.QueryInMode CardanoMode result
      -> m (Either AcquiringFailure result)
  , submitTxToNodeLocal
      :: forall era
       . CardanoEra era
      -> Tx era
      -> m (SubmitResult (TxValidationErrorInMode CardanoMode))
  }

chainSync
  :: (MonadUnliftIO m, MonadEvent r s m, HasSpanContext r, MonadFail m, WithLog env Message m)
  => Component m (ChainSyncDependencies r s m) Probes
chainSync = proc ChainSyncDependencies{..} -> do
  let DatabaseQueries{..} = databaseQueries
  chainSyncServer -< ChainSyncServerDependencies{..}
  chainSyncQueryServer -< ChainSyncQueryServerDependencies{..}
  chainSyncJobServer -< ChainSyncJobServerDependencies{..}
  returnA -< Probes
    { startup = pure True
    , liveness = pure True
    , readiness = pure True
    }
