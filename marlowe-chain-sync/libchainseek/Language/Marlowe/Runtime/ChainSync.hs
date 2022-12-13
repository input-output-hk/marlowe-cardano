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
import Language.Marlowe.Runtime.ChainSync.Database (DatabaseQueries(..))
import Language.Marlowe.Runtime.ChainSync.JobServer
  (ChainSyncJobServerDependencies(..), RunJobServer, chainSyncJobServer)
import Language.Marlowe.Runtime.ChainSync.QueryServer
  (ChainSyncQueryServerDependencies(..), RunQueryServer, chainSyncQueryServer)
import Language.Marlowe.Runtime.ChainSync.Server (ChainSyncServerDependencies(..), RunChainSeekServer, chainSyncServer)
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult)

data ChainSyncDependencies r = ChainSyncDependencies
  { databaseQueries          :: DatabaseQueries IO
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

chainSync :: Component IO (ChainSyncDependencies r) ()
chainSync = proc ChainSyncDependencies{..} -> do
  let DatabaseQueries{..} = databaseQueries
  chainSyncServer -< ChainSyncServerDependencies{..}
  chainSyncQueryServer -< ChainSyncQueryServerDependencies{..}
  chainSyncJobServer -< ChainSyncJobServerDependencies{..}
