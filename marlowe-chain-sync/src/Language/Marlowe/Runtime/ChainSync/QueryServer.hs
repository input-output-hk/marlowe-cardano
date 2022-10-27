{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.QueryServer
  where

import Cardano.Api
  ( AnyCardanoEra(..)
  , CardanoMode
  , ConsensusMode(..)
  , ConsensusModeIsMultiEra(..)
  , EraInMode(..)
  , GenesisParameters(..)
  , QueryInEra(..)
  , QueryInMode(..)
  , QueryInShelleyBasedEra(..)
  , ShelleyBasedEra(..)
  , toEraInMode
  )
import qualified Cardano.Api as Cardano
import Colog (logError)
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(ExceptT), except, runExceptT, throwE, withExceptT)
import Data.Bifunctor (bimap, first)
import qualified Data.Text as T
import Data.Void (Void)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery(..), SlotConfig(..), querySchema)
import qualified Language.Marlowe.Runtime.ChainSync.Database as Database
import Language.Marlowe.Runtime.Logging.Colog.LogIO (ConcurrentlyLogIO(..), LogIO, catchLogIO)
import Network.Protocol.Query.Server (QueryServer(..))
import qualified Network.Protocol.Query.Server as QueryServer
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Unsafe.Coerce (unsafeCoerce)

newtype RunQueryServer m = RunQueryServer (forall a. QueryServer ChainSyncQuery m a -> LogIO a)

data ChainSyncQueryServerDependencies = ChainSyncQueryServerDependencies
  { acceptRunQueryServer :: LogIO (RunQueryServer LogIO)
  , queryLocalNodeState
      :: forall result
       . Maybe Cardano.ChainPoint
      -> QueryInMode CardanoMode result
      -> LogIO (Either AcquireFailure result)
  , getUTxOs :: !(Database.GetUTxOs LogIO)
  }

newtype ChainSyncQueryServer = ChainSyncQueryServer
  { runChainSyncQueryServer :: LogIO Void
  }

mkChainSyncQueryServer :: ChainSyncQueryServerDependencies -> STM ChainSyncQueryServer
mkChainSyncQueryServer ChainSyncQueryServerDependencies{..} = do
  let
    runChainSyncQueryServer = do
      runQueryServer <- acceptRunQueryServer
      Worker{..} <- liftIO $ atomically $ mkWorker WorkerDependencies {..}
      runConcurrentlyLogIO $
        ConcurrentlyLogIO (runWorker `catchLogIO` catchWorker) *> ConcurrentlyLogIO runChainSyncQueryServer
  pure $ ChainSyncQueryServer { runChainSyncQueryServer }

catchWorker :: SomeException -> LogIO ()
catchWorker = logError . T.pack . mappend "Query worker crashed with exception: " . show

data WorkerDependencies = WorkerDependencies
  { runQueryServer :: RunQueryServer LogIO
  , queryLocalNodeState
      :: forall result
       . Maybe Cardano.ChainPoint
      -> QueryInMode CardanoMode result
      -> LogIO (Either AcquireFailure result)
  , getUTxOs :: !(Database.GetUTxOs LogIO)
  }

newtype Worker = Worker
  { runWorker :: LogIO ()
  }

mkWorker :: WorkerDependencies -> STM Worker
mkWorker WorkerDependencies{..} =
  let
    RunQueryServer run = runQueryServer
  in
    pure Worker { runWorker = run server }

  where
    server :: QueryServer ChainSyncQuery LogIO ()
    server = QueryServer.liftNonPaginated querySchema $ \case
        GetSlotConfig        -> queryGenesisParameters extractSlotConfig
        GetSecurityParameter -> queryGenesisParameters protocolParamSecurity
        GetNetworkId -> queryGenesisParameters protocolParamNetworkId
        GetProtocolParameters -> queryShelley (const QueryProtocolParameters)
        GetSystemStart ->
          bimap (const ()) unsafeCoerce <$> queryLocalNodeState Nothing QuerySystemStart
        GetEraHistory ->
          first (const ()) <$> queryLocalNodeState Nothing (QueryEraHistory CardanoModeIsMultiEra)
        GetUTxOs utxosQuery -> do
          utxos <- Database.runGetUTxOs getUTxOs utxosQuery
          pure $ Right utxos

    queryGenesisParameters f = fmap f <$> queryShelley (const QueryGenesisParameters)

    queryShelley
      :: (forall era. ShelleyBasedEra era -> QueryInShelleyBasedEra era result)
      -> LogIO (Either () result)
    queryShelley query = runExceptT do
      AnyCardanoEra era <- withExceptT (const ())
        $ ExceptT
        $ queryLocalNodeState Nothing
        $ QueryCurrentEra CardanoModeIsMultiEra
      eraInMode <- case toEraInMode era CardanoMode of
        Nothing        -> throwE ()
        Just eraInMode -> pure eraInMode
      shelleyBasedEra <- case eraInMode of
        ByronEraInCardanoMode   -> throwE ()
        ShelleyEraInCardanoMode -> pure ShelleyBasedEraShelley
        AllegraEraInCardanoMode -> pure ShelleyBasedEraAllegra
        MaryEraInCardanoMode    -> pure ShelleyBasedEraMary
        AlonzoEraInCardanoMode  -> pure ShelleyBasedEraAlonzo
        BabbageEraInCardanoMode -> pure ShelleyBasedEraBabbage
      result <- withExceptT (const ())
        $ ExceptT
        $ queryLocalNodeState Nothing
        $ QueryInEra eraInMode
        $ QueryInShelleyBasedEra shelleyBasedEra $ query shelleyBasedEra
      withExceptT (const ()) $ except result

    extractSlotConfig GenesisParameters{..} = SlotConfig protocolParamSystemStart protocolParamSlotLength
