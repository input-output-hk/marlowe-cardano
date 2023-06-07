{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.QueryServer where

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
import Cardano.Api.Shelley (AcquiringFailure)
import Colog (WithLog)
import qualified Colog as C
import Control.Concurrent.Component
import Control.Monad.Trans.Except (ExceptT(ExceptT), except, runExceptT, throwE, withExceptT)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery(..))
import qualified Language.Marlowe.Runtime.ChainSync.Database as Database
import Network.Protocol.Connection (ConnectionSource, Connector, acceptConnector, runConnector)
import Network.Protocol.Query.Server (QueryServer(..), ServerStReq(..))
import Network.Protocol.Query.Types
import UnliftIO (MonadUnliftIO)

data ChainSyncQueryServerDependencies m = ChainSyncQueryServerDependencies
  { querySource :: ConnectionSource (QueryServer ChainSyncQuery) m
  , queryLocalNodeState
      :: forall result
       . Maybe Cardano.ChainPoint
      -> QueryInMode CardanoMode result
      -> m (Either AcquiringFailure result)
  , getUTxOs :: Database.GetUTxOs m
  }

chainSyncQueryServer
  :: (MonadUnliftIO m, MonadFail m, WithLog env C.Message m)
  => Component m (ChainSyncQueryServerDependencies m) ()
chainSyncQueryServer = serverComponent "chain-sync-query-server" worker \ChainSyncQueryServerDependencies{..} -> do
  connector <- acceptConnector querySource
  pure WorkerDependencies {..}

data WorkerDependencies m = WorkerDependencies
  { connector :: Connector (QueryServer ChainSyncQuery) m
  , queryLocalNodeState
      :: forall result
       . Maybe Cardano.ChainPoint
      -> QueryInMode CardanoMode result
      -> m (Either AcquiringFailure result)
  , getUTxOs :: Database.GetUTxOs m
  }

worker
  :: forall env m. (MonadUnliftIO m, MonadFail m, WithLog env C.Message m)
  => Component m (WorkerDependencies m) ()
worker = component_ "chain-sync-query-worker" \WorkerDependencies{..} -> do
  let
    server :: QueryServer ChainSyncQuery m ()
    server = QueryServer $ pure serverReq

    serverReq :: ServerStReq ChainSyncQuery m ()
    serverReq = ServerStReq
      { recvMsgDone = pure ()
      , recvMsgRequest = fmap (,serverReq) . traverseReqTree \case
          GetSecurityParameter -> queryGenesisParameters protocolParamSecurity
          GetNetworkId -> queryGenesisParameters protocolParamNetworkId
          GetProtocolParameters -> queryShelley (const QueryProtocolParameters)
          GetSystemStart -> either (fail . show) pure =<< queryLocalNodeState Nothing QuerySystemStart
          GetEraHistory -> either (fail . show) pure =<< queryLocalNodeState Nothing (QueryEraHistory CardanoModeIsMultiEra)
          GetUTxOs utxosQuery -> Database.runGetUTxOs getUTxOs utxosQuery
      }

    queryGenesisParameters :: (GenesisParameters -> a) -> m a
    queryGenesisParameters f = f <$> queryShelley (const QueryGenesisParameters)

    queryShelley
      :: (forall era. ShelleyBasedEra era -> QueryInShelleyBasedEra era a)
      -> m a
    queryShelley query = either fail pure =<< runExceptT do
      AnyCardanoEra era <- withExceptT show
        $ ExceptT
        $ queryLocalNodeState Nothing
        $ QueryCurrentEra CardanoModeIsMultiEra
      eraInMode <- case toEraInMode era CardanoMode of
        Nothing        -> throwE $ "cannot convert " <> show era <> " to era in mode"
        Just eraInMode -> pure eraInMode
      shelleyBasedEra <- case eraInMode of
        ByronEraInCardanoMode   -> throwE "Cannot query shelley in byron era"
        ShelleyEraInCardanoMode -> pure ShelleyBasedEraShelley
        AllegraEraInCardanoMode -> pure ShelleyBasedEraAllegra
        MaryEraInCardanoMode    -> pure ShelleyBasedEraMary
        AlonzoEraInCardanoMode  -> pure ShelleyBasedEraAlonzo
        BabbageEraInCardanoMode -> pure ShelleyBasedEraBabbage
      result <- withExceptT show
        $ ExceptT
        $ queryLocalNodeState Nothing
        $ QueryInEra eraInMode
        $ QueryInShelleyBasedEra shelleyBasedEra $ query shelleyBasedEra
      withExceptT show $ except result

    traverseReqTree :: (forall x. ChainSyncQuery x -> m x) -> ReqTree ChainSyncQuery a -> m a
    traverseReqTree f = \case
      ReqLeaf req -> f req
      ReqBin l r -> (,) <$> traverseReqTree f l <*> traverseReqTree f r

  runConnector connector server
