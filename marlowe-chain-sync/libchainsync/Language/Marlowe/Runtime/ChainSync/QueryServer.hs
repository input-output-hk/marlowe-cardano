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
import Control.Concurrent.Component
import Control.Monad.Event.Class
import Control.Monad.Trans.Except (ExceptT(ExceptT), except, runExceptT, throwE, withExceptT)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery(..))
import qualified Language.Marlowe.Runtime.ChainSync.Database as Database
import Network.Protocol.Connection (SomeConnectionSourceTraced, SomeServerConnectorTraced, acceptSomeConnectorTraced)
import Network.Protocol.Driver.Trace (HasSpanContext, runSomeConnectorTraced)
import Network.Protocol.Query.Server (QueryServer(..), ServerStReq(..))
import Network.Protocol.Query.Types
import UnliftIO (MonadUnliftIO)

data ChainSyncQueryServerDependencies r s m = ChainSyncQueryServerDependencies
  { querySource :: SomeConnectionSourceTraced (QueryServer ChainSyncQuery) r s m
  , queryLocalNodeState
      :: forall result
       . Maybe Cardano.ChainPoint
      -> QueryInMode CardanoMode result
      -> m (Either AcquiringFailure result)
  , getUTxOs :: Database.GetUTxOs m
  }

chainSyncQueryServer
  :: (MonadUnliftIO m, MonadEvent r s m, HasSpanContext r, MonadFail m)
  => Component m (ChainSyncQueryServerDependencies r s m) ()
chainSyncQueryServer = serverComponent worker \ChainSyncQueryServerDependencies{..} -> do
  connector <- acceptSomeConnectorTraced querySource
  pure WorkerDependencies {..}

data WorkerDependencies r s m = WorkerDependencies
  { connector :: SomeServerConnectorTraced (QueryServer ChainSyncQuery) r s m
  , queryLocalNodeState
      :: forall result
       . Maybe Cardano.ChainPoint
      -> QueryInMode CardanoMode result
      -> m (Either AcquiringFailure result)
  , getUTxOs :: Database.GetUTxOs m
  }

worker
  :: forall r s m. (MonadUnliftIO m, MonadEvent r s m, HasSpanContext r, MonadFail m)
  => Component m (WorkerDependencies r s m) ()
worker = component_ \WorkerDependencies{..} -> do
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

  runSomeConnectorTraced connector server
