{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.QueryServer where

import Cardano.Api (
  AnyCardanoEra (..),
  CardanoMode,
  ConsensusMode (..),
  ConsensusModeIsMultiEra (..),
  EraInMode (..),
  GenesisParameters (..),
  QueryInEra (..),
  QueryInMode (..),
  QueryInShelleyBasedEra (..),
  ShelleyBasedEra (..),
  toEraInMode,
 )
import qualified Cardano.Api as Cardano
import Cardano.Api.Shelley (AcquiringFailure)
import Control.Concurrent.STM (STM)
import Control.Monad.Trans.Except (ExceptT (ExceptT), except, runExceptT, throwE, withExceptT)
import Language.Marlowe.Runtime.ChainSync.Api (ChainPoint, ChainSyncQuery (..), Request (..), Response (..))
import Language.Marlowe.Runtime.ChainSync.Database (GetTip (runGetTip))
import qualified Language.Marlowe.Runtime.ChainSync.Database as Database
import Network.Protocol.Connection (ServerSource (..))
import Network.Protocol.Query.Server (QueryServerT, batchWith, respond)
import Network.Protocol.Query.Types
import UnliftIO (Concurrently (Concurrently, runConcurrently), MonadUnliftIO, atomically)

data ChainSyncQueryServerDependencies m = ChainSyncQueryServerDependencies
  { queryLocalNodeState
      :: forall result
       . Maybe Cardano.ChainPoint
      -> QueryInMode CardanoMode result
      -> m (Either AcquiringFailure result)
  , getUTxOs :: Database.GetUTxOs m
  , getTip :: Database.GetTip m
  , nodeTip :: STM ChainPoint
  }

chainSyncQueryServer
  :: forall m
   . (MonadUnliftIO m, MonadFail m)
  => ChainSyncQueryServerDependencies m
  -> ServerSource (QueryServerT (Tree ChainSyncQuery)) m ()
chainSyncQueryServer ChainSyncQueryServerDependencies{..} = ServerSource $ pure server
  where
    server :: QueryServerT (Tree ChainSyncQuery) m ()
    server = respond $ batchWith Concurrently runConcurrently \case
      ReqGetSecurityParameter -> ResGetSecurityParameter <$> queryGenesisParameters protocolParamSecurity
      ReqGetNetworkId -> ResGetNetworkId <$> queryGenesisParameters protocolParamNetworkId
      ReqGetProtocolParameters -> ResGetProtocolParameters <$> queryShelley (const QueryProtocolParameters)
      ReqGetSystemStart -> either (fail . show) (pure . ResGetSystemStart) =<< queryLocalNodeState Nothing QuerySystemStart
      ReqGetEraHistory ->
        either (fail . show) (pure . ResGetEraHistory) =<< queryLocalNodeState Nothing (QueryEraHistory CardanoModeIsMultiEra)
      ReqGetUTxOs utxosQuery -> ResGetUTxOs <$> Database.runGetUTxOs getUTxOs utxosQuery
      ReqGetNodeTip -> ResGetNodeTip <$> atomically nodeTip
      ReqGetTip -> ResGetTip <$> runGetTip getTip
      ReqGetEra -> either (fail . show) (pure . ResGetEra) =<< queryLocalNodeState Nothing (QueryCurrentEra CardanoModeIsMultiEra)

    queryGenesisParameters :: (GenesisParameters -> a) -> m a
    queryGenesisParameters f = f <$> queryShelley (const QueryGenesisParameters)

    queryShelley
      :: (forall era. ShelleyBasedEra era -> QueryInShelleyBasedEra era a)
      -> m a
    queryShelley query =
      either fail pure =<< runExceptT do
        AnyCardanoEra era <-
          withExceptT show $
            ExceptT $
              queryLocalNodeState Nothing $
                QueryCurrentEra CardanoModeIsMultiEra
        eraInMode <- case toEraInMode era CardanoMode of
          Nothing -> throwE $ "cannot convert " <> show era <> " to era in mode"
          Just eraInMode -> pure eraInMode
        shelleyBasedEra <- case eraInMode of
          ByronEraInCardanoMode -> throwE "Cannot query shelley in byron era"
          ShelleyEraInCardanoMode -> pure ShelleyBasedEraShelley
          AllegraEraInCardanoMode -> pure ShelleyBasedEraAllegra
          MaryEraInCardanoMode -> pure ShelleyBasedEraMary
          AlonzoEraInCardanoMode -> pure ShelleyBasedEraAlonzo
          BabbageEraInCardanoMode -> pure ShelleyBasedEraBabbage
          ConwayEraInCardanoMode -> pure ShelleyBasedEraConway
        result <-
          withExceptT show $
            ExceptT $
              queryLocalNodeState Nothing $
                QueryInEra eraInMode $
                  QueryInShelleyBasedEra shelleyBasedEra $
                    query shelleyBasedEra
        withExceptT show $ except result
