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
import Language.Marlowe.Runtime.ChainSync.Api (ChainPoint, ChainSyncQuery (..))
import Language.Marlowe.Runtime.ChainSync.Database (GetTip (runGetTip))
import qualified Language.Marlowe.Runtime.ChainSync.Database as Database
import Network.Protocol.Connection (ServerSource (..))
import Network.Protocol.Query.Server (QueryServer (..), ServerStReq (..))
import Network.Protocol.Query.Types
import UnliftIO (MonadUnliftIO, atomically)

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
  -> ServerSource (QueryServer ChainSyncQuery) m ()
chainSyncQueryServer ChainSyncQueryServerDependencies{..} = ServerSource $ pure server
  where
    server :: QueryServer ChainSyncQuery m ()
    server = QueryServer $ pure serverReq

    serverReq :: ServerStReq ChainSyncQuery m ()
    serverReq =
      ServerStReq
        { recvMsgDone = pure ()
        , recvMsgRequest =
            fmap (,serverReq) . traverseReqTree \case
              GetSecurityParameter -> queryGenesisParameters protocolParamSecurity
              GetNetworkId -> queryGenesisParameters protocolParamNetworkId
              GetProtocolParameters -> queryShelley (const QueryProtocolParameters)
              GetSystemStart -> either (fail . show) pure =<< queryLocalNodeState Nothing QuerySystemStart
              GetEraHistory -> either (fail . show) pure =<< queryLocalNodeState Nothing (QueryEraHistory CardanoModeIsMultiEra)
              GetUTxOs utxosQuery -> Database.runGetUTxOs getUTxOs utxosQuery
              GetNodeTip -> atomically nodeTip
              GetTip -> runGetTip getTip
              GetEra -> either (fail . show) pure =<< queryLocalNodeState Nothing (QueryCurrentEra CardanoModeIsMultiEra)
        }

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
        result <-
          withExceptT show $
            ExceptT $
              queryLocalNodeState Nothing $
                QueryInEra eraInMode $
                  QueryInShelleyBasedEra shelleyBasedEra $
                    query shelleyBasedEra
        withExceptT show $ except result

    traverseReqTree :: (forall x. ChainSyncQuery x -> m x) -> ReqTree ChainSyncQuery a -> m a
    traverseReqTree f = \case
      ReqLeaf req -> f req
      ReqBin l r -> (,) <$> traverseReqTree f l <*> traverseReqTree f r
