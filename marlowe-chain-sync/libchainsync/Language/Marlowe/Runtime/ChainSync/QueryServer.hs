{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.QueryServer where

import Cardano.Api (
  AnyCardanoEra (..),
  GenesisParameters (..),
  QueryInEra (..),
  QueryInMode (..),
  QueryInShelleyBasedEra (..),
  ShelleyBasedEra (..),
  babbageEraOnwardsToShelleyBasedEra,
  inEonForEra,
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

type QueryLocalNodeState m =
  forall result
   . Maybe Cardano.ChainPoint
  -> QueryInMode result
  -> m (Either AcquiringFailure result)

data ChainSyncQueryServerDependencies m = ChainSyncQueryServerDependencies
  { queryLocalNodeState :: QueryLocalNodeState m
  , getUTxOs :: Database.GetUTxOs m
  , getScripts :: Database.GetScripts m
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
              GetProtocolParameters era -> do
                let sbe = babbageEraOnwardsToShelleyBasedEra era
                either (fail . show) pure -- handling era mismatch failure
                  =<< either (fail . show) pure -- handling acquiring failure
                  =<< queryLocalNodeState Nothing (QueryInEra $ QueryInShelleyBasedEra sbe QueryProtocolParameters)
              GetSystemStart -> either (fail . show) pure =<< queryLocalNodeState Nothing QuerySystemStart
              GetEraHistory -> either (fail . show) pure =<< queryLocalNodeState Nothing QueryEraHistory
              GetUTxOs utxosQuery -> Database.runGetUTxOs getUTxOs utxosQuery
              GetNodeTip -> atomically nodeTip
              GetTip -> runGetTip getTip
              GetEra -> either (fail . show) pure =<< queryLocalNodeState Nothing QueryCurrentEra
              GetScripts era scripts -> Database.runGetScripts getScripts era scripts
        }

    queryGenesisParameters :: (forall era. GenesisParameters era -> a) -> m a
    queryGenesisParameters f = f <$> queryShelley (const $ QueryInShelleyBasedEraProjection QueryGenesisParameters id)

    queryShelley
      :: forall r
       . (forall era. ShelleyBasedEra era -> QueryInShelleyBasedEraProjection era r)
      -> m r
    queryShelley mkQuery =
      either fail pure =<< runExceptT do
        AnyCardanoEra era <- withExceptT show $ ExceptT $ queryLocalNodeState Nothing QueryCurrentEra
        shelleyBasedEra <- inEonForEra (throwE "Cannot query shelley in byron era") pure era
        withCurrentEra shelleyBasedEra
      where
        withCurrentEra :: forall era. ShelleyBasedEra era -> ExceptT String m r
        withCurrentEra shelleyBasedEra = case mkQuery shelleyBasedEra of
          QueryInShelleyBasedEraProjection query projection -> do
            result <-
              withExceptT show
                . ExceptT
                . queryLocalNodeState Nothing
                . QueryInEra
                $ QueryInShelleyBasedEra shelleyBasedEra query
            withExceptT show $ except $ projection <$> result

    traverseReqTree :: (forall x. ChainSyncQuery x -> m x) -> ReqTree ChainSyncQuery a -> m a
    traverseReqTree f = \case
      ReqLeaf req -> f req
      ReqBin l r -> (,) <$> traverseReqTree f l <*> traverseReqTree f r

data QueryInShelleyBasedEraProjection era r where
  QueryInShelleyBasedEraProjection
    :: QueryInShelleyBasedEra era a
    -> (a -> r)
    -> QueryInShelleyBasedEraProjection era r
