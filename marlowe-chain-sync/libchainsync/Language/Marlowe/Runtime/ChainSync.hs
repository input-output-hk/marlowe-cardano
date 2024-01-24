{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync (
  ChainSync (..),
  ChainSyncDependencies (..),
  chainSync,
  renderDatabaseSelectorOTel,
  renderNodeServiceSelectorOTel,
) where

import Cardano.Api (ShelleyBasedEra, Tx, TxValidationErrorInCardanoMode)
import qualified Cardano.Api as Cardano
import Cardano.Api.Shelley (AcquiringFailure)
import Control.Arrow (returnA)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.ChainSync.Database (DatabaseQueries (..))
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL (QuerySelector)
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as DB
import Language.Marlowe.Runtime.ChainSync.JobServer (ChainSyncJobServerDependencies (..), chainSyncJobServer)
import Language.Marlowe.Runtime.ChainSync.NodeClient (NodeClientSelector (..))
import Language.Marlowe.Runtime.ChainSync.QueryServer (ChainSyncQueryServerDependencies (..), chainSyncQueryServer)
import Language.Marlowe.Runtime.ChainSync.Server (ChainSyncServerDependencies (..), chainSyncServer)
import Network.Protocol.Connection (ServerSource (..))
import Network.Protocol.Job.Server (JobServer)
import Network.Protocol.Query.Server (QueryServer)
import Numeric.Natural (Natural)
import Observe.Event.Render.OpenTelemetry (OTelRendered (..), RenderSelectorOTel)
import OpenTelemetry.Trace.Core (SpanKind (..), toAttribute)
import Ouroboros.Network.Protocol.LocalStateQuery.Client (Some (..))
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult)
import UnliftIO (MonadUnliftIO, STM)

data ChainSyncDependencies m = ChainSyncDependencies
  { databaseQueries :: DatabaseQueries m
  , queryLocalNodeState
      :: Maybe Cardano.ChainPoint
      -> forall result
       . Cardano.QueryInMode result
      -> m (Either AcquiringFailure result)
  , submitTxToNodeLocal
      :: forall era
       . ShelleyBasedEra era
      -> Tx era
      -> m (SubmitResult TxValidationErrorInCardanoMode)
  , nodeTip :: STM ChainPoint
  , scanBatchSize :: Natural
  }

data ChainSync m = ChainSync
  { syncServerSource :: ServerSource RuntimeChainSeekServer m ()
  , queryServerSource :: ServerSource (QueryServer ChainSyncQuery) m ()
  , jobServerSource :: ServerSource (JobServer ChainSyncCommand) m ()
  , probes :: Probes
  }

chainSync :: (MonadUnliftIO m, MonadFail m) => Component m (ChainSyncDependencies m) (ChainSync m)
chainSync = proc ChainSyncDependencies{..} -> do
  let DatabaseQueries{..} = databaseQueries
  returnA
    -<
      ChainSync
        { syncServerSource = chainSyncServer ChainSyncServerDependencies{..}
        , queryServerSource = chainSyncQueryServer ChainSyncQueryServerDependencies{..}
        , jobServerSource = chainSyncJobServer ChainSyncJobServerDependencies{..}
        , probes =
            Probes
              { startup = pure True
              , liveness = pure True
              , readiness = pure True
              }
        }

renderNodeServiceSelectorOTel :: RenderSelectorOTel NodeClientSelector
renderNodeServiceSelectorOTel = \case
  Submit ->
    OTelRendered
      { eventName = "cardano/submit_tx"
      , eventKind = Client
      , renderField = pure . ("cardano.transaction",) . toAttribute . T.pack . show
      }
  Query ->
    OTelRendered
      { eventName = "cardano/query_node_local_state"
      , eventKind = Client
      , renderField = \(Some query) -> [("cardano.query", toAttribute $ T.pack $ show query)]
      }

renderDatabaseSelectorOTel
  :: Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> RenderSelectorOTel QuerySelector
renderDatabaseSelectorOTel dbName dbUser host port = \case
  DB.Query queryName ->
    OTelRendered
      { eventName = queryName <> " " <> maybe "chain" decodeUtf8 dbName
      , eventKind = Client
      , renderField = \case
          DB.SqlStatement sql -> [("db.statement", toAttribute $ decodeUtf8 sql)]
          DB.Parameters params -> [("db.parameters", toAttribute params)]
          DB.QueryName _ ->
            catMaybes
              [ Just ("db.system", "postgresql")
              , ("db.user",) . toAttribute . decodeUtf8 <$> dbUser
              , ("net.peer.name",) . toAttribute . decodeUtf8 <$> host
              , ("net.peer.port",) . toAttribute . decodeUtf8 <$> port
              , ("db.name",) . toAttribute . decodeUtf8 <$> dbName
              , Just ("net.transport", "ip_tcp")
              , Just ("db.operation", "SELECT")
              ]
      }
