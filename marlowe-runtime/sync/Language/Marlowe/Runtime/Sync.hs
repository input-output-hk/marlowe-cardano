{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Sync where

import Control.Arrow (arr)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Version (Version)
import Language.Marlowe.Protocol.BulkSync.Server (MarloweBulkSyncServer)
import Language.Marlowe.Protocol.HeaderSync.Server (MarloweHeaderSyncServer)
import Language.Marlowe.Protocol.Query.Server (MarloweQueryServer)
import Language.Marlowe.Protocol.Sync.Server (MarloweSyncServer)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery, renderTxOutRef)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (..), renderContractId)
import Language.Marlowe.Runtime.Sync.Database (
  DatabaseQueries,
  DatabaseSelector (..),
  GetHeadersArguments (..),
  GetIntersectionForContractArguments (..),
  GetNextStepsArguments (..),
  GetPayoutsArguments (..),
  GetWithdrawalsArguments (..),
  QueryField (..),
 )
import qualified Language.Marlowe.Runtime.Sync.Database as Sync
import Language.Marlowe.Runtime.Sync.MarloweBulkSyncServer (
  MarloweBulkSyncServerDependencies (..),
  marloweBulkSyncServer,
 )
import Language.Marlowe.Runtime.Sync.MarloweHeaderSyncServer
import Language.Marlowe.Runtime.Sync.MarloweSyncServer
import Language.Marlowe.Runtime.Sync.QueryServer
import Network.Protocol.Connection (Connector, ServerSource)
import Network.Protocol.Query.Client (QueryClient)
import Observe.Event.Render.OpenTelemetry (OTelRendered (..), RenderSelectorOTel)
import OpenTelemetry.Attributes (Attribute, toAttribute)
import OpenTelemetry.Trace.Core (SpanKind (..))
import UnliftIO (MonadUnliftIO)
import Prelude hiding (filter)

data SyncDependencies m = SyncDependencies
  { runtimeVersion :: Version
  , chainSyncQueryConnector :: Connector (QueryClient ChainSyncQuery) m
  , databaseQueries :: DatabaseQueries m
  }

data MarloweSync m = MarloweSync
  { syncServerSource :: ServerSource MarloweSyncServer m ()
  , headerSyncServerSource :: ServerSource MarloweHeaderSyncServer m ()
  , bulkSyncServerSource :: ServerSource MarloweBulkSyncServer m ()
  , queryServerSource :: ServerSource MarloweQueryServer m ()
  , probes :: Probes
  }

sync :: (MonadUnliftIO m) => Component m (SyncDependencies m) (MarloweSync m)
sync = arr \SyncDependencies{..} ->
  MarloweSync
    { syncServerSource = marloweSyncServer MarloweSyncServerDependencies{..}
    , headerSyncServerSource = marloweHeaderSyncServer MarloweHeaderSyncServerDependencies{..}
    , bulkSyncServerSource = marloweBulkSyncServer MarloweBulkSyncServerDependencies{..}
    , queryServerSource = queryServer QueryServerDependencies{..}
    , probes =
        Probes
          { startup = pure True
          , liveness = pure True
          , readiness = pure True
          }
    }

renderDatabaseSelectorOTel
  :: Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> RenderSelectorOTel Sync.DatabaseSelector
renderDatabaseSelectorOTel dbName dbUser host port = \case
  GetTip -> renderQuerySelectorOTel "get_tip" $ const Nothing
  GetTipForContract ->
    renderQuerySelectorOTel "get_tip_for_contract" $
      Just . toAttribute . renderContractId
  GetCreateStep ->
    renderQuerySelectorOTel "get_create_step" $
      Just . toAttribute . renderContractId
  GetIntersectionForContract -> renderQuerySelectorOTel "get_intersection_for_contract" \case
    GetIntersectionForContractArguments{..} ->
      Just $ toAttribute $ renderContractId contractId : (fromString . show <$> points)
  GetIntersection ->
    renderQuerySelectorOTel "get_intersection" $
      Just . toAttribute . fmap (fromString @Text . show)
  GetNextHeaders -> renderQuerySelectorOTel "get_next_headers" $ Just . fromString . show
  GetNextBlocks -> renderQuerySelectorOTel "get_next_blocks" $ Just . fromString . show
  GetNextSteps MarloweV1 -> renderQuerySelectorOTel "get_next_steps" \case
    GetNextStepsArguments{..} ->
      Just $
        toAttribute
          [ fromString $ show MarloweV1
          , renderContractId contractId
          , fromString $ show fromPoint
          ]
  GetHeaders -> renderQuerySelectorOTel "get_headers" \case
    GetHeadersArguments{..} ->
      Just $
        toAttribute
          [ fromString @Text $ show filter
          , fromString $ show range
          ]
  GetContractState ->
    renderQuerySelectorOTel "get_contract_state" $
      Just . toAttribute . renderContractId
  GetTransaction ->
    renderQuerySelectorOTel "get_transaction" $
      Just . fromString . show
  GetTransactions ->
    renderQuerySelectorOTel "get_transactions" $
      Just . toAttribute . renderContractId
  GetWithdrawal ->
    renderQuerySelectorOTel "get_withdrawal" $
      Just . fromString . show
  GetWithdrawals -> renderQuerySelectorOTel "get_withdrawals" \case
    GetWithdrawalsArguments{..} ->
      Just $
        toAttribute
          [ fromString @Text $ show filter
          , fromString $ show range
          ]
  GetPayouts -> renderQuerySelectorOTel "get_payouts" \case
    GetPayoutsArguments{..} ->
      Just $
        toAttribute
          [ fromString @Text $ show filter
          , fromString $ show range
          ]
  GetPayout ->
    renderQuerySelectorOTel "get_payout" $
      Just . toAttribute . renderTxOutRef
  GetRoleCurrencies ->
    renderQuerySelectorOTel "get_role_currencies" $
      Just . fromString . show
  where
    renderQuerySelectorOTel :: Text -> (p -> Maybe Attribute) -> OTelRendered (QueryField p r)
    renderQuerySelectorOTel queryName renderArguments =
      OTelRendered
        { eventName = queryName <> " " <> maybe "chain" decodeUtf8 dbName
        , eventKind = Client
        , renderField = \case
            Arguments p ->
              catMaybes
                [ Just ("db.system", "postgresql")
                , ("db.user",) . toAttribute . decodeUtf8 <$> dbUser
                , ("net.peer.name",) . toAttribute . decodeUtf8 <$> host
                , ("net.peer.port",) . toAttribute . decodeUtf8 <$> port
                , ("db.name",) . toAttribute . decodeUtf8 <$> dbName
                , Just ("net.transport", "ip_tcp")
                , Just ("db.operation", "SELECT")
                , ("db.parameters",) <$> renderArguments p
                ]
            Result _ -> []
        }
