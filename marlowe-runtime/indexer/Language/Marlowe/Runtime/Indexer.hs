{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.Indexer where

import Colog (Message, WithLog)
import Control.Arrow (returnA)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes
import Control.Concurrent.STM (atomically)
import Control.Monad.Event.Class (Inject, MonadEvent)
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set.NonEmpty (NESet)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (NominalDiffTime)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery, RuntimeChainSeekClient, ScriptHash)
import Language.Marlowe.Runtime.Indexer.ChainSeekClient
import Language.Marlowe.Runtime.Indexer.Database (DatabaseQueries)
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL (QueryField (..), QuerySelector (..))
import Language.Marlowe.Runtime.Indexer.Store
import Network.Protocol.Connection (Connector)
import Network.Protocol.Query.Client (QueryClient)
import Observe.Event.Render.OpenTelemetry (OTelRendered (..), RenderSelectorOTel)
import OpenTelemetry.Trace.Core (Span, SpanKind (..), ToAttribute (..))
import UnliftIO (MonadUnliftIO)

data MarloweIndexerSelector r f where
  StoreEvent :: StoreSelector f -> MarloweIndexerSelector r f
  ChainSeekClientEvent :: ChainSeekClientSelector r f -> MarloweIndexerSelector r f

data MarloweIndexerDependencies m = MarloweIndexerDependencies
  { databaseQueries :: DatabaseQueries m
  , chainSyncConnector :: Connector RuntimeChainSeekClient m
  , chainSyncQueryConnector :: Connector (QueryClient ChainSyncQuery) m
  , pollingInterval :: NominalDiffTime
  , marloweScriptHashes :: NESet ScriptHash
  , payoutScriptHashes :: NESet ScriptHash
  }

marloweIndexer
  :: ( MonadEvent r s m
     , MonadUnliftIO m
     , Inject StoreSelector s
     , Inject (ChainSeekClientSelector r) s
     , MonadFail m
     , WithLog env Message m
     )
  => Component m (MarloweIndexerDependencies m) Probes
marloweIndexer = proc MarloweIndexerDependencies{..} -> do
  (connected, pullEvent) <- chainSeekClient -< ChainSeekClientDependencies{..}
  store -< StoreDependencies{..}
  returnA
    -<
      Probes
        { startup = pure True
        , liveness = atomically connected
        , readiness = pure True
        }

renderDatabaseSelectorOTel
  :: Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> RenderSelectorOTel QuerySelector
renderDatabaseSelectorOTel dbName dbUser host port = \case
  Query queryName ->
    OTelRendered
      { eventName = queryName <> " " <> maybe "chain" decodeUtf8 dbName
      , eventKind = Client
      , renderField = \case
          SqlStatement sql -> [("db.statement", toAttribute $ decodeUtf8 sql)]
          Parameters params -> [("db.parameters", toAttribute params)]
          Operation operation ->
            catMaybes
              [ Just ("db.system", "postgresql")
              , ("db.user",) . toAttribute . decodeUtf8 <$> dbUser
              , ("net.peer.name",) . toAttribute . decodeUtf8 <$> host
              , ("net.peer.port",) . toAttribute . decodeUtf8 <$> port
              , ("db.name",) . toAttribute . decodeUtf8 <$> dbName
              , Just ("net.transport", "ip_tcp")
              , Just ("db.operation", toAttribute operation)
              ]
      }

renderMarloweIndexerSelectorOTel :: RenderSelectorOTel (MarloweIndexerSelector Span)
renderMarloweIndexerSelectorOTel = \case
  StoreEvent sel -> renderStoreSelectorOTel sel
  ChainSeekClientEvent sel -> renderChainSeekClientSelectorOTel sel

renderStoreSelectorOTel :: RenderSelectorOTel StoreSelector
renderStoreSelectorOTel = \case
  Save ->
    OTelRendered
      { eventName = "marlowe_indexer/save"
      , eventKind = Consumer
      , renderField = \case
          RollbackPoint point -> [("cardano.sync.rollback_point", toAttribute $ T.pack $ show point)]
          Stats ChangesStatistics{..} ->
            [ ("marlowe.indexer.block_count", toAttribute blockCount)
            , ("marlowe.indexer.create_tx_count", toAttribute createTxCount)
            , ("marlowe.indexer.apply_inputs_tx_count", toAttribute applyInputsTxCount)
            , ("marlowe.indexer.withdraw_tx_count", toAttribute withdrawTxCount)
            ]
          LocalTip tip -> [("cardano.sync.local_tip", toAttribute $ T.pack $ show tip)]
          RemoteTip tip -> [("cardano.sync.remote_tip", toAttribute $ T.pack $ show tip)]
          InvalidCreateTxs errors ->
            [("marlowe.indexer.invalid_create_txs", toAttribute $ T.pack . show <$> Map.toList errors)]
          InvalidApplyInputsTxs errors ->
            [("marlowe.indexer.invalid_apply_inputs_txs", toAttribute $ T.pack . show <$> Map.toList errors)]
      }

renderChainSeekClientSelectorOTel :: RenderSelectorOTel (ChainSeekClientSelector Span)
renderChainSeekClientSelectorOTel = \case
  LoadMarloweUTxO ->
    OTelRendered
      { eventName = "marlowe_indexer/load_marlowe_utxo"
      , eventKind = Internal
      , renderField = pure . ("marlowe.marlowe_utxo",) . toAttribute . T.pack . show
      }
  EmitEvent ->
    OTelRendered
      { eventName = "marlowe_indexer/emit_event"
      , eventKind = Producer
      , renderField = const []
      }
