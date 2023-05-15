{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Logging
  ( RootSelector(..)
  , renderRootSelectorOTel
  ) where

import Control.Monad.Event.Class (Inject(..))
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery, RuntimeChainSeek)
import Language.Marlowe.Runtime.Indexer (MarloweIndexerSelector(..))
import Language.Marlowe.Runtime.Indexer.ChainSeekClient (ChainSeekClientSelector(..))
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL (QueryField(..), QuerySelector(..))
import Language.Marlowe.Runtime.Indexer.Store (ChangesStatistics(..), SaveField(..), StoreSelector(..))
import Network.Protocol.Driver.Trace (TcpClientSelector, renderTcpClientSelectorOTel)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Query.Types (Query)
import Observe.Event (idInjectSelector, injectSelector)
import Observe.Event.Render.OpenTelemetry
import OpenTelemetry.Trace

data RootSelector f where
  ChainSeekClient :: TcpClientSelector (Handshake RuntimeChainSeek) f -> RootSelector f
  ChainQueryClient :: TcpClientSelector (Handshake (Query ChainSyncQuery)) f -> RootSelector f
  Database :: QuerySelector f -> RootSelector f
  App :: MarloweIndexerSelector Span f -> RootSelector f

instance Inject RootSelector RootSelector where
  inject = idInjectSelector

instance Inject StoreSelector RootSelector where
  inject = injectSelector $ App . StoreEvent

instance Inject (ChainSeekClientSelector Span) RootSelector where
  inject = injectSelector $ App . ChainSeekClientEvent

instance Inject QuerySelector RootSelector where
  inject = injectSelector Database

renderRootSelectorOTel
  :: Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> RenderSelectorOTel RootSelector
renderRootSelectorOTel dbName dbUser host port = \case
  ChainSeekClient sel -> renderTcpClientSelectorOTel sel
  ChainQueryClient sel -> renderTcpClientSelectorOTel sel
  Database sel -> renderDatabaseSelectorOTel dbName dbUser host port sel
  App sel -> renderAppSelectorOTel sel

renderDatabaseSelectorOTel
  :: Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> RenderSelectorOTel QuerySelector
renderDatabaseSelectorOTel dbName dbUser host port = \case
  Query queryName -> OTelRendered
    { eventName = queryName <> " " <> maybe "chain" decodeUtf8 dbName
    , eventKind = Client
    , renderField = \case
        SqlStatement sql -> [ ("db.statement", toAttribute $ decodeUtf8 sql) ]
        Parameters params -> [ ("db.parameters", toAttribute params) ]
        Operation operation -> catMaybes
          [ Just ("db.system", "postgresql")
          , ("db.user",) . toAttribute . decodeUtf8 <$> dbUser
          , ("net.peer.name",) . toAttribute . decodeUtf8 <$> host
          , ("net.peer.port",) . toAttribute . decodeUtf8 <$> port
          , ("db.name",) . toAttribute . decodeUtf8 <$> dbName
          , Just ("net.transport", "ip_tcp")
          , Just ("db.operation", toAttribute operation)
          ]
    }

renderAppSelectorOTel :: RenderSelectorOTel (MarloweIndexerSelector Span)
renderAppSelectorOTel = \case
  StoreEvent sel -> renderStoreSelectorOTel sel
  ChainSeekClientEvent sel -> renderChainSeekClientSelectorOTel sel

renderStoreSelectorOTel :: RenderSelectorOTel StoreSelector
renderStoreSelectorOTel = \case
  Save -> OTelRendered
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
  LoadMarloweUTxO -> OTelRendered
    { eventName = "marlowe_indexer/load_marlowe_utxo"
    , eventKind = Internal
    , renderField = pure . ("marlowe.marlowe_utxo",) . toAttribute . T.pack . show
    }
  EmitEvent -> OTelRendered
    { eventName = "marlowe_indexer/emit_event"
    , eventKind = Producer
    , renderField = const []
    }
