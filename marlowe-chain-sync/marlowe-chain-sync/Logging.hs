{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Logging
  ( RootSelector(..)
  , renderRootSelectorOTel
  ) where

import Control.Monad.Event.Class (Inject(..))
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand, ChainSyncQuery, RuntimeChainSeek)
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as DB
import Language.Marlowe.Runtime.ChainSync.NodeClient (NodeClientSelector(..))
import Network.Protocol.Driver.Trace (TcpServerSelector, renderTcpServerSelectorOTel)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Query.Types (Query)
import Observe.Event (idInjectSelector, injectSelector)
import Observe.Event.Render.OpenTelemetry
import OpenTelemetry.Trace
import Ouroboros.Network.Protocol.LocalStateQuery.Codec (Some(..))

data RootSelector f where
  ChainSeekServer :: TcpServerSelector (Handshake RuntimeChainSeek) f -> RootSelector f
  QueryServer :: TcpServerSelector (Handshake (Query ChainSyncQuery)) f -> RootSelector f
  JobServer :: TcpServerSelector (Handshake (Job ChainSyncCommand)) f -> RootSelector f
  Database :: DB.QuerySelector f -> RootSelector f
  NodeService :: NodeClientSelector f -> RootSelector f

instance Inject RootSelector RootSelector where
  inject = idInjectSelector

instance Inject DB.QuerySelector RootSelector where
  inject = injectSelector Database

instance Inject NodeClientSelector RootSelector where
  inject = injectSelector NodeService

renderRootSelectorOTel
  :: Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> RenderSelectorOTel RootSelector
renderRootSelectorOTel dbName dbUser host port = \case
  ChainSeekServer sel -> renderTcpServerSelectorOTel sel
  QueryServer sel -> renderTcpServerSelectorOTel sel
  JobServer sel -> renderTcpServerSelectorOTel sel
  Database sel -> renderDatabaseSelectorOTel dbName dbUser host port sel
  NodeService sel -> renderNodeServiceSelectorOTel sel

renderNodeServiceSelectorOTel :: RenderSelectorOTel NodeClientSelector
renderNodeServiceSelectorOTel = \case
  Submit -> OTelRendered
    { eventName = "cardano/submit_tx"
    , eventKind = Client
    , renderField = pure . ("cardano.transaction",) . toAttribute . T.pack . show
    }
  Query -> OTelRendered
    { eventName = "cardano/query_node_local_state"
    , eventKind = Client
    , renderField = \(Some query) -> [("cardano.query", toAttribute $ T.pack $ show query)]
    }

renderDatabaseSelectorOTel
  :: Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> RenderSelectorOTel DB.QuerySelector
renderDatabaseSelectorOTel dbName dbUser host port = \case
  DB.Query queryName -> OTelRendered
    { eventName = queryName <> " " <> maybe "chain" decodeUtf8 dbName
    , eventKind = Client
    , renderField = \case
        DB.SqlStatement sql -> [ ("db.statement", toAttribute $ decodeUtf8 sql) ]
        DB.Parameters params -> [ ("db.parameters", toAttribute params) ]
        DB.QueryName _ -> catMaybes
          [ Just ("db.system", "postgresql")
          , ("db.user",) . toAttribute . decodeUtf8 <$> dbUser
          , ("net.peer.name",) . toAttribute . decodeUtf8 <$> host
          , ("net.peer.port",) . toAttribute . decodeUtf8 <$> port
          , ("db.name",) . toAttribute . decodeUtf8 <$> dbName
          , Just ("net.transport", "ip_tcp")
          , Just ("db.operation", "SELECT")
          ]
    }
