{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.ChainIndexer (
  ChainIndexerDependencies (..),
  ChainIndexerSelector (..),
  chainIndexer,
  renderChainIndexerSelectorOTel,
  renderDatabaseSelectorOTel,
) where

import Cardano.Api (CardanoMode, LocalNodeClientProtocolsInMode)
import Colog (Message, WithLog)
import Control.Arrow (returnA)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes
import Control.Concurrent.STM (atomically)
import Control.Monad (join)
import Control.Monad.Event.Class (Inject, MonadEvent)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (NominalDiffTime)
import Language.Marlowe.Runtime.ChainIndexer.Database (DatabaseQueries (..))
import Language.Marlowe.Runtime.ChainIndexer.Database.PostgreSQL (QueryField (..), QuerySelector (..))
import Language.Marlowe.Runtime.ChainIndexer.Genesis (GenesisBlock)
import Language.Marlowe.Runtime.ChainIndexer.NodeClient (
  Changes (..),
  CostModel,
  NodeClient (..),
  NodeClientDependencies (..),
  nodeClient,
 )
import Language.Marlowe.Runtime.ChainIndexer.Store (
  ChainStoreDependencies (..),
  ChainStoreSelector (..),
  CheckGenesisBlockField (..),
  chainStore,
 )
import Observe.Event.Render.OpenTelemetry (OTelRendered (..), RenderSelectorOTel)
import OpenTelemetry.Trace.Core (SpanKind (..), toAttribute)
import UnliftIO (MonadUnliftIO)

data ChainIndexerSelector f where
  ChainStoreEvent :: ChainStoreSelector f -> ChainIndexerSelector f

data ChainIndexerDependencies m = ChainIndexerDependencies
  { connectToLocalNode :: !(LocalNodeClientProtocolsInMode CardanoMode -> m ())
  , maxCost :: !Int
  , costModel :: !CostModel
  , databaseQueries :: !(DatabaseQueries m)
  , persistRateLimit :: !NominalDiffTime
  , genesisBlock :: !GenesisBlock
  }

chainIndexer
  :: ( MonadUnliftIO m
     , MonadEvent r s m
     , Inject ChainStoreSelector s
     , WithLog env Message m
     )
  => Component m (ChainIndexerDependencies m) Probes
chainIndexer = proc ChainIndexerDependencies{..} -> do
  let DatabaseQueries{..} = databaseQueries
  NodeClient{..} <-
    nodeClient
      -<
        NodeClientDependencies
          { connectToLocalNode
          , getIntersectionPoints
          , maxCost
          , costModel
          }
  let rateLimit = persistRateLimit
  ready <-
    chainStore
      -<
        ChainStoreDependencies
          { commitRollback
          , commitBlocks
          , rateLimit
          , getChanges
          , getGenesisBlock
          , genesisBlock
          , commitGenesisBlock
          }
  returnA
    -<
      Probes
        { liveness = atomically connected
        , startup = pure True
        , readiness = atomically ready
        }

renderChainIndexerSelectorOTel :: RenderSelectorOTel ChainIndexerSelector
renderChainIndexerSelectorOTel = \case
  ChainStoreEvent sel -> renderChainStoreSelectorOTel sel

renderChainStoreSelectorOTel :: RenderSelectorOTel ChainStoreSelector
renderChainStoreSelectorOTel = \case
  CheckGenesisBlock ->
    OTelRendered
      { eventName = "marlowe_chain_indexer/check_genesis_block"
      , eventKind = Internal
      , renderField = \case
          Computed blk -> [("marlowe.chain_indexer.genesis_block.computed", fromString $ show blk)]
          Saved blk -> [("marlowe.chain_indexer.genesis_block.saved", fromString $ show blk)]
      }
  Save ->
    OTelRendered
      { eventName = "marlowe_chain_indexer/save"
      , eventKind = Consumer
      , renderField = \Changes{..} ->
          join
            [ [("cardano.sync.local_tip", fromString $ show changesLocalTip)]
            , [("cardano.sync.remote_tip", fromString $ show changesTip)]
            , case changesRollback of
                Nothing ->
                  [ ("marlowe.chain_indexer.blockCount", toAttribute changesBlockCount)
                  , ("marlowe.chain_indexer.txCount", toAttribute changesTxCount)
                  ]
                Just point -> [("cardano.sync.rollback_point", fromString $ show point)]
            ]
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
          Operation operation -> ("db.operation", toAttribute operation) : standardAttributes
      }
  CopyBlocks -> renderCopy "block"
  CopyTxs -> renderCopy "tx"
  CopyTxOuts -> renderCopy "txOut"
  CopyTxIns -> renderCopy "txIn"
  CopyAssetOuts -> renderCopy "assetOut"
  CopyAssetMints -> renderCopy "assetMint"
  EnableIndexes ->
    OTelRendered
      { eventName = "enable indexes"
      , eventKind = Internal
      , renderField = \_ -> ("db.statement", "REINDEX SCHEMA chain") : standardAttributes
      }
  where
    standardAttributes =
      catMaybes
        [ Just ("db.system", "postgresql")
        , ("db.user",) . toAttribute . decodeUtf8 <$> dbUser
        , ("net.peer.name",) . toAttribute . decodeUtf8 <$> host
        , ("net.peer.port",) . toAttribute . decodeUtf8 <$> port
        , ("db.name",) . toAttribute . decodeUtf8 <$> dbName
        , Just ("net.transport", "ip_tcp")
        ]
    renderCopy :: Text -> OTelRendered Int64
    renderCopy table =
      OTelRendered
        { eventName = "COPY chain." <> table
        , eventKind = Internal
        , renderField = \rows ->
            standardAttributes
              <> [ ("db.statement", toAttribute $ "COPY chain." <> table <> " FROM STDIN WITH (FORMAT 'csv')")
                 , ("db.rowsAffected", toAttribute rows)
                 ]
        }
