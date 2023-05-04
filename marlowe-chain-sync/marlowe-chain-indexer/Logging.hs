{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Logging
  ( RootSelector(..)
  , renderRootSelectorOTel
  ) where

import Cardano.Api
  (BlockHeader(..), CardanoMode, ChainTip(..), ConsensusModeParams(..), EpochSlots(..), LocalNodeConnectInfo(..))
import Control.Monad (join)
import Control.Monad.Event.Class (Inject(..))
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes)
import Data.String (IsString(fromString))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Language.Marlowe.Runtime.ChainIndexer (ChainIndexerSelector(..))
import Language.Marlowe.Runtime.ChainIndexer.Database.PostgreSQL (QueryField(..), QuerySelector(..))
import Language.Marlowe.Runtime.ChainIndexer.NodeClient
  (Changes(..), NodeClientSelector(..), RollBackwardField(..), RollForwardField(..))
import Language.Marlowe.Runtime.ChainIndexer.Store (ChainStoreSelector(..), CheckGenesisBlockField(..))
import Observe.Event (idInjectSelector, injectSelector)
import Observe.Event.Render.OpenTelemetry
import OpenTelemetry.Trace

data RootSelector r f where
  App :: ChainIndexerSelector r f -> RootSelector r f
  Database :: QuerySelector f -> RootSelector r f
  ConnectToNode :: RootSelector r (LocalNodeConnectInfo CardanoMode)

instance Inject (RootSelector r) (RootSelector r) where
  inject = idInjectSelector

instance Inject QuerySelector (RootSelector r) where
  inject = injectSelector Database

instance Inject NodeClientSelector (RootSelector r) where
  inject = injectSelector $ App . NodeClientEvent

instance Inject (ChainStoreSelector r) (RootSelector r) where
  inject = injectSelector $ App . ChainStoreEvent

renderRootSelectorOTel
  :: Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> RenderSelectorOTel (RootSelector r)
renderRootSelectorOTel dbName dbUser host port = \case
  App sel -> renderAppSelectorOTel sel
  Database sel -> renderDatabaseSelectorOTel dbName dbUser host port sel
  ConnectToNode -> OTelRendered
    { eventName = "cardano.connect"
    , eventKind = Client
    , renderField = \LocalNodeConnectInfo{..} ->
        [ ("net.transport", "ip_tcp")
        , ("net.protocol.name", "mux")
        , ("net.sock.peer.name", fromString localNodeSocketPath)
        , ("net.sock.peer.addr", fromString localNodeSocketPath)
        , ("net.sock.family", "unix")
        , ( "cardano.epoch_slots"
          , toAttribute $ IntAttribute $ fromIntegral let CardanoModeParams (EpochSlots slots) = localConsensusModeParams in slots
          )
        , ("cardano.network_id", fromString $ show localNodeNetworkId)
        ]
    }

renderAppSelectorOTel :: RenderSelectorOTel (ChainIndexerSelector r)
renderAppSelectorOTel = \case
  NodeClientEvent sel -> renderNodeClientSelectorOTel sel
  ChainStoreEvent sel -> renderChainStoreSelectorOTel sel

renderNodeClientSelectorOTel :: RenderSelectorOTel NodeClientSelector
renderNodeClientSelectorOTel = \case
  Intersect -> OTelRendered
    { eventName = "marlowe_chain_indexer/intersect"
    , eventKind = Internal
    , renderField = \points ->
        [("cardano.sync.intersect.points", toAttribute $ T.pack . show <$> points)]
    }
  IntersectFound -> OTelRendered
    { eventName = "marlowe_chain_indexer/intersect/found"
    , eventKind = Internal
    , renderField = \(point, tip) ->
        [ ("cardano.sync.intersect", fromString $ show point)
        , ("cardano.sync.local_tip", fromString $ show point)
        , ("cardano.sync.remote_tip", fromString $ show tip)
        ]
    }
  IntersectNotFound -> OTelRendered
    { eventName = "marlowe_chain_indexer/intersect/not_found"
    , eventKind = Internal
    , renderField = \tip ->
        [("cardano.sync.remote_tip", fromString $ show tip)]
    }
  RollForward -> OTelRendered
    { eventName = "marlowe_chain_indexer/roll_forward"
    , eventKind = Producer
    , renderField = \case
        RollForwardBlock (BlockHeader slot hash block) ->
          [("cardano.sync.local_tip", fromString $ show $ show $ ChainTip slot hash block )]
        RollForwardTip tip ->
          [("cardano.sync.remote_tip", fromString $ show $ show tip )]
        _ -> []
    }
  RollBackward -> OTelRendered
    { eventName = "marlowe_chain_indexer/roll_backward"
    , eventKind = Producer
    , renderField = \case
        RollBackwardPoint point ->
          [("cardano.sync.local_tip", fromString $ show $ show point)]
        RollBackwardTip tip ->
          [("cardano.sync.remote_tip", fromString $ show $ show tip )]
        _ -> []
    }

renderChainStoreSelectorOTel :: RenderSelectorOTel (ChainStoreSelector r)
renderChainStoreSelectorOTel = \case
  CheckGenesisBlock -> OTelRendered
    { eventName = "marlowe_chain_indexer/check_genesis_block"
    , eventKind = Internal
    , renderField = \case
        Computed blk -> [("marlowe.chain_indexer.genesis_block.computed", fromString $ show blk)]
        Saved blk -> [("marlowe.chain_indexer.genesis_block.saved", fromString $ show blk)]
    }
  Save -> OTelRendered
    { eventName = "marlowe_chain_indexer/save"
    , eventKind = Consumer
    , renderField = \Changes{..} -> join
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
