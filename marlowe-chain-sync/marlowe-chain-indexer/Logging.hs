{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Logging (
  RootSelector (..),
  renderRootSelectorOTel,
) where

import Cardano.Api (CardanoMode, ConsensusModeParams (..), EpochSlots (..), LocalNodeConnectInfo (..))
import Control.Monad.Event.Class (Inject (..))
import Data.ByteString (ByteString)
import Data.String (IsString (fromString))
import Language.Marlowe.Runtime.ChainIndexer (
  ChainIndexerSelector (..),
  renderChainIndexerSelectorOTel,
  renderDatabaseSelectorOTel,
 )
import Language.Marlowe.Runtime.ChainIndexer.Database.PostgreSQL (QuerySelector (..))
import Language.Marlowe.Runtime.ChainIndexer.NodeClient (NodeClientSelector (..))
import Language.Marlowe.Runtime.ChainIndexer.Store (ChainStoreSelector (..))
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
  App sel -> renderChainIndexerSelectorOTel sel
  Database sel -> renderDatabaseSelectorOTel dbName dbUser host port sel
  ConnectToNode ->
    OTelRendered
      { eventName = "cardano.connect"
      , eventKind = Client
      , renderField = \LocalNodeConnectInfo{..} ->
          [ ("net.transport", "ip_tcp")
          , ("net.protocol.name", "mux")
          , ("net.sock.peer.name", fromString localNodeSocketPath)
          , ("net.sock.peer.addr", fromString localNodeSocketPath)
          , ("net.sock.family", "unix")
          ,
            ( "cardano.epoch_slots"
            , toAttribute $ IntAttribute $ fromIntegral let CardanoModeParams (EpochSlots slots) = localConsensusModeParams in slots
            )
          , ("cardano.network_id", fromString $ show localNodeNetworkId)
          ]
      }
