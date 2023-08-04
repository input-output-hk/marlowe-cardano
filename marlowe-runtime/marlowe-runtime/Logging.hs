{-# LANGUAGE FlexibleInstances #-}
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
import Data.String (fromString)
import Language.Marlowe.Protocol.Types (MarloweRuntime)
import Language.Marlowe.Runtime.ChainIndexer (ChainIndexerSelector)
import qualified Language.Marlowe.Runtime.ChainIndexer as ChainIndexer
import qualified Language.Marlowe.Runtime.ChainIndexer.Database.PostgreSQL as ChainIndexer
import Language.Marlowe.Runtime.ChainIndexer.Store (ChainStoreSelector)
import qualified Language.Marlowe.Runtime.ChainSync as ChainSync
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as ChainSync
import qualified Language.Marlowe.Runtime.ChainSync.NodeClient as ChainSync
import Language.Marlowe.Runtime.Contract (renderContractStoreSelectorOTel)
import Language.Marlowe.Runtime.Contract.Store (ContractStoreSelector)
import Language.Marlowe.Runtime.Indexer (MarloweIndexerSelector, renderMarloweIndexerSelectorOTel)
import qualified Language.Marlowe.Runtime.Indexer as MarloweIndexer
import qualified Language.Marlowe.Runtime.Indexer.ChainSeekClient as MarloweIndexer
import qualified Language.Marlowe.Runtime.Indexer.Database.PostgreSQL as MarloweIndexer
import qualified Language.Marlowe.Runtime.Indexer.Store as MarloweIndexer
import qualified Language.Marlowe.Runtime.Sync as Sync
import qualified Language.Marlowe.Runtime.Sync.Database as Sync
import Language.Marlowe.Runtime.Transaction (
  renderLoadMarloweContextSelectorOTel,
  renderLoadWalletContextSelectorOTel,
  renderTransactionServerSelectorOTel,
 )
import qualified Language.Marlowe.Runtime.Transaction.Query as Q
import Language.Marlowe.Runtime.Transaction.Server (TransactionServerSelector)
import Network.Protocol.Driver.Trace (TcpServerSelector, renderTcpServerSelectorOTel)
import Network.Protocol.Handshake.Types (Handshake)
import Observe.Event (idInjectSelector, injectSelector)
import Observe.Event.Render.OpenTelemetry
import OpenTelemetry.Trace
import Prelude hiding (filter)

data RootSelector f where
  ProxyServer :: TcpServerSelector (Handshake MarloweRuntime) f -> RootSelector f
  SyncDatabase :: Sync.DatabaseSelector f -> RootSelector f
  ChainIndexerDatabase :: ChainIndexer.QuerySelector f -> RootSelector f
  ChainIndexer :: ChainIndexerSelector f -> RootSelector f
  ConnectToNode :: RootSelector (LocalNodeConnectInfo CardanoMode)
  ChainSyncDatabase :: ChainSync.QuerySelector f -> RootSelector f
  ChainSyncNodeService :: ChainSync.NodeClientSelector f -> RootSelector f
  MarloweIndexerDatabase :: MarloweIndexer.QuerySelector f -> RootSelector f
  MarloweIndexer :: MarloweIndexerSelector Span f -> RootSelector f
  MarloweTx :: TransactionServerSelector f -> RootSelector f
  LoadWalletContext :: Q.LoadWalletContextSelector f -> RootSelector f
  LoadMarloweContext :: Q.LoadMarloweContextSelector f -> RootSelector f
  ContractStoreSelector :: ContractStoreSelector f -> RootSelector f

instance Inject RootSelector RootSelector where
  inject = idInjectSelector

instance Inject Sync.DatabaseSelector RootSelector where
  inject = injectSelector SyncDatabase

instance Inject ChainIndexer.QuerySelector RootSelector where
  inject = injectSelector ChainIndexerDatabase

instance Inject ChainStoreSelector RootSelector where
  inject = injectSelector $ ChainIndexer . ChainIndexer.ChainStoreEvent

instance Inject ChainSync.QuerySelector RootSelector where
  inject = injectSelector ChainSyncDatabase

instance Inject ChainSync.NodeClientSelector RootSelector where
  inject = injectSelector ChainSyncNodeService

instance Inject MarloweIndexer.StoreSelector RootSelector where
  inject = injectSelector $ MarloweIndexer . MarloweIndexer.StoreEvent

instance Inject (MarloweIndexer.ChainSeekClientSelector Span) RootSelector where
  inject = injectSelector $ MarloweIndexer . MarloweIndexer.ChainSeekClientEvent

instance Inject MarloweIndexer.QuerySelector RootSelector where
  inject = injectSelector MarloweIndexerDatabase

instance Inject Q.LoadWalletContextSelector RootSelector where
  inject = injectSelector LoadWalletContext

instance Inject Q.LoadMarloweContextSelector RootSelector where
  inject = injectSelector LoadMarloweContext

instance Inject TransactionServerSelector RootSelector where
  inject = injectSelector MarloweTx

instance Inject ContractStoreSelector RootSelector where
  inject = injectSelector ContractStoreSelector

renderRootSelectorOTel
  :: Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> RenderSelectorOTel RootSelector
renderRootSelectorOTel dbName dbUser host port = \case
  ProxyServer sel -> renderTcpServerSelectorOTel sel
  SyncDatabase sel -> Sync.renderDatabaseSelectorOTel dbName dbUser host port sel
  ChainIndexerDatabase sel -> ChainIndexer.renderDatabaseSelectorOTel dbName dbUser host port sel
  ChainIndexer sel -> ChainIndexer.renderChainIndexerSelectorOTel sel
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
  ChainSyncDatabase sel -> ChainSync.renderDatabaseSelectorOTel dbName dbUser host port sel
  ChainSyncNodeService sel -> ChainSync.renderNodeServiceSelectorOTel sel
  MarloweIndexerDatabase sel -> MarloweIndexer.renderDatabaseSelectorOTel dbName dbUser host port sel
  MarloweIndexer sel -> renderMarloweIndexerSelectorOTel sel
  MarloweTx sel -> renderTransactionServerSelectorOTel sel
  LoadWalletContext sel -> renderLoadWalletContextSelectorOTel sel
  LoadMarloweContext sel -> renderLoadMarloweContextSelectorOTel sel
  ContractStoreSelector sel -> renderContractStoreSelectorOTel sel
