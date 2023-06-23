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
import qualified Language.Marlowe.Runtime.ChainIndexer.NodeClient as ChainIndexer
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

data RootSelector r f where
  ProxyServer :: TcpServerSelector (Handshake MarloweRuntime) f -> RootSelector r f
  SyncDatabase :: Sync.DatabaseSelector f -> RootSelector r f
  ChainIndexerDatabase :: ChainIndexer.QuerySelector f -> RootSelector r f
  ChainIndexer :: ChainIndexerSelector r f -> RootSelector r f
  ConnectToNode :: RootSelector r (LocalNodeConnectInfo CardanoMode)
  ChainSyncDatabase :: ChainSync.QuerySelector f -> RootSelector r f
  ChainSyncNodeService :: ChainSync.NodeClientSelector f -> RootSelector r f
  MarloweIndexerDatabase :: MarloweIndexer.QuerySelector f -> RootSelector r f
  MarloweIndexer :: MarloweIndexerSelector Span f -> RootSelector r f
  MarloweTx :: TransactionServerSelector f -> RootSelector r f
  LoadWalletContext :: Q.LoadWalletContextSelector f -> RootSelector r f
  LoadMarloweContext :: Q.LoadMarloweContextSelector f -> RootSelector r f
  ContractStoreSelector :: ContractStoreSelector f -> RootSelector r f

instance Inject (RootSelector r) (RootSelector r) where
  inject = idInjectSelector

instance Inject Sync.DatabaseSelector (RootSelector r) where
  inject = injectSelector SyncDatabase

instance Inject ChainIndexer.QuerySelector (RootSelector r) where
  inject = injectSelector ChainIndexerDatabase

instance Inject ChainIndexer.NodeClientSelector (RootSelector r) where
  inject = injectSelector $ ChainIndexer . ChainIndexer.NodeClientEvent

instance Inject (ChainStoreSelector r) (RootSelector r) where
  inject = injectSelector $ ChainIndexer . ChainIndexer.ChainStoreEvent

instance Inject ChainSync.QuerySelector (RootSelector r) where
  inject = injectSelector ChainSyncDatabase

instance Inject ChainSync.NodeClientSelector (RootSelector r) where
  inject = injectSelector ChainSyncNodeService

instance Inject MarloweIndexer.StoreSelector (RootSelector r) where
  inject = injectSelector $ MarloweIndexer . MarloweIndexer.StoreEvent

instance Inject (MarloweIndexer.ChainSeekClientSelector Span) (RootSelector Span) where
  inject = injectSelector $ MarloweIndexer . MarloweIndexer.ChainSeekClientEvent

instance Inject MarloweIndexer.QuerySelector (RootSelector r) where
  inject = injectSelector MarloweIndexerDatabase

instance Inject Q.LoadWalletContextSelector (RootSelector r) where
  inject = injectSelector LoadWalletContext

instance Inject Q.LoadMarloweContextSelector (RootSelector r) where
  inject = injectSelector LoadMarloweContext

instance Inject TransactionServerSelector (RootSelector r) where
  inject = injectSelector MarloweTx

instance Inject ContractStoreSelector (RootSelector r) where
  inject = injectSelector ContractStoreSelector

renderRootSelectorOTel
  :: Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> RenderSelectorOTel (RootSelector r)
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
