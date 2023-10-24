{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Logging (
  RootSelector (..),
  renderRootSelectorOTel,
) where

import Control.Monad.Event.Class (Inject (..))
import Data.ByteString (ByteString)
import Language.Marlowe.Protocol.BulkSync.Types (MarloweBulkSync)
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import Language.Marlowe.Runtime.ChainSync.Api (
  ChainSyncQueryClientSelector,
  renderChainSyncQueryClientSelector,
 )
import Language.Marlowe.Runtime.Sync (renderDatabaseSelectorOTel)
import Language.Marlowe.Runtime.Sync.Database
import Network.Protocol.Driver.Trace (
  TcpServerSelector,
  renderTcpServerSelectorOTel,
 )
import Network.Protocol.Handshake.Types (Handshake)
import qualified Network.Protocol.Peer.Monad.TCP as PeerT
import Observe.Event (idInjectSelector, injectSelector)
import Observe.Event.Render.OpenTelemetry
import Prelude hiding (filter)

data RootSelector f where
  ChainSyncQueryClient :: PeerT.TcpClientSelector ChainSyncQueryClientSelector f -> RootSelector f
  MarloweSyncServer :: TcpServerSelector (Handshake MarloweSync) f -> RootSelector f
  MarloweHeaderSyncServer :: TcpServerSelector (Handshake MarloweHeaderSync) f -> RootSelector f
  MarloweBulkSyncServer :: TcpServerSelector (Handshake MarloweBulkSync) f -> RootSelector f
  MarloweQueryServer :: TcpServerSelector (Handshake MarloweQuery) f -> RootSelector f
  Database :: DatabaseSelector f -> RootSelector f

instance Inject RootSelector RootSelector where
  inject = idInjectSelector

instance Inject DatabaseSelector RootSelector where
  inject = injectSelector Database

renderRootSelectorOTel
  :: Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> RenderSelectorOTel RootSelector
renderRootSelectorOTel dbName dbUser host port = \case
  ChainSyncQueryClient sel -> PeerT.renderTcpClientSelectorOTel renderChainSyncQueryClientSelector sel
  MarloweSyncServer sel -> renderTcpServerSelectorOTel sel
  MarloweHeaderSyncServer sel -> renderTcpServerSelectorOTel sel
  MarloweBulkSyncServer sel -> renderTcpServerSelectorOTel sel
  MarloweQueryServer sel -> renderTcpServerSelectorOTel sel
  Database sel -> renderDatabaseSelectorOTel dbName dbUser host port sel
