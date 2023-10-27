{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Logging (
  RootSelector (..),
  renderRootSelectorOTel,
) where

import Control.Monad.Event.Class (Inject (..))
import Data.ByteString (ByteString)
import Language.Marlowe.Runtime.ChainSync.Api (
  ChainSyncQueryClientSelector,
  RuntimeChainSeekClientSelector,
  renderChainSeekClientSelectorOTel,
  renderChainSyncQueryClientSelector,
 )
import Language.Marlowe.Runtime.Indexer (
  MarloweIndexerSelector (..),
  renderDatabaseSelectorOTel,
  renderMarloweIndexerSelectorOTel,
 )
import Language.Marlowe.Runtime.Indexer.ChainSeekClient (ChainSeekClientSelector (..))
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL (QuerySelector (..))
import Language.Marlowe.Runtime.Indexer.Store (StoreSelector (..))
import qualified Network.Protocol.Peer.Monad.TCP as PeerT
import Observe.Event (idInjectSelector, injectSelector)
import Observe.Event.Render.OpenTelemetry
import OpenTelemetry.Trace

data RootSelector f where
  ChainQueryClient :: PeerT.TcpClientSelector ChainSyncQueryClientSelector f -> RootSelector f
  Database :: QuerySelector f -> RootSelector f
  App :: MarloweIndexerSelector Span f -> RootSelector f
  ChainSeekClient :: PeerT.TcpClientSelector RuntimeChainSeekClientSelector f -> RootSelector f

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
  ChainQueryClient sel -> PeerT.renderTcpClientSelectorOTel renderChainSyncQueryClientSelector sel
  Database sel -> renderDatabaseSelectorOTel dbName dbUser host port sel
  App sel -> renderMarloweIndexerSelectorOTel sel
  ChainSeekClient sel -> PeerT.renderTcpClientSelectorOTel renderChainSeekClientSelectorOTel sel
