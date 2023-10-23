{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Logging (
  RootSelector (..),
  renderRootSelectorOTel,
) where

import Control.Monad.Event.Class (Inject (..))
import Data.ByteString (ByteString)
import Language.Marlowe.Runtime.ChainSync (renderDatabaseSelectorOTel, renderNodeServiceSelectorOTel)
import Language.Marlowe.Runtime.ChainSync.Api (
  ChainSyncCommand,
  ChainSyncQuery,
  RuntimeChainSeekServerSelector,
  renderChainSeekServerSelectorOTel,
 )
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as DB
import Language.Marlowe.Runtime.ChainSync.NodeClient (NodeClientSelector (..))
import Network.Protocol.Driver.Trace (TcpServerSelector, renderTcpServerSelectorOTel)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Job.Types (Job)
import qualified Network.Protocol.Peer.Monad.TCP as PeerT
import Network.Protocol.Query.Types (Query)
import Observe.Event (idInjectSelector, injectSelector)
import Observe.Event.Render.OpenTelemetry

data RootSelector f where
  ChainSeekServer :: PeerT.TcpServerSelector RuntimeChainSeekServerSelector f -> RootSelector f
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
  ChainSeekServer sel -> PeerT.renderTcpServerSelectorOTel renderChainSeekServerSelectorOTel sel
  QueryServer sel -> renderTcpServerSelectorOTel sel
  JobServer sel -> renderTcpServerSelectorOTel sel
  Database sel -> renderDatabaseSelectorOTel dbName dbUser host port sel
  NodeService sel -> renderNodeServiceSelectorOTel sel
