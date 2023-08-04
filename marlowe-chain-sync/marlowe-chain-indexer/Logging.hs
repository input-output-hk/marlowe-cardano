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
import Language.Marlowe.Runtime.ChainIndexer (
  ChainIndexerSelector (..),
  renderChainIndexerSelectorOTel,
  renderDatabaseSelectorOTel,
 )
import Language.Marlowe.Runtime.ChainIndexer.Database.PostgreSQL (QuerySelector (..))
import Language.Marlowe.Runtime.ChainIndexer.Store (ChainStoreSelector (..))
import Observe.Event (idInjectSelector, injectSelector)
import Observe.Event.Render.OpenTelemetry

data RootSelector f where
  App :: ChainIndexerSelector f -> RootSelector f
  Database :: QuerySelector f -> RootSelector f

instance Inject RootSelector RootSelector where
  inject = idInjectSelector

instance Inject QuerySelector RootSelector where
  inject = injectSelector Database

instance Inject ChainStoreSelector RootSelector where
  inject = injectSelector $ App . ChainStoreEvent

renderRootSelectorOTel
  :: Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> RenderSelectorOTel RootSelector
renderRootSelectorOTel dbName dbUser host port = \case
  App sel -> renderChainIndexerSelectorOTel sel
  Database sel -> renderDatabaseSelectorOTel dbName dbUser host port sel
