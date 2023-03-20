{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Logging
  ( RootSelector(..)
  , defaultRootSelectorLogConfig
  , getRootSelectorConfig
  ) where

import Data.Foldable (fold)
import Data.Map (Map)
import Data.Text (Text)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand, ChainSyncQuery, RuntimeChainSeek)
import Language.Marlowe.Runtime.ChainSync.NodeClient (NodeClientSelector(..), getNodeClientSelectorConfig)
import Network.Protocol.Connection (ConnectorSelector, getConnectorSelectorConfig, getDefaultConnectorLogConfig)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Query.Types (Query)
import Observe.Event.Component
  ( ConfigWatcherSelector(..)
  , GetSelectorConfig
  , SelectorConfig(..)
  , SelectorLogConfig
  , getDefaultLogConfig
  , prependKey
  , singletonFieldConfig
  )

data RootSelector f where
  ChainSeekServer :: ConnectorSelector (Handshake RuntimeChainSeek) f -> RootSelector f
  QueryServer :: ConnectorSelector (Handshake (Query ChainSyncQuery)) f -> RootSelector f
  JobServer :: ConnectorSelector (Handshake (Job ChainSyncCommand)) f -> RootSelector f
  NodeService :: NodeClientSelector f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

-- TODO automate this boilerplate with Template Haskell
getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  ChainSeekServer sel -> prependKey "chain-sync" $ getConnectorSelectorConfig False False sel
  QueryServer sel -> prependKey "query" $ getConnectorSelectorConfig False False sel
  JobServer sel -> prependKey "job" $ getConnectorSelectorConfig False False sel
  NodeService sel -> prependKey "node" $ getNodeClientSelectorConfig sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True
    $ singletonFieldConfig "config" True

defaultRootSelectorLogConfig :: Map Text SelectorLogConfig
defaultRootSelectorLogConfig = fold
  [ getDefaultConnectorLogConfig getRootSelectorConfig ChainSeekServer
  , getDefaultConnectorLogConfig getRootSelectorConfig QueryServer
  , getDefaultConnectorLogConfig getRootSelectorConfig JobServer
  , getDefaultLogConfig getRootSelectorConfig $ NodeService Connect
  , getDefaultLogConfig getRootSelectorConfig $ NodeService Submit
  , getDefaultLogConfig getRootSelectorConfig $ NodeService Query
  , getDefaultLogConfig getRootSelectorConfig $ ConfigWatcher ReloadConfig
  ]
