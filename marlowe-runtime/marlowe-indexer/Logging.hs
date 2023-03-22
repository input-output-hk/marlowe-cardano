{-# LANGUAGE GADTs #-}

module Logging
  ( RootSelector(..)
  , defaultRootSelectorLogConfig
  , getRootSelectorConfig
  ) where

import Data.Foldable (fold)
import Data.Map (Map)
import Data.Text (Text)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery, RuntimeChainSeek)
import Language.Marlowe.Runtime.Indexer (MarloweIndexerSelector(..), getMarloweIndexerSelectorConfig)
import Language.Marlowe.Runtime.Indexer.ChainSeekClient (ChainSeekClientSelector(..))
import Language.Marlowe.Runtime.Indexer.Store (StoreSelector(..))
import Network.Protocol.Connection (ConnectorSelector, getConnectorSelectorConfig, getDefaultConnectorLogConfig)
import Network.Protocol.Handshake.Types (Handshake)
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
  ChainSeekClient :: ConnectorSelector (Handshake RuntimeChainSeek) f -> RootSelector f
  ChainQueryClient :: ConnectorSelector (Handshake (Query ChainSyncQuery)) f -> RootSelector f
  App :: MarloweIndexerSelector f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  ChainSeekClient sel -> prependKey "chain-sync" $ getConnectorSelectorConfig False False sel
  ChainQueryClient sel -> prependKey "chain-query" $ getConnectorSelectorConfig False False sel
  App sel -> prependKey "marlowe-indexer" $ getMarloweIndexerSelectorConfig sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True
    $ singletonFieldConfig "config" True

defaultRootSelectorLogConfig :: Map Text SelectorLogConfig
defaultRootSelectorLogConfig = fold
  [ getDefaultConnectorLogConfig getRootSelectorConfig ChainSeekClient
  , getDefaultConnectorLogConfig getRootSelectorConfig ChainQueryClient
  , getDefaultLogConfig getRootSelectorConfig $ App $ StoreEvent Save
  , getDefaultLogConfig getRootSelectorConfig $ App $ ChainSeekClientEvent LoadMarloweUTxO
  , getDefaultLogConfig getRootSelectorConfig $ ConfigWatcher ReloadConfig
  ]
