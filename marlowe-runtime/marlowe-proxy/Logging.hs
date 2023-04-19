{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Logging
  ( RootSelector(..)
  , defaultRootSelectorLogConfig
  , getRootSelectorConfig
  ) where

import Control.Monad.Event.Class
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Text (Text)
import Language.Marlowe.Protocol.Types (MarloweRuntime)
import Network.Protocol.Connection (ConnectorSelector, getConnectorSelectorConfig, getDefaultConnectorLogConfig)
import Network.Protocol.Handshake.Types (Handshake)
import Observe.Event (idInjectSelector)
import Observe.Event.Component
  ( ConfigWatcherSelector(ReloadConfig)
  , GetSelectorConfig
  , SelectorConfig(..)
  , SelectorLogConfig
  , getDefaultLogConfig
  , prependKey
  , singletonFieldConfig
  )

data RootSelector f where
  MarloweRuntimeServer :: ConnectorSelector (Handshake MarloweRuntime) f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

instance Inject RootSelector RootSelector where
  inject = idInjectSelector

getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  MarloweRuntimeServer sel -> prependKey "proxy-server" $ getConnectorSelectorConfig False False sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True $ singletonFieldConfig "config" True

defaultRootSelectorLogConfig :: Map Text SelectorLogConfig
defaultRootSelectorLogConfig = fold
  [ getDefaultConnectorLogConfig getRootSelectorConfig MarloweRuntimeServer
  , getDefaultLogConfig getRootSelectorConfig $ ConfigWatcher ReloadConfig
  ]
