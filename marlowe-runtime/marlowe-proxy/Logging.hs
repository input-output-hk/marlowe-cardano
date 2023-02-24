{-# LANGUAGE GADTs #-}

module Logging
  ( RootSelector(..)
  , getRootSelectorConfig
  ) where

import Language.Marlowe.Protocol.Types (Marlowe)
import Network.Protocol.Connection (ConnectorSelector, getConnectorSelectorConfig)
import Network.Protocol.Handshake.Types (Handshake)
import Observe.Event.Component
  (ConfigWatcherSelector(ReloadConfig), GetSelectorConfig, SelectorConfig(..), prependKey, singletonFieldConfig)

data RootSelector f where
  MarloweServer :: ConnectorSelector (Handshake Marlowe) f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  MarloweServer sel -> prependKey "proxy-server" $ getConnectorSelectorConfig False False sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True $ singletonFieldConfig "config" True
