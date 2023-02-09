{-# LANGUAGE GADTs #-}

module Logging
  ( RootSelector(..)
  , getRootSelectorConfig
  ) where

import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import Language.Marlowe.Runtime.ChainSync.Api (RuntimeChainSeek)
import Language.Marlowe.Runtime.Discovery.Api (DiscoveryQuery)
import Network.Protocol.Driver
  ( AcceptSocketDriverSelector
  , ConnectSocketDriverSelector
  , SocketDriverConfigOptions(..)
  , getAcceptSocketDriverSelectorConfig
  , getConnectSocketDriverSelectorConfig
  )
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Query.Types (Query)
import Observe.Event.Component
  (ConfigWatcherSelector(ReloadConfig), GetSelectorConfig, SelectorConfig(..), prependKey, singletonFieldConfig)

data RootSelector f where
  ChainSeekClient :: ConnectSocketDriverSelector (Handshake RuntimeChainSeek) f -> RootSelector f
  QueryServer :: AcceptSocketDriverSelector (Handshake (Query DiscoveryQuery)) f -> RootSelector f
  SyncServer :: AcceptSocketDriverSelector (Handshake MarloweHeaderSync) f -> RootSelector f
  -- App :: ChainSyncSelector f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

-- TODO automate this boilerplate with Template Haskell
getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  ChainSeekClient sel -> prependKey "chain-seek" $ getConnectSocketDriverSelectorConfig chainSeekConfig sel
  QueryServer sel -> prependKey "query" $ getAcceptSocketDriverSelectorConfig queryConfig sel
  SyncServer sel -> prependKey "sync" $ getAcceptSocketDriverSelectorConfig syncConfig sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True
    $ singletonFieldConfig "config" True

chainSeekConfig :: SocketDriverConfigOptions
chainSeekConfig = SocketDriverConfigOptions
  { enableConnected = True
  , enableDisconnected = True
  , enableServerDriverEvent = False
  }

queryConfig :: SocketDriverConfigOptions
queryConfig = SocketDriverConfigOptions
  { enableConnected = True
  , enableDisconnected = True
  , enableServerDriverEvent = True
  }

syncConfig :: SocketDriverConfigOptions
syncConfig = SocketDriverConfigOptions
  { enableConnected = True
  , enableDisconnected = True
  , enableServerDriverEvent = False
  }
