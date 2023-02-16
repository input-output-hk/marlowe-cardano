{-# LANGUAGE GADTs #-}

module Logging
  ( RootSelector(..)
  , getRootSelectorConfig
  ) where

import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import Language.Marlowe.Runtime.ChainSync.Api (RuntimeChainSeek)
import Language.Marlowe.Runtime.History.Api (HistoryCommand, HistoryQuery)
import Network.Protocol.Driver
  ( AcceptSocketDriverSelector
  , ConnectSocketDriverSelector
  , SocketDriverConfigOptions(..)
  , getAcceptSocketDriverSelectorConfig
  , getConnectSocketDriverSelectorConfig
  )
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Query.Types (Query)
import Observe.Event.Component
  (ConfigWatcherSelector(ReloadConfig), GetSelectorConfig, SelectorConfig(..), prependKey, singletonFieldConfig)

data RootSelector f where
  ChainSeekClient :: ConnectSocketDriverSelector (Handshake RuntimeChainSeek) f -> RootSelector f
  QueryServer :: AcceptSocketDriverSelector (Handshake (Query HistoryQuery)) f -> RootSelector f
  JobServer :: AcceptSocketDriverSelector (Handshake (Job HistoryCommand)) f -> RootSelector f
  SyncServer :: AcceptSocketDriverSelector (Handshake MarloweSync) f -> RootSelector f
  -- App :: ChainSyncSelector f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

-- TODO automate this boilerplate with Template Haskell
getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  ChainSeekClient sel -> prependKey "chain-sync" $ getConnectSocketDriverSelectorConfig chainSeekConfig sel
  QueryServer sel -> prependKey "query" $ getAcceptSocketDriverSelectorConfig queryConfig sel
  JobServer sel -> prependKey "job" $ getAcceptSocketDriverSelectorConfig jobConfig sel
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

jobConfig :: SocketDriverConfigOptions
jobConfig = SocketDriverConfigOptions
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
