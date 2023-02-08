{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Logging
  ( RootSelector(..)
  , getRootSelectorConfig
  ) where

import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand, ChainSyncQuery, RuntimeChainSeek)
import Network.Protocol.Driver
  (AcceptSocketDriverSelector(..), SocketDriverConfigOptions(..), getAcceptSocketDriverSelectorConfig)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Query.Types (Query)
import Observe.Event.Component
  (ConfigWatcherSelector(..), GetSelectorConfig, SelectorConfig(..), prependKey, singletonFieldConfig)

data RootSelector f where
  ChainSeekServer :: AcceptSocketDriverSelector (Handshake RuntimeChainSeek) f -> RootSelector f
  QueryServer :: AcceptSocketDriverSelector (Handshake (Query ChainSyncQuery)) f -> RootSelector f
  JobServer :: AcceptSocketDriverSelector (Handshake (Job ChainSyncCommand)) f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

-- TODO automate this boilerplate with Template Haskell
getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  ChainSeekServer sel -> prependKey "chain-seek" $ getAcceptSocketDriverSelectorConfig chainSeekConfig sel
  QueryServer sel -> prependKey "query" $ getAcceptSocketDriverSelectorConfig queryConfig sel
  JobServer sel -> prependKey "job" $ getAcceptSocketDriverSelectorConfig jobConfig sel
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
