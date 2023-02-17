{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Logging
  ( RootSelector(..)
  , getRootSelectorConfig
  ) where

import Data.ByteString.Lazy (ByteString)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand, ChainSyncQuery, RuntimeChainSeek)
import Network.Protocol.Driver (ConnectionSourceSelector, getConnectionSourceSelectorConfig)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Query.Types (Query)
import Observe.Event.Component
  (ConfigWatcherSelector(..), GetSelectorConfig, SelectorConfig(..), prependKey, singletonFieldConfig)

data RootSelector f where
  ChainSeekServer :: ConnectionSourceSelector (Handshake RuntimeChainSeek) ByteString f -> RootSelector f
  QueryServer :: ConnectionSourceSelector (Handshake (Query ChainSyncQuery)) ByteString f -> RootSelector f
  JobServer :: ConnectionSourceSelector (Handshake (Job ChainSyncCommand)) ByteString f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

-- TODO automate this boilerplate with Template Haskell
getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  ChainSeekServer sel -> prependKey "chain-sync" $ getConnectionSourceSelectorConfig True False sel
  QueryServer sel -> prependKey "query" $ getConnectionSourceSelectorConfig True True sel
  JobServer sel -> prependKey "job" $ getConnectionSourceSelectorConfig True True sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True
    $ singletonFieldConfig "config" True
