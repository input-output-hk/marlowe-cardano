{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Logging
  ( RootSelector(..)
  , defaultRootSelectorLogConfig
  , getRootSelectorConfig
  ) where

import Data.Aeson (ToJSON(toJSON), Value)
import Data.Foldable (fold)
import Language.Marlowe.Runtime.ChainIndexer (ChainIndexerSelector(..), getChainIndexerSelectorConfig)
import Language.Marlowe.Runtime.ChainIndexer.NodeClient (NodeClientSelector(..))
import Language.Marlowe.Runtime.ChainIndexer.Store (ChainStoreSelector(..))
import Observe.Event.Component
  (ConfigWatcherSelector(..), GetSelectorConfig, SelectorConfig(..), getDefaultLogConfig, singletonFieldConfig)

data RootSelector f where
  App :: ChainIndexerSelector f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

-- TODO automate this boilerplate with Template Haskell
getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  App sel -> getChainIndexerSelectorConfig sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True
    $ singletonFieldConfig "config" True

defaultRootSelectorLogConfig :: Value
defaultRootSelectorLogConfig = toJSON $ fold
  [ getDefaultLogConfig (App $ NodeClientEvent Connect) getRootSelectorConfig
  , getDefaultLogConfig (App $ NodeClientEvent Intersect) getRootSelectorConfig
  , getDefaultLogConfig (App $ NodeClientEvent IntersectFound) getRootSelectorConfig
  , getDefaultLogConfig (App $ NodeClientEvent IntersectNotFound) getRootSelectorConfig
  , getDefaultLogConfig (App $ NodeClientEvent RollForward) getRootSelectorConfig
  , getDefaultLogConfig (App $ NodeClientEvent RollBackward) getRootSelectorConfig
  , getDefaultLogConfig (App $ ChainStoreEvent CheckGenesisBlock) getRootSelectorConfig
  , getDefaultLogConfig (App $ ChainStoreEvent Save) getRootSelectorConfig
  , getDefaultLogConfig (ConfigWatcher ReloadConfig) getRootSelectorConfig
  ]
