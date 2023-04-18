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
import Language.Marlowe.Runtime.ChainIndexer (ChainIndexerSelector(..), getChainIndexerSelectorConfig)
import Language.Marlowe.Runtime.ChainIndexer.Database.PostgreSQL (QuerySelector(..), getQuerySelectorConfig)
import Language.Marlowe.Runtime.ChainIndexer.NodeClient (NodeClientSelector(..))
import Language.Marlowe.Runtime.ChainIndexer.Store (ChainStoreSelector(..))
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
  App :: ChainIndexerSelector f -> RootSelector f
  Database :: QuerySelector f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

-- TODO automate this boilerplate with Template Haskell
getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  App sel -> getChainIndexerSelectorConfig sel
  Database sel -> prependKey "database" $ getQuerySelectorConfig sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True
    $ singletonFieldConfig "config" True

defaultRootSelectorLogConfig :: Map Text SelectorLogConfig
defaultRootSelectorLogConfig = fold
  [ getDefaultLogConfig getRootSelectorConfig $ Database $ Query "commitRollback"
  , getDefaultLogConfig getRootSelectorConfig $ Database $ Query "commitBlocks"
  , getDefaultLogConfig getRootSelectorConfig $ Database $ Query "commitGenesisBlock"
  , getDefaultLogConfig getRootSelectorConfig $ Database $ Query "getIntersectionPoints"
  , getDefaultLogConfig getRootSelectorConfig $ Database $ Query "getGenesisBlock"
  , getDefaultLogConfig getRootSelectorConfig (App $ NodeClientEvent Intersect)
  , getDefaultLogConfig getRootSelectorConfig (App $ NodeClientEvent IntersectFound)
  , getDefaultLogConfig getRootSelectorConfig (App $ NodeClientEvent IntersectNotFound)
  , getDefaultLogConfig getRootSelectorConfig (App $ NodeClientEvent RollForward)
  , getDefaultLogConfig getRootSelectorConfig (App $ NodeClientEvent RollBackward)
  , getDefaultLogConfig getRootSelectorConfig (App $ ChainStoreEvent CheckGenesisBlock)
  , getDefaultLogConfig getRootSelectorConfig (App $ ChainStoreEvent Save)
  , getDefaultLogConfig getRootSelectorConfig (ConfigWatcher ReloadConfig)
  ]
