{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
module ChainSync where

import ChainSync.Client (ChainSyncClientConfig, ChainSyncClientDependencies (ChainSyncClientDependencies), ChainSyncMsg)
import qualified ChainSync.Client as Client
import ChainSync.Database (ChainSyncDatabaseDependencies (..), ChainSyncQuery)
import qualified ChainSync.Database as Database
import ChainSync.Logger (ChainSyncLoggerConfig)
import qualified ChainSync.Logger as Logger
import ChainSync.Store (ChainStoreQuery, ChainSyncStoreDependencies (ChainSyncStoreDependencies))
import qualified ChainSync.Store as Store
import Control.Distributed.Process (Closure, Process, RemoteTable, SendPort, newChan, receiveChan, sendChan)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Extras (Routable (sendTo), spawnLinkLocal)
import Control.Distributed.Process.Extras.Time (Delay (..), TimeInterval, minutes, seconds)
import Control.Distributed.Process.Supervisor (ChildSpec (..), ChildStart (..), ChildStopPolicy (..), ChildType (..),
                                               RegisteredName (..), RestartLimit (..), RestartMode (..),
                                               RestartOrder (..), RestartPolicy (..), RestartStrategy (..),
                                               ShutdownMode (..), maxRestarts)
import qualified Control.Distributed.Process.Supervisor as Supervisor
import Control.Monad (forever)
import Data.Binary (Binary)
import Data.Data (Typeable)
import GHC.Generics (Generic)

worker :: String -> RestartPolicy -> Maybe TimeInterval -> ChildStopPolicy -> ChildStart -> ChildSpec
worker name restartPolicy restartDelay stopPolicy start =
  ChildSpec name Worker restartPolicy restartDelay stopPolicy start $ Just $ LocalName name

data ChainSyncDependencies = ChainSyncDependencies
  { config        :: ChainSyncConfig
  , msgChan       :: SendPort ChainSyncMsg
  , initDbChan    :: SendPort (SendPort ChainSyncQuery)
  , initStoreChan :: SendPort (SendPort ChainStoreQuery)
  , dbChan        :: SendPort ChainSyncQuery
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary ChainSyncDependencies

data ChainSyncConfig = ChainSyncConfig
  { client :: ChainSyncClientConfig
  , logger :: ChainSyncLoggerConfig
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary ChainSyncConfig

chainSync :: ChainSyncDependencies -> Process ()
chainSync ChainSyncDependencies{..} = do
  (sendMsg, receiveMsg) <- newChan
  let ChainSyncConfig{..} =  config
  let limit = RestartLimit (maxRestarts 20) (seconds 1)
  let mode = RestartRevOrder RightToLeft
  _ <- spawnLinkLocal $ Supervisor.run (RestartRight limit mode) ParallelShutdown
    [ worker "chain-sync.logger" Permanent Nothing StopImmediately $ RunClosure $ Logger.process logger
    , worker "chain-sync.store" Permanent Nothing StopImmediately $ RunClosure $ Store.process $ ChainSyncStoreDependencies dbChan initStoreChan
    , worker "chain-sync.db" Permanent Nothing StopImmediately $ RunClosure $ Database.process $ ChainSyncDatabaseDependencies initDbChan
    , worker "chain-sync.client" Intrinsic (Just (seconds 5)) StopImmediately $ RunClosure $ Client.process $ ChainSyncClientDependencies client sendMsg dbChan
    ]
  forever do
    msg <- receiveChan receiveMsg
    sendTo "chain-sync.logger" msg
    sendTo "chain-sync.store" msg
    sendChan msgChan msg

remotable ['chainSync]

remoteTable :: RemoteTable -> RemoteTable
remoteTable = __remoteTable . Logger.__remoteTable . Client.__remoteTable . Store.__remoteTable . Database.__remoteTable

process :: ChainSyncDependencies -> Closure (Process ())
process = $(mkClosure 'chainSync)
