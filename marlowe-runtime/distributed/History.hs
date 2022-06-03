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
module History where

import ChainSync.Client (ChainSyncMsg)
import ChainSync.Database (ChainSyncQueryChan)
import ChainSync.Store (ChainStoreQuery)
import Control.Distributed.Process (Closure, Process, RemoteTable, SendPort, match, matchChan, newChan, nsend,
                                    receiveWait)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Extras (spawnLinkLocal)
import Control.Distributed.Process.Extras.Time (Delay (..), TimeInterval)
import Control.Distributed.Process.Supervisor (ChildSpec (..), ChildStart (..), ChildStopPolicy (..), ChildType (..),
                                               RegisteredName (..), RestartPolicy (..), ShutdownMode (..), restartOne,
                                               restartRight)
import qualified Control.Distributed.Process.Supervisor as Supervisor
import Control.Monad (forever)
import Data.Binary (Binary)
import Data.Data (Typeable)
import Data.Foldable (traverse_)
import GHC.Generics (Generic)
import History.Database (HistoryQueryChan)
import qualified History.Database as Database
import History.Digest (HistoryDigestConfig)
import qualified History.Digest as Digest
import History.Logger (HistoryLoggerConfig)
import qualified History.Logger as Logger
import History.Store (HistoryStoreChan)
import qualified History.Store as Store

supervisor :: String -> ChildStart -> ChildSpec
supervisor name start =
  ChildSpec name Supervisor Intrinsic Nothing (StopTimeout Infinity) start
    $ Just
    $ LocalName name

worker :: String -> RestartPolicy -> Maybe TimeInterval -> ChildStopPolicy -> ChildStart -> ChildSpec
worker name restartPolicy restartDelay stopPolicy start =
  ChildSpec name Worker restartPolicy restartDelay stopPolicy start $ Just $ LocalName name

data HistoryConfig = HistoryConfig
  { digest :: HistoryDigestConfig
  , logger :: HistoryLoggerConfig
  -- , store  :: HistoryStoreConfig
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary HistoryConfig

data HistoryDependencies = HistoryDependencies
  { config           :: HistoryConfig
  , chainDbChan      :: ChainSyncQueryChan
  , chainStoreChan   :: SendPort ChainStoreQuery
  , historyStoreChan :: HistoryStoreChan
  , initDbChan       :: SendPort HistoryQueryChan
  , initStoreChan    :: SendPort HistoryStoreChan
  , dbChan           :: HistoryQueryChan
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary HistoryDependencies

history :: HistoryDependencies -> Process ()
history HistoryDependencies{..} = do
  let HistoryConfig{..} = config
  (sendEvents, receiveEvents) <- newChan
  _ <- spawnLinkLocal $ Supervisor.run restartRight ParallelShutdown
    [ worker "history.logger" Permanent Nothing StopImmediately $ RunClosure $ Logger.process logger
    , worker "history.store" Permanent Nothing StopImmediately $ RunClosure $ Store.process $ Store.HistoryStoreDependencies dbChan initStoreChan
    , worker "history.database" Permanent Nothing StopImmediately $ RunClosure $ Database.process $ Database.HistoryDatabaseDependencies initDbChan chainDbChan
    , worker "history.digest" Permanent Nothing StopImmediately $ RunClosure $ Digest.process $ Digest.HistoryDigestDependencies digest sendEvents chainStoreChan dbChan
    ]
  forever $ receiveWait
    [ match \(msg :: ChainSyncMsg) -> do
        nsend "history.logger" msg
        nsend "history.store" msg
    , matchChan receiveEvents \events -> do
        traverse_ (nsend "history.logger") events
        nsend "history.store" events
    ]

remotable ['history]

remoteTable :: RemoteTable -> RemoteTable
remoteTable = __remoteTable . Logger.__remoteTable . Digest.__remoteTable . Database.__remoteTable . Store.__remoteTable

process :: HistoryDependencies -> Closure (Process ())
process = $(mkClosure 'history)
