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

import ChainSync.Client (ChainSyncMsg, ChainSyncQuery)
import Control.Distributed.Process (Closure, Process, RemoteTable, SendPort, match, matchChan, newChan, nsend,
                                    receiveWait)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Extras (Routable (sendTo), spawnLinkLocal)
import Control.Distributed.Process.Extras.Time (Delay (..), TimeInterval)
import Control.Distributed.Process.Supervisor (ChildSpec (..), ChildStart (..), ChildStopPolicy (..), ChildType (..),
                                               RegisteredName (..), RestartPolicy (..), ShutdownMode (..), restartOne)
import qualified Control.Distributed.Process.Supervisor as Supervisor
import Control.Monad (forever)
import Data.Binary (Binary)
import Data.Data (Typeable)
import GHC.Generics (Generic)
import History.Digest (HistoryDigestConfig)
import qualified History.Digest as Digest
import History.Logger (HistoryLoggerConfig)
import qualified History.Logger as Logger

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
  { config      :: HistoryConfig
  , sendRequest :: SendPort ChainSyncQuery
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary HistoryDependencies

history :: HistoryDependencies -> Process ()
history HistoryDependencies{..} = do
  let HistoryConfig{..} = config
  (sendEvent, receiveEvent) <- newChan
  _ <- spawnLinkLocal $ Supervisor.run restartOne ParallelShutdown
    [ supervisor "history.digest" $ RunClosure $ Digest.process $ Digest.HistoryDigestDependencies digest sendEvent sendRequest
    , worker "history.logger" Permanent Nothing StopImmediately $ RunClosure $ Logger.process logger
    ]
  forever $ receiveWait
    [ match \(msg :: ChainSyncMsg) -> do
        sendTo "history.digest" msg
        nsend "history.logger" msg
    , matchChan receiveEvent \event -> do
        nsend "history.logger" event
    ]

remotable ['history]

remoteTable :: RemoteTable -> RemoteTable
remoteTable = __remoteTable . Logger.__remoteTable . Digest.remoteTable

process :: HistoryDependencies -> Closure (Process ())
process = $(mkClosure 'history)
