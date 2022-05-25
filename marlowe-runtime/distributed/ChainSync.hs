{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
module ChainSync where

import ChainSync.Client (ChainSyncClientConfig, ChainSyncMsg)
import qualified ChainSync.Client as Client
import ChainSync.Logger (ChainSyncLoggerConfig)
import qualified ChainSync.Logger as Logger
import Control.Distributed.Process (Closure, Process, ProcessId, RemoteTable, getSelfPid, link, match, nsend,
                                    receiveWait, send)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Extras.Time (Delay (..), TimeInterval, minutes)
import Control.Distributed.Process.Supervisor (ChildSpec (..), ChildStart (..), ChildStopPolicy (..), ChildType (..),
                                               RegisteredName (..), RestartPolicy (..), ShutdownMode (..), restartOne)
import qualified Control.Distributed.Process.Supervisor as Supervisor
import Control.Monad (forever)
import Data.Binary (Binary)
import Data.Data (Typeable)
import GHC.Generics (Generic)

worker :: String -> RestartPolicy -> Maybe TimeInterval -> ChildStopPolicy -> ChildStart -> ChildSpec
worker name restartPolicy restartDelay stopPolicy start =
  ChildSpec name Worker restartPolicy restartDelay stopPolicy start $ Just $ LocalName name

data ChainSyncConfig = ChainSyncConfig
  { client :: ChainSyncClientConfig
  , logger :: ChainSyncLoggerConfig
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary ChainSyncConfig

chainSync :: (ChainSyncConfig, ProcessId) -> Process ()
chainSync (ChainSyncConfig{..}, parent) = do
  self <- getSelfPid
  supervisor <- Supervisor.start restartOne ParallelShutdown
    [ worker "chain-sync.client" Intrinsic Nothing (StopTimeout (Delay $ minutes 1)) $ RunClosure $ Client.process client self
    , worker "chain-sync.logger" Intrinsic Nothing StopImmediately $ RunClosure $ Logger.process logger
    ]
  link supervisor
  forever $ receiveWait
    [ match \(msg :: ChainSyncMsg) -> do
        nsend "chain-sync.logger" msg
        send parent msg
    ]

remotable ['chainSync]

remoteTable :: RemoteTable -> RemoteTable
remoteTable = __remoteTable . Logger.__remoteTable . Client.__remoteTable

process :: ChainSyncConfig -> ProcessId -> Closure (Process ())
process = curry $(mkClosure 'chainSync)
