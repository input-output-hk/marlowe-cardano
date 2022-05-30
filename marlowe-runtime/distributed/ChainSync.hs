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

import ChainSync.Client (ChainSyncClientConfig, ChainSyncClientDependencies (ChainSyncClientDependencies), ChainSyncMsg,
                         ChainSyncQuery)
import qualified ChainSync.Client as Client
import ChainSync.Logger (ChainSyncLoggerConfig)
import qualified ChainSync.Logger as Logger
import ChainSync.Store (ChainSyncStoreConfig, ChainSyncStoreDependencies (ChainSyncStoreDependencies))
import qualified ChainSync.Store as Store
import Control.Distributed.Process (Closure, Process, RemoteTable, SendPort, newChan, receiveChan, sendChan)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Extras (Routable (sendTo), spawnLinkLocal)
import Control.Distributed.Process.Extras.Time (Delay (..), TimeInterval, minutes, seconds)
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

data ChainSyncDependencies = ChainSyncDependencies
  { config          :: ChainSyncConfig
  , msgChan         :: SendPort ChainSyncMsg
  , initRequestChan :: SendPort (SendPort ChainSyncQuery)
  , sendRequest     :: SendPort ChainSyncQuery
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary ChainSyncDependencies

data ChainSyncConfig = ChainSyncConfig
  { client :: ChainSyncClientConfig
  , logger :: ChainSyncLoggerConfig
  , store  :: ChainSyncStoreConfig
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary ChainSyncConfig

chainSync :: ChainSyncDependencies -> Process ()
chainSync ChainSyncDependencies{..} = do
  (sendMsg, receiveMsg) <- newChan
  let ChainSyncConfig{..} =  config
  _ <- spawnLinkLocal $ Supervisor.run restartOne ParallelShutdown
    [ worker "chain-sync.store" Permanent Nothing StopImmediately $ RunClosure $ Store.process $ ChainSyncStoreDependencies store initRequestChan
    , worker "chain-sync.client" Intrinsic (Just (seconds 5)) (StopTimeout (Delay $ minutes 1)) $ RunClosure $ Client.process $ ChainSyncClientDependencies client sendMsg sendRequest
    , worker "chain-sync.logger" Permanent Nothing StopImmediately $ RunClosure $ Logger.process logger
    ]
  forever do
    msg <- receiveChan receiveMsg
    sendTo "chain-sync.logger" msg
    sendTo "chain-sync.store" msg
    sendChan msgChan msg

remotable ['chainSync]

remoteTable :: RemoteTable -> RemoteTable
remoteTable = __remoteTable . Logger.__remoteTable . Client.__remoteTable . Store.__remoteTable

process :: ChainSyncDependencies -> Closure (Process ())
process = $(mkClosure 'chainSync)
