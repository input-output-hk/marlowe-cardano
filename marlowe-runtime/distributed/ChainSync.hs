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

import ChainSync.Client (ChainSyncClientConfig, ChainSyncMsg)
import qualified ChainSync.Client as Client
import ChainSync.Logger (ChainSyncLoggerConfig)
import qualified ChainSync.Logger as Logger
import ChainSync.Store (ChainSyncStoreConfig)
import qualified ChainSync.Store as Store
import Control.Distributed.Process (Closure, Process, ProcessId, RemoteTable, getSelfPid, match, receiveWait, send)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Extras (Routable (sendTo), spawnLinkLocal)
import Control.Distributed.Process.Extras.Time (Delay (..), TimeInterval, minutes, seconds)
import Control.Distributed.Process.Supervisor (ChildSpec (..), ChildStart (..), ChildStopPolicy (..), ChildType (..),
                                               RegisteredName (..), RestartPolicy (..), ShutdownMode (..), restartOne)
import qualified Control.Distributed.Process.Supervisor as Supervisor
import Data.Binary (Binary)
import Data.Data (Typeable)
import Data.Foldable (traverse_)
import qualified Data.Set as Set
import GHC.Generics (Generic)

worker :: String -> RestartPolicy -> Maybe TimeInterval -> ChildStopPolicy -> ChildStart -> ChildSpec
worker name restartPolicy restartDelay stopPolicy start =
  ChildSpec name Worker restartPolicy restartDelay stopPolicy start $ Just $ LocalName name

data ChainSyncConfig = ChainSyncConfig
  { client :: ChainSyncClientConfig
  , logger :: ChainSyncLoggerConfig
  , store  :: ChainSyncStoreConfig
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary ChainSyncConfig

newtype ChainSyncSubscribe = ChainSyncSubscribe ProcessId
  deriving (Generic, Typeable, Show, Eq)

instance Binary ChainSyncSubscribe

newtype ChainSyncUnsubscribe = ChainSyncUnsubscribe ProcessId
  deriving (Generic, Typeable, Show, Eq)

instance Binary ChainSyncUnsubscribe

chainSync :: ChainSyncConfig -> Process ()
chainSync ChainSyncConfig{..} = do
  self <- getSelfPid
  _ <- spawnLinkLocal $ Supervisor.run restartOne ParallelShutdown
    [ worker "chain-sync.store" Permanent Nothing StopImmediately $ RunClosure $ Store.process store
    , worker "chain-sync.client" Intrinsic (Just (seconds 5)) (StopTimeout (Delay $ minutes 1)) $ RunClosure $ Client.process client self
    , worker "chain-sync.logger" Permanent Nothing StopImmediately $ RunClosure $ Logger.process logger
    ]
  go Set.empty
  where
    go subscribers = receiveWait
      [ match \(msg :: ChainSyncMsg) -> do
          sendTo "chain-sync.logger" msg
          sendTo "chain-sync.store" msg
          traverse_ (flip send msg) subscribers
          go subscribers
      , match \(ChainSyncSubscribe subscriber) -> do
          go $ Set.insert subscriber subscribers
      , match \(ChainSyncUnsubscribe subscriber) -> do
          go $ Set.delete subscriber subscribers
      ]

remotable ['chainSync]

remoteTable :: RemoteTable -> RemoteTable
remoteTable = __remoteTable . Logger.__remoteTable . Client.__remoteTable . Store.__remoteTable

process :: ChainSyncConfig -> Closure (Process ())
process = $(mkClosure 'chainSync)
