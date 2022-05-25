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
module Main where

import qualified ChainSync
import ChainSync.Client (ChainSyncClientConfig (..), ChainSyncMsg (..))
import ChainSync.Logger (ChainSyncLoggerConfig (..))
import ChainSync.Store (ChainSyncStoreConfig (..))
import Control.Distributed.Process (Process, ProcessInfo (ProcessInfo, infoRegisteredNames), RemoteTable, expect,
                                    getProcessInfo, getSelfPid, liftIO, link, match, nsend, receiveWait, reregister,
                                    send)
import Control.Distributed.Process.Extras (spawnLinkLocal)
import Control.Distributed.Process.Extras.Time (Delay (..))
import Control.Distributed.Process.Internal.Primitives (SayMessage (..))
import Control.Distributed.Process.Node (initRemoteTable, newLocalNode, runProcess)
import Control.Distributed.Process.Supervisor (ChildRef (..), ChildSpec (..), ChildStart (..), ChildStopPolicy (..),
                                               ChildType (..), RegisteredName (..), RestartPolicy (..),
                                               ShutdownMode (..), lookupChild, restartOne)
import qualified Control.Distributed.Process.Supervisor as Supervisor
import Control.Monad (forever)
import Data.Binary (Binary)
import Data.Data (Typeable)
import Data.Functor (void)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import GHC.Generics (Generic)
import Network.Transport.TCP (TCPAddr (..), createTransport, defaultTCPParameters)

supervisor :: String -> ChildStart -> ChildSpec
supervisor name start =
  ChildSpec name Supervisor Intrinsic Nothing (StopTimeout Infinity) start
    $ Just
    $ LocalName name

remoteTable :: RemoteTable
remoteTable = ChainSync.remoteTable initRemoteTable

newtype Config = Config
  { chainSync :: ChainSync.ChainSyncConfig
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary Config

app :: Config -> Process ()
app Config{..} = do
  self <- getSelfPid
  appSupervisor <- Supervisor.start restartOne ParallelShutdown
    [ supervisor "chain-sync" $ RunClosure $ ChainSync.process chainSync
    ]
  Just (ChildRunning chainSyncPid, _) <- lookupChild appSupervisor "chain-sync"
  send chainSyncPid $ ChainSync.ChainSyncSubscribe self
  link appSupervisor
  reregister "logger" =<< spawnLinkLocal appLogger
  forever $ receiveWait
    [ match \(msg :: ChainSyncMsg) -> do
        nsend "history.digest" msg
    ]

main :: IO ()
main = do
  Right transport <- createTransport Unaddressable defaultTCPParameters
  node <- newLocalNode transport remoteTable
  let
    config = Config
      { chainSync = ChainSync.ChainSyncConfig
        { logger = ChainSyncLoggerConfig
            { syncLoggingFrequency = 4000
            }
        , client = ChainSyncClientConfig
            { epochSlots = 21600
            , networkMagic = Just 1566
            , nodeSocket = "/var/lib/containers/storage/volumes/marlowe-dashboard-client_cardano-ipc/_data/node.socket"
            }
        , store = ChainSyncStoreConfig
            { directory = "."
            , batchSize = 4000
            }
        }
      }
  void $ runProcess node $ app config

appLogger :: Process ()
appLogger = forever do
  SayMessage{..} <- expect
  getProcessInfo sayProcess >>= mapM_ \ProcessInfo{..} -> do
    let
      label = case infoRegisteredNames of
        name : _ -> name
        _        -> show sayProcess
    let timestamp = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) sayTime
    liftIO $ putStrLn $ "[" <> label <> "] [" <> timestamp <> "] " <> sayMessage
