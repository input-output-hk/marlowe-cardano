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
import ChainSync.Client (ChainSyncClientConfig (..))
import ChainSync.Logger (ChainSyncLoggerConfig (..))
import ChainSync.Store (ChainSyncStoreConfig (..))
import Control.Distributed.Process (Process, ProcessInfo (ProcessInfo, infoRegisteredNames), RemoteTable, expect,
                                    getProcessInfo, kill, liftIO, link, matchChan, newChan, nsend, receiveChan,
                                    receiveWait, reregister, sendChan)
import Control.Distributed.Process.Extras (spawnLinkLocal)
import Control.Distributed.Process.Extras.SystemLog (LogLevel (Info), systemLog)
import Control.Distributed.Process.Extras.Time (Delay (..))
import Control.Distributed.Process.Internal.Primitives (SayMessage (..))
import Control.Distributed.Process.Node (initRemoteTable, newLocalNode, runProcess)
import Control.Distributed.Process.Supervisor (ChildSpec (..), ChildStart (..), ChildStopPolicy (..), ChildType (..),
                                               RegisteredName (..), RestartPolicy (..), ShutdownMode (..), restartOne)
import qualified Control.Distributed.Process.Supervisor as Supervisor
import Control.Monad (forever)
import Data.Binary (Binary)
import Data.Data (Typeable)
import Data.Functor (void)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import GHC.Generics (Generic)
import qualified History
import History.Digest (HistoryDigestConfig (HistoryDigestConfig))
import History.Logger (HistoryLoggerConfig (..))
import Network.Transport.TCP (TCPAddr (..), createTransport, defaultTCPParameters)
import System.IO (BufferMode (LineBuffering), IOMode (AppendMode), hClose, hPutStrLn, hSetBuffering, openFile)

supervisor :: String -> ChildStart -> ChildSpec
supervisor name start =
  ChildSpec name Supervisor Intrinsic Nothing (StopTimeout Infinity) start
    $ Just
    $ LocalName name

remoteTable :: RemoteTable
remoteTable = History.remoteTable . ChainSync.remoteTable $ initRemoteTable

data Config = Config
  { chainSync :: ChainSync.ChainSyncConfig
  , history   :: History.HistoryConfig
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary Config

app :: Config -> Process ()
app Config{..} = do
  (sendMsg, receiveMsg) <- newChan
  (sendSendRequest, receiveSendRequest) <- newChan
  (sendRequestProxy, receiveRequest) <- newChan
  reregister "logger" =<< spawnLinkLocal appLogger
  h <- liftIO $ openFile "./system.log" AppendMode
  liftIO $ hSetBuffering h LineBuffering
  syslog <- systemLog (liftIO . hPutStrLn h) (liftIO (hClose h)) Info pure
  kill syslog "killing syslog"
  appSupervisor <- Supervisor.start restartOne ParallelShutdown
    [ supervisor "chain-sync" $ RunClosure $ ChainSync.process $ ChainSync.ChainSyncDependencies chainSync sendMsg sendSendRequest sendRequestProxy
    , supervisor "history" $ RunClosure $ History.process $ History.HistoryDependencies history sendRequestProxy
    ]
  link appSupervisor
  let
    go sendRequest = receiveWait
      [ matchChan receiveMsg \msg -> do
          nsend "history" msg
          go sendRequest
      , matchChan receiveSendRequest go
      , matchChan receiveRequest \request -> do
          sendChan sendRequest request
          go sendRequest
      ]
  go =<< receiveChan receiveSendRequest

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
      , history = History.HistoryConfig
          { digest = HistoryDigestConfig
          , logger = HistoryLoggerConfig
            { syncLoggingFrequency = 4000
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
        name : _ -> "[" <> name <> "] "
        _        -> ""
    let timestamp = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) sayTime
    liftIO $ putStrLn $ label <> "[" <> timestamp <> "] [" <> show sayProcess <> "] " <> sayMessage
