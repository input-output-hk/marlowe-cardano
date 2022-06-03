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
import ChainSync.Database (ChainSyncQueryChan)
import ChainSync.Logger (ChainSyncLoggerConfig (..))
import ChainSync.Store (ChainStoreQuery)
import Control.Distributed.Process (Process, ProcessInfo (ProcessInfo, infoRegisteredNames), RemoteTable, SendPort,
                                    expect, getProcessInfo, kill, liftIO, link, matchChan, newChan, nsend, receiveChan,
                                    receiveWait, reregister, say, sendChan, spawnLocal, whereis)
import Control.Distributed.Process.Extras (spawnLinkLocal)
import Control.Distributed.Process.Extras.SystemLog (LogLevel (..), systemLog)
import Control.Distributed.Process.Extras.Time (Delay (..), seconds)
import Control.Distributed.Process.Extras.Timer (periodically)
import Control.Distributed.Process.Internal.Primitives (SayMessage (..))
import Control.Distributed.Process.Node (initRemoteTable, newLocalNode, runProcess)
import Control.Distributed.Process.Supervisor (ChildSpec (..), ChildStart (..), ChildStopPolicy (..), ChildType (..),
                                               RegisteredName (..), RestartPolicy (..), ShutdownMode (..), restartRight)
import qualified Control.Distributed.Process.Supervisor as Supervisor
import Control.Monad (forever)
import Data.Binary (Binary)
import Data.Data (Typeable)
import Data.Foldable (for_)
import Data.Functor (void)
import qualified Data.IntMap as IntMap
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import GHC.Generics (Generic)
import qualified History
import History.Database (HistoryQueryChan)
import History.Digest (HistoryDigestConfig (HistoryDigestConfig))
import History.Logger (HistoryLoggerConfig (..))
import History.Store (HistoryStoreChan)
import Network.Transport.TCP (TCPAddr (..), createTransport, defaultTCPParameters)
import System.Directory.Internal.Prelude (traverse_)
import System.IO (BufferMode (LineBuffering), IOMode (AppendMode), hClose, hPutStrLn, hSetBuffering, openFile)
import System.Random (randomRIO)

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

data State = State
  { chainDbChan      :: ChainSyncQueryChan
  , chainStoreChan   :: SendPort ChainStoreQuery
  , historyDbChan    :: HistoryQueryChan
  , historyStoreChan :: HistoryStoreChan
  }

killable :: IntMap.IntMap [Char]
killable = IntMap.fromDistinctAscList $ zip [0..]
  [ "chain-sync.logger"
  , "chain-sync.store"
  , "chain-sync.db"
  , "chain-sync.client"
  , "history.logger"
  , "history.store"
  , "history.database"
  , "history.digest"
  ]

chaosMonkey :: Process ()
chaosMonkey = void $ periodically (seconds 1) do
  index <- liftIO $ randomRIO (0, 100)
  for_ (IntMap.lookup index killable) \toKill -> do
    say $ "chaos monkey killing process " <> toKill
    mProcId <- whereis toKill
    traverse_ (flip kill "killed by chaos monkey") mProcId

app :: Config -> Process ()
app Config{..} = do
  (sendMsg, receiveMsg) <- newChan
  (initChainDbChan, receiveInitChainDbChan) <- newChan
  (initChainStoreChan, receiveInitChainStoreChan) <- newChan
  (chainDbChanProxy, receiveChainDbChan) <- newChan
  (chainStoreChanProxy, receiveChainStoreChan) <- newChan
  (initHistoryDbChan, receiveInitHistoryDbChan) <- newChan
  (initHistoryStoreChan, receiveInitHistoryStoreChan) <- newChan
  (historyDbChanProxy, receiveHistoryDbChan) <- newChan
  (historyStoreChanProxy, receiveHistoryStoreChan) <- newChan
  reregister "logger" =<< spawnLinkLocal appLogger
  h <- liftIO $ openFile "./system.log" AppendMode
  liftIO $ hSetBuffering h LineBuffering
  syslog <- systemLog (liftIO . hPutStrLn h) (liftIO (hClose h)) Debug pure
  kill syslog "killing syslog"
  chaosMonkeyPid <- spawnLocal chaosMonkey
  kill chaosMonkeyPid "killing chaos monkey"
  appSupervisor <- spawnLinkLocal $ Supervisor.run restartRight ParallelShutdown
    [ supervisor "chain-sync" $ RunClosure $ ChainSync.process $ ChainSync.ChainSyncDependencies chainSync sendMsg initChainDbChan initChainStoreChan chainDbChanProxy
    , supervisor "history" $ RunClosure $ History.process $ History.HistoryDependencies history chainDbChanProxy chainStoreChanProxy historyStoreChanProxy initHistoryDbChan initHistoryStoreChan historyDbChanProxy
    ]
  link appSupervisor
  let
    go state@State{..} = receiveWait
      [ matchChan receiveMsg \msg -> do
          nsend "history" msg
          go state
      , matchChan receiveInitChainDbChan \newChainDbChan -> go state { chainDbChan = newChainDbChan }
      , matchChan receiveInitChainStoreChan \newChainStoreChan -> go state { chainStoreChan = newChainStoreChan }
      , matchChan receiveInitHistoryDbChan \newHistoryDbChan -> go state { historyDbChan = newHistoryDbChan }
      , matchChan receiveInitHistoryStoreChan \newHistoryStoreChan -> go state { historyStoreChan = newHistoryStoreChan }
      , matchChan receiveChainDbChan \query -> do
          sendChan chainDbChan query
          go state
      , matchChan receiveChainStoreChan \query -> do
          sendChan chainStoreChan query
          go state
      , matchChan receiveHistoryDbChan \query -> do
          sendChan historyDbChan query
          go state
      , matchChan receiveHistoryStoreChan \query -> do
          sendChan historyStoreChan query
          go state
      ]
  chainDbChan <- receiveChan receiveInitChainDbChan
  chainStoreChan <- receiveChan receiveInitChainStoreChan
  historyDbChan <- receiveChan receiveInitHistoryDbChan
  historyStoreChan <- receiveChan receiveInitHistoryStoreChan
  go $ State {..}

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
    liftIO $ putStrLn $ "[" <> timestamp <> "] [" <> show sayProcess <> "] " <> label <> sayMessage
