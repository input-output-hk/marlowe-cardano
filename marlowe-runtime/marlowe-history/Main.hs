{-# LANGUAGE GADTs #-}

module Main
  where

import Control.Arrow (arr, (<<<))
import Control.Concurrent.Component
import Control.Concurrent.STM (atomically)
import Control.Exception (bracket, bracketOnError, throwIO)
import Data.Either (fromRight)
import qualified Data.Text.Lazy.IO as TL
import Data.UUID.V4 (nextRandom)
import Data.Void (Void)
import Language.Marlowe.Protocol.Sync.Codec (codecMarloweSync)
import Language.Marlowe.Protocol.Sync.Server (marloweSyncServerPeer)
import Language.Marlowe.Runtime.ChainSync.Api
  (ChainSyncQuery(..), RuntimeChainSeekClient, WithGenesis(..), runtimeChainSeekCodec)
import Language.Marlowe.Runtime.History (HistoryDependencies(..), history)
import Language.Marlowe.Runtime.History.Api (historyJobCodec, historyQueryCodec)
import Language.Marlowe.Runtime.History.Store (hoistHistoryQueries)
import Language.Marlowe.Runtime.History.Store.Memory (mkHistoryQueriesInMemory)
import Logging (RootSelector(..), getRootSelectorConfig)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import Network.Protocol.Handshake.Client
  (runClientPeerOverSocketWithHandshake, runClientPeerOverSocketWithLoggingWithHandshake)
import Network.Protocol.Handshake.Server (acceptRunServerPeerOverSocketWithLoggingWithHandshake)
import Network.Protocol.Job.Server (jobServerPeer)
import Network.Protocol.Query.Client (liftQuery, queryClientPeer)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Protocol.Query.Server (queryServerPeer)
import Network.Socket
  ( AddrInfo(..)
  , AddrInfoFlag(..)
  , HostName
  , PortNumber
  , SocketOption(..)
  , SocketType(..)
  , bind
  , close
  , defaultHints
  , getAddrInfo
  , listen
  , openSocket
  , setCloseOnExecIfNeeded
  , setSocketOption
  , withFdSocket
  , withSocketsDo
  )
import Observe.Event.Backend (narrowEventBackend, newOnceFlagMVar)
import Observe.Event.Component (LoggerDependencies(..), logger)
import Options.Applicative
  ( auto
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , optional
  , progDesc
  , short
  , showDefault
  , strOption
  , value
  )
import System.IO (stderr)

main :: IO ()
main = run =<< getOptions

clientHints :: AddrInfo
clientHints = defaultHints { addrSocketType = Stream }

run :: Options -> IO ()
run Options{..} = withSocketsDo do
  jobAddr <- resolve commandPort
  queryAddr <- resolve queryPort
  syncAddr <- resolve syncPort
  bracket (openServer jobAddr) close \jobSocket ->
    bracket (openServer queryAddr) close \querySocket -> do
      bracket (openServer syncAddr) close \syncSocket -> do
        securityParameter <- queryChainSync GetSecurityParameter
        let followerPageSize = 1024 -- TODO move to config with a default
        historyQueries <- atomically $ hoistHistoryQueries atomically <$> mkHistoryQueriesInMemory
        let
          historyDependencies eventBackend =
            let
              connectToChainSeek :: forall a. RuntimeChainSeekClient IO a -> IO a
              connectToChainSeek client = do
                addr' <- head <$> getAddrInfo (Just clientHints) (Just chainSeekHost) (Just $ show chainSeekPort)
                runClientPeerOverSocketWithLoggingWithHandshake
                  (narrowEventBackend ChainSeekClient eventBackend)
                  throwIO
                  addr'
                  runtimeChainSeekCodec
                  (chainSeekClientPeer Genesis)
                  client
              acceptRunJobServer = acceptRunServerPeerOverSocketWithLoggingWithHandshake
                (narrowEventBackend JobServer eventBackend)
                throwIO
                jobSocket
                historyJobCodec
                jobServerPeer
              acceptRunQueryServer = acceptRunServerPeerOverSocketWithLoggingWithHandshake
                (narrowEventBackend QueryServer eventBackend)
                throwIO
                querySocket
                historyQueryCodec
                queryServerPeer
              acceptRunSyncServer = acceptRunServerPeerOverSocketWithLoggingWithHandshake
                (narrowEventBackend SyncServer eventBackend)
                throwIO
                syncSocket
                codecMarloweSync
                marloweSyncServerPeer
             in HistoryDependencies{..}
        let appComponent = history <<< arr historyDependencies <<< logger
        runComponent_ appComponent LoggerDependencies
          { configFilePath = logConfigFile
          , getSelectorConfig = getRootSelectorConfig
          , newRef = nextRandom
          , newOnceFlag = newOnceFlagMVar
          , writeText = TL.hPutStr stderr
          , injectConfigWatcherSelector = ConfigWatcher
          }
  where
    openServer addr = bracketOnError (openSocket addr) close \socket -> do
      setSocketOption socket ReuseAddr 1
      withFdSocket socket setCloseOnExecIfNeeded
      bind socket $ addrAddress addr
      listen socket 2048
      return socket

    resolve p = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) (Just host) (Just $ show p)

    queryChainSeek :: ChainSyncQuery Void e a -> IO (Either e a)
    queryChainSeek query = do
      addr <- head <$> getAddrInfo (Just clientHints) (Just chainSeekHost) (Just $ show chainSeekQueryPort)
      runClientPeerOverSocketWithHandshake throwIO addr codecQuery queryClientPeer $ liftQuery query

    queryChainSync :: ChainSyncQuery Void e a -> IO a
    queryChainSync = fmap (fromRight $ error "failed to query chain seek server") . queryChainSeek

data Options = Options
  { chainSeekPort      :: PortNumber
  , chainSeekQueryPort :: PortNumber
  , commandPort        :: PortNumber
  , queryPort          :: PortNumber
  , syncPort           :: PortNumber
  , chainSeekHost      :: HostName
  , host               :: HostName
  , logConfigFile      :: Maybe FilePath
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> parser) infoMod
  where
    parser = Options
      <$> chainSeekPortParser
      <*> chainSeekQueryPortParser
      <*> commandPortParser
      <*> queryPortParser
      <*> syncPortParser
      <*> chainSeekHostParser
      <*> hostParser
      <*> logConfigFileParser

    chainSeekPortParser = option auto $ mconcat
      [ long "chain-seek-port-number"
      , value 3715
      , metavar "PORT_NUMBER"
      , help "The port number of the chain seek server."
      , showDefault
      ]

    chainSeekQueryPortParser = option auto $ mconcat
      [ long "chain-seek-query-port-number"
      , value 3716
      , metavar "PORT_NUMBER"
      , help "The port number of the chain sync query server."
      , showDefault
      ]

    commandPortParser = option auto $ mconcat
      [ long "command-port"
      , value 3717
      , metavar "PORT_NUMBER"
      , help "The port number to run the job server on."
      , showDefault
      ]

    queryPortParser = option auto $ mconcat
      [ long "query-port"
      , value 3718
      , metavar "PORT_NUMBER"
      , help "The port number to run the query server on."
      , showDefault
      ]

    syncPortParser = option auto $ mconcat
      [ long "sync-port"
      , value 3719
      , metavar "PORT_NUMBER"
      , help "The port number to run the sync server on."
      , showDefault
      ]

    chainSeekHostParser = strOption $ mconcat
      [ long "chain-seek-host"
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name of the chain seek server."
      , showDefault
      ]

    hostParser = strOption $ mconcat
      [ long "host"
      , short 'h'
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name to run the history server on."
      , showDefault
      ]

    logConfigFileParser = optional $ strOption $ mconcat
      [ long "log-config-file"
      , metavar "FILE_PATH"
      , help "The logging configuration JSON file."
      ]

    infoMod = mconcat
      [ fullDesc
      , progDesc "Contract history service for Marlowe Runtime"
      , header "marlowe-history : a contract history service for the Marlowe Runtime."
      ]
