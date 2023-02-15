module Main
  where

import Control.Arrow (arr, (<<<))
import Control.Concurrent.Component
import Control.Exception (bracket, bracketOnError, throwIO)
import qualified Data.Text.Lazy.IO as TL
import Data.UUID.V4 (nextRandom)
import Language.Marlowe.Protocol.HeaderSync.Codec (codecMarloweHeaderSync)
import Language.Marlowe.Protocol.HeaderSync.Server (marloweHeaderSyncServerPeer)
import Language.Marlowe.Runtime.ChainSync.Api (RuntimeChainSeekClient, WithGenesis(..), runtimeChainSeekCodec)
import qualified Language.Marlowe.Runtime.Core.ScriptRegistry as ScriptRegistry
import Language.Marlowe.Runtime.Discovery (DiscoveryDependencies(..), discovery)
import Logging (RootSelector(..), getRootSelectorConfig)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import Network.Protocol.Handshake.Client (runClientPeerOverSocketWithLoggingWithHandshake)
import Network.Protocol.Handshake.Server (acceptRunServerPeerOverSocketWithLoggingWithHandshake)
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
  queryAddr <- resolve queryPort
  syncAddr <- resolve syncPort
  bracket (openServer queryAddr) close \querySocket -> do
    bracket (openServer syncAddr) close \syncSocket -> do
      let
        pageSize = 1024 -- TODO move to config with a default
        discoveryDependencies eventBackend =
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
            acceptRunQueryServer = acceptRunServerPeerOverSocketWithLoggingWithHandshake
              (narrowEventBackend QueryServer eventBackend)
              throwIO
              querySocket
              codecQuery
              queryServerPeer
            acceptRunSyncServer = acceptRunServerPeerOverSocketWithLoggingWithHandshake
              (narrowEventBackend SyncServer eventBackend)
              throwIO
              syncSocket
              codecMarloweHeaderSync
              marloweHeaderSyncServerPeer
            getMarloweVersion = ScriptRegistry.getMarloweVersion
            getScripts = ScriptRegistry.getScripts
          in DiscoveryDependencies{..}
        appComponent = discovery <<< arr discoveryDependencies <<< logger
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

data Options = Options
  { chainSeekPort      :: PortNumber
  , chainSeekQueryPort :: PortNumber
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
      <*> queryPortParser
      <*> syncPortParser
      <*> chainSeekHostParser
      <*> hostParser
      <*> logConfigFileParser

    chainSeekPortParser = option auto $ mconcat
      [ long "chain-sync-port"
      , value 3715
      , metavar "PORT_NUMBER"
      , help "The port number of the chain sync server."
      , showDefault
      ]

    chainSeekQueryPortParser = option auto $ mconcat
      [ long "chain-sync-query-port"
      , value 3716
      , metavar "PORT_NUMBER"
      , help "The port number of the chain sync query server."
      , showDefault
      ]

    queryPortParser = option auto $ mconcat
      [ long "query-port"
      , value 3721
      , metavar "PORT_NUMBER"
      , help "The port number to run the query server on."
      , showDefault
      ]

    syncPortParser = option auto $ mconcat
      [ long "sync-port"
      , value 3722
      , metavar "PORT_NUMBER"
      , help "The port number to run the sync server on."
      , showDefault
      ]

    chainSeekHostParser = strOption $ mconcat
      [ long "chain-sync-host"
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name of the chain sync server."
      , showDefault
      ]

    hostParser = strOption $ mconcat
      [ long "host"
      , short 'h'
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name to run the discovery server on."
      , showDefault
      ]

    logConfigFileParser = optional $ strOption $ mconcat
      [ long "log-config-file"
      , metavar "FILE_PATH"
      , help "The logging configuration JSON file."
      ]

    infoMod = mconcat
      [ fullDesc
      , progDesc "Contract discovery service for Marlowe Runtime"
      , header "marlowe-discovery : a contract discovery service for the Marlowe Runtime."
      ]
