{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}

module Main
  where

import Control.Concurrent.Component
import Data.Either (fromRight)
import qualified Data.Text.Lazy.IO as TL
import Data.UUID.V4 (nextRandom)
import Language.Marlowe.Runtime.ChainSync.Api
  (BlockNo(..), ChainSyncQuery(..), RuntimeChainSeekClient, WithGenesis(..), runtimeChainSeekCodec)
import qualified Language.Marlowe.Runtime.Core.ScriptRegistry as ScriptRegistry
import Language.Marlowe.Runtime.Transaction (TransactionDependencies(..), transaction)
import qualified Language.Marlowe.Runtime.Transaction.Query as Query
import qualified Language.Marlowe.Runtime.Transaction.Submit as Submit
import Logging (RootSelector(..), getRootSelectorConfig)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import Network.Protocol.Driver
  (RunClient, TcpServerDependencies(TcpServerDependencies), awaitConnection, logConnectionSource, tcpServer)
import Network.Protocol.Handshake.Client (runClientPeerOverSocketWithLoggingWithHandshake)
import Network.Protocol.Handshake.Server (handshakeConnectionSource)
import Network.Protocol.Job.Client (jobClientPeer)
import Network.Protocol.Job.Codec (codecJob)
import Network.Protocol.Job.Server (jobServerPeer)
import Network.Protocol.Query.Client (QueryClient, liftQuery, queryClientPeer)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Socket (HostName, PortNumber)
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

run :: Options -> IO ()
run = runComponent_ proc Options{..} -> do
  eventBackend <- logger -< LoggerDependencies
    { configFilePath = logConfigFile
    , getSelectorConfig = getRootSelectorConfig
    , newRef = nextRandom
    , newOnceFlag = newOnceFlagMVar
    , writeText = TL.hPutStr stderr
    , injectConfigWatcherSelector = ConfigWatcher
    }
  serverSource <- tcpServer -< TcpServerDependencies host port codecJob jobServerPeer
  let
    connectToChainSeek :: RunClient IO RuntimeChainSeekClient
    connectToChainSeek = runClientPeerOverSocketWithLoggingWithHandshake
      (narrowEventBackend ChainSeekClient eventBackend)
      chainSeekHost
      chainSeekPort
      runtimeChainSeekCodec
      (chainSeekClientPeer Genesis)

    runChainSyncQueryClient :: RunClient IO (QueryClient ChainSyncQuery)
    runChainSyncQueryClient = runClientPeerOverSocketWithLoggingWithHandshake
      (narrowEventBackend ChainSyncQueryClient eventBackend)
      chainSeekHost
      chainSeekQueryPort
      codecQuery
      queryClientPeer

    queryChainSync = fmap (fromRight $ error "failed to query chain sync server") . runChainSyncQueryClient . liftQuery
  transaction -< TransactionDependencies
    { acceptRunTransactionServer = awaitConnection
        $ logConnectionSource (narrowEventBackend Server eventBackend)
        $ handshakeConnectionSource serverSource
    , mkSubmitJob = Submit.mkSubmitJob Submit.SubmitJobDependencies
        { runChainSyncJobClient = runClientPeerOverSocketWithLoggingWithHandshake
            (narrowEventBackend ChainSyncJobClient eventBackend)
            chainSeekHost
            chainSeekCommandPort
            codecJob
            jobClientPeer
        , ..
        }
    , loadMarloweContext = \eb version contractId -> do
        networkId <- queryChainSync GetNetworkId
        Query.loadMarloweContext ScriptRegistry.getScripts networkId connectToChainSeek runChainSyncQueryClient eb version contractId
    , loadWalletContext = Query.loadWalletContext $ queryChainSync . GetUTxOs
    , eventBackend = narrowEventBackend App eventBackend
    , getCurrentScripts = ScriptRegistry.getCurrentScripts
    , ..
    }

data Options = Options
  { chainSeekPort :: PortNumber
  , chainSeekQueryPort :: PortNumber
  , chainSeekCommandPort :: PortNumber
  , chainSeekHost :: HostName
  , port :: PortNumber
  , host :: HostName
  , logConfigFile :: Maybe FilePath
  , submitConfirmationBlocks :: BlockNo
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> parser) infoMod
  where
    parser = Options
      <$> chainSeekPortParser
      <*> chainSeekQueryPortParser
      <*> chainSeekCommandPortParser
      <*> chainSeekHostParser
      <*> portParser
      <*> hostParser
      <*> logConfigFileParser
      <*> submitConfirmationBlocksParser

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

    chainSeekCommandPortParser = option auto $ mconcat
      [ long "chain-sync-command-port"
      , value 3720
      , metavar "PORT_NUMBER"
      , help "The port number of the chain sync job server."
      , showDefault
      ]

    portParser = option auto $ mconcat
      [ long "command-port"
      , value 3723
      , metavar "PORT_NUMBER"
      , help "The port number to run the job server on."
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
      , help "The host name to run the tx server on."
      , showDefault
      ]

    logConfigFileParser = optional $ strOption $ mconcat
      [ long "log-config-file"
      , metavar "FILE_PATH"
      , help "The logging configuration JSON file."
      ]

    submitConfirmationBlocksParser = option (BlockNo <$> auto) $ mconcat
      [ long "submit-confirmation-blocks"
      , value 0
      , metavar "INTEGER"
      , help "The number of blocks after a transaction has been confirmed to wait before displaying the block in which was confirmed."
      , showDefault
      ]

    infoMod = mconcat
      [ fullDesc
      , progDesc "Marlowe runtime transaction creation server"
      , header "marlowe-tx : the transaction creation server of the Marlowe Runtime"
      ]
