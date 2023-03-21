{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}

module Main
  where

import Control.Concurrent.Component
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Either (fromRight)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.IO as TL
import Data.UUID.V4 (nextRandom)
import Language.Marlowe.Runtime.ChainSync.Api (BlockNo(..), ChainSyncQuery(..), RuntimeChainSeekClient)
import qualified Language.Marlowe.Runtime.Core.ScriptRegistry as ScriptRegistry
import Language.Marlowe.Runtime.Transaction (TransactionDependencies(..), transaction)
import qualified Language.Marlowe.Runtime.Transaction.Query as Query
import qualified Language.Marlowe.Runtime.Transaction.Submit as Submit
import Logging (RootSelector(..), defaultRootSelectorLogConfig, getRootSelectorConfig)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import Network.Protocol.Connection
  (SomeClientConnector, SomeConnectionSource(..), SomeConnector(SomeConnector), logConnectionSource, logConnector)
import Network.Protocol.Driver (TcpServerDependencies(..), runSomeConnector, tcpClient, tcpServer)
import Network.Protocol.Handshake.Client (handshakeClientConnector)
import Network.Protocol.Handshake.Server (handshakeConnectionSource)
import Network.Protocol.Job.Client (jobClientPeer)
import Network.Protocol.Job.Server (jobServerPeer)
import Network.Protocol.Query.Client (QueryClient, liftQuery, queryClientPeer)
import Network.Socket (HostName, PortNumber)
import Observe.Event.Backend (injectSelector, narrowEventBackend)
import Observe.Event.Component (LoggerDependencies(..), logger)
import Options.Applicative
  ( auto
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , infoOption
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
    , writeText = TL.hPutStr stderr
    , injectConfigWatcherSelector = injectSelector ConfigWatcher
    }
  serverSource <- tcpServer -< TcpServerDependencies host port jobServerPeer
  let
    chainSyncConnector :: SomeClientConnector RuntimeChainSeekClient IO
    chainSyncConnector = SomeConnector
      $ logConnector (narrowEventBackend (injectSelector ChainSeekClient) eventBackend)
      $ handshakeClientConnector
      $ tcpClient chainSeekHost chainSeekPort chainSeekClientPeer

    chainSyncQueryConnector :: SomeClientConnector (QueryClient ChainSyncQuery) IO
    chainSyncQueryConnector = SomeConnector
      $ logConnector (narrowEventBackend (injectSelector ChainSyncQueryClient) eventBackend)
      $ handshakeClientConnector
      $ tcpClient chainSeekHost chainSeekQueryPort queryClientPeer

    queryChainSync = fmap (fromRight $ error "failed to query chain sync server") . runSomeConnector chainSyncQueryConnector . liftQuery
  transaction -< TransactionDependencies
    { connectionSource = SomeConnectionSource
        $ logConnectionSource (narrowEventBackend (injectSelector Server) eventBackend)
        $ handshakeConnectionSource serverSource
    , mkSubmitJob = Submit.mkSubmitJob Submit.SubmitJobDependencies
        { chainSyncJobConnector = SomeConnector
            $ logConnector (narrowEventBackend (injectSelector ChainSyncJobClient) eventBackend)
            $ handshakeClientConnector
            $ tcpClient chainSeekHost chainSeekCommandPort jobClientPeer
        , ..
        }
    , loadMarloweContext = \eb version contractId -> do
        networkId <- queryChainSync GetNetworkId
        Query.loadMarloweContext ScriptRegistry.getScripts networkId chainSyncConnector chainSyncQueryConnector eb version contractId
    , loadWalletContext = Query.loadWalletContext $ queryChainSync . GetUTxOs
    , eventBackend = narrowEventBackend (injectSelector App) eventBackend
    , getCurrentScripts = ScriptRegistry.getCurrentScripts
    , httpPort = fromIntegral httpPort
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
  , httpPort :: PortNumber
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> printLogConfigOption <*> parser) infoMod
  where
    printLogConfigOption = infoOption
      (T.unpack $ decodeUtf8 $ encodePretty defaultRootSelectorLogConfig)
      (long "print-log-config" <> help "Print the default log configuration.")

    parser = Options
      <$> chainSeekPortParser
      <*> chainSeekQueryPortParser
      <*> chainSeekCommandPortParser
      <*> chainSeekHostParser
      <*> portParser
      <*> hostParser
      <*> logConfigFileParser
      <*> submitConfirmationBlocksParser
      <*> httpPortParser

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

    httpPortParser = option auto $ mconcat
      [ long "http-port"
      , metavar "PORT_NUMBER"
      , help "Port number to serve the http healthcheck API on"
      , value 8080
      , showDefault
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
