{-# LANGUAGE GADTs #-}

module Main
  where

import Control.Arrow (arr, (<<<))
import Control.Concurrent.Component
import Control.Monad ((<=<))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Either (fromRight)
import qualified Data.Set.NonEmpty as NESet
import Data.String (fromString)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.IO as TL
import Data.UUID.V4 (nextRandom)
import Data.Void (Void)
import Hasql.Pool
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery(..))
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..))
import qualified Language.Marlowe.Runtime.Core.ScriptRegistry as ScriptRegistry
import Language.Marlowe.Runtime.Indexer (MarloweIndexerDependencies(..), marloweIndexer)
import Language.Marlowe.Runtime.Indexer.Database (hoistDatabaseQueries)
import qualified Language.Marlowe.Runtime.Indexer.Database.PostgreSQL as PostgreSQL
import Logging (RootSelector(..), defaultRootSelectorLogConfig, getRootSelectorConfig)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import Network.Protocol.Connection (SomeConnector(..), logConnector)
import Network.Protocol.Driver (runConnector, tcpClient)
import Network.Protocol.Handshake.Client (handshakeClientConnector)
import Network.Protocol.Query.Client (liftQuery, queryClientPeer)
import Network.Socket (AddrInfo(..), HostName, PortNumber, SocketType(..), defaultHints, withSocketsDo)
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

clientHints :: AddrInfo
clientHints = defaultHints { addrSocketType = Stream }

run :: Options -> IO ()
run Options{..} = withSocketsDo do
  pool <- Pool.acquire 100 (Just 5000000) (fromString databaseUri)
  scripts <- case ScriptRegistry.getScripts MarloweV1 of
    NESet.IsEmpty -> fail "No known marlowe scripts"
    NESet.IsNonEmpty scripts -> pure scripts
  securityParameter <- queryChainSync GetSecurityParameter
  let
    indexerDependencies eventBackend = MarloweIndexerDependencies
      { chainSyncConnector = SomeConnector
          $ logConnector (narrowEventBackend (injectSelector ChainSeekClient) eventBackend)
          $ handshakeClientConnector
          $ tcpClient chainSeekHost chainSeekPort chainSeekClientPeer
      , chainSyncQueryConnector = SomeConnector
          $ logConnector (narrowEventBackend (injectSelector ChainQueryClient) eventBackend) chainSyncQueryConnector
      , databaseQueries = hoistDatabaseQueries
          (either throwUsageError pure <=< Pool.use pool)
          (PostgreSQL.databaseQueries securityParameter)
      , eventBackend = narrowEventBackend (injectSelector App) eventBackend
      , pollingInterval = 1
      , marloweScriptHashes = NESet.map ScriptRegistry.marloweScript scripts
      , payoutScriptHashes = NESet.map ScriptRegistry.payoutScript scripts
      , httpPort = fromIntegral httpPort
      }
  let appComponent = marloweIndexer <<< arr indexerDependencies <<< logger
  runComponent_ appComponent LoggerDependencies
    { configFilePath = logConfigFile
    , getSelectorConfig = getRootSelectorConfig
    , newRef = nextRandom
    , writeText = TL.hPutStr stderr
    , injectConfigWatcherSelector = injectSelector ConfigWatcher
    }
  where
    throwUsageError (ConnectionUsageError err)                       = error $ show err
    throwUsageError (SessionUsageError (Session.QueryError _ _ err)) = error $ show err
    throwUsageError AcquisitionTimeoutUsageError                     = error "hasql-timeout"


    chainSyncQueryConnector = handshakeClientConnector $ tcpClient chainSeekHost chainSeekQueryPort queryClientPeer

    queryChainSync :: ChainSyncQuery Void e a -> IO a
    queryChainSync = fmap (fromRight $ error "failed to query chain sync server")
      . runConnector chainSyncQueryConnector
      . liftQuery

data Options = Options
  { chainSeekPort :: PortNumber
  , chainSeekQueryPort :: PortNumber
  , chainSeekHost :: HostName
  , databaseUri :: String
  , logConfigFile :: Maybe FilePath
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
      <*> chainSeekHostParser
      <*> databaseUriParser
      <*> logConfigFileParser
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

    chainSeekHostParser = strOption $ mconcat
      [ long "chain-sync-host"
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name of the chain sync server."
      , showDefault
      ]

    databaseUriParser = strOption $ mconcat
      [ long "database-uri"
      , short 'd'
      , metavar "DATABASE_URI"
      , help "URI of the database where the contract information is saved."
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

    infoMod = mconcat
      [ fullDesc
      , progDesc "Contract indexing service for Marlowe Runtime"
      , header "marlowe-indexer : a contract indexing service for the Marlowe Runtime."
      ]
