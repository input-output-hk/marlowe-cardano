{-# LANGUAGE GADTs #-}

module Main
  where

import Control.Arrow (arr, (<<<))
import Control.Concurrent.Component
import Control.Exception (throwIO)
import Control.Monad ((<=<))
import Data.Either (fromRight)
import qualified Data.Set.NonEmpty as NESet
import Data.String (fromString)
import qualified Data.Text.Lazy.IO as TL
import Data.Time (secondsToNominalDiffTime)
import Data.UUID.V4 (nextRandom)
import Data.Void (Void)
import Hasql.Pool
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery(..), WithGenesis(..), runtimeChainSeekCodec)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..))
import qualified Language.Marlowe.Runtime.Core.ScriptRegistry as ScriptRegistry
import Language.Marlowe.Runtime.Indexer (MarloweIndexerDependencies(..), marloweIndexer)
import Language.Marlowe.Runtime.Indexer.Database (hoistDatabaseQueries)
import qualified Language.Marlowe.Runtime.Indexer.Database.PostgreSQL as PostgreSQL
import Logging (RootSelector(..), getRootSelectorConfig)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import Network.Protocol.Handshake.Client
  (runClientPeerOverSocketWithHandshake, runClientPeerOverSocketWithLoggingWithHandshake)
import Network.Protocol.Query.Client (liftQuery, queryClientPeer)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Socket (AddrInfo(..), HostName, PortNumber, SocketType(..), defaultHints, getAddrInfo, withSocketsDo)
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
  pool <- Pool.acquire (100, secondsToNominalDiffTime 5, fromString databaseUri)
  scripts <- case ScriptRegistry.getScripts MarloweV1 of
    NESet.IsEmpty -> fail "No known marlowe scripts"
    NESet.IsNonEmpty scripts -> pure scripts
  securityParameter <- queryChainSync GetSecurityParameter
  let
    indexerDependencies eventBackend = MarloweIndexerDependencies
      { runChainSeekClient = \client -> do
          addr' <- head <$> getAddrInfo (Just clientHints) (Just chainSeekHost) (Just $ show chainSeekPort)
          runClientPeerOverSocketWithLoggingWithHandshake
            (narrowEventBackend ChainSeekClient eventBackend)
            throwIO
            addr'
            runtimeChainSeekCodec
            (chainSeekClientPeer Genesis)
            client
      , runChainSyncQueryClient = \client -> do
          addr' <- head <$> getAddrInfo (Just clientHints) (Just chainSeekHost) (Just $ show chainSeekQueryPort)
          runClientPeerOverSocketWithLoggingWithHandshake
            (narrowEventBackend ChainQueryClient eventBackend)
            throwIO
            addr'
            codecQuery
            queryClientPeer
            client
      , databaseQueries = hoistDatabaseQueries
          (either throwUsageError pure <=< Pool.use pool)
          (PostgreSQL.databaseQueries securityParameter)
      , eventBackend = narrowEventBackend App eventBackend
      , pollingInterval = 1
      , marloweScriptHashes = NESet.map ScriptRegistry.marloweScript scripts
      , payoutScriptHashes = NESet.map ScriptRegistry.payoutScript scripts
      }
  let appComponent = marloweIndexer <<< arr indexerDependencies <<< logger
  runComponent_ appComponent LoggerDependencies
    { configFilePath = logConfigFile
    , getSelectorConfig = getRootSelectorConfig
    , newRef = nextRandom
    , newOnceFlag = newOnceFlagMVar
    , writeText = TL.hPutStr stderr
    , injectConfigWatcherSelector = ConfigWatcher
    }
  where
    throwUsageError (ConnectionError err)                       = error $ show err
    throwUsageError (SessionError (Session.QueryError _ _ err)) = error $ show err

    queryChainSync :: ChainSyncQuery Void e a -> IO a
    queryChainSync query = fmap (fromRight $ error "failed to query chain sync server") do
      addr <- head <$> getAddrInfo (Just clientHints) (Just chainSeekHost) (Just $ show chainSeekQueryPort)
      runClientPeerOverSocketWithHandshake throwIO addr codecQuery queryClientPeer $ liftQuery query

data Options = Options
  { chainSeekPort :: PortNumber
  , chainSeekQueryPort :: PortNumber
  , chainSeekHost :: HostName
  , databaseUri :: String
  , logConfigFile :: Maybe FilePath
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> parser) infoMod
  where
    parser = Options
      <$> chainSeekPortParser
      <*> chainSeekQueryPortParser
      <*> chainSeekHostParser
      <*> databaseUriParser
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

    infoMod = mconcat
      [ fullDesc
      , progDesc "Contract indexing service for Marlowe Runtime"
      , header "marlowe-indexer : a contract indexing service for the Marlowe Runtime."
      ]
