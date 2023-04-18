{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  where

import Control.Concurrent.Component
import Control.Concurrent.Component.Probes (ProbeServerDependencies(..), probeServer)
import Control.Concurrent.Component.UnliftIO (convertComponent)
import Control.Exception (bracket)
import Control.Monad ((<=<))
import Control.Monad.Base (MonadBase)
import Control.Monad.Event.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.With
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.GeneralAllocate
import Data.String (fromString)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.IO as TL
import Data.UUID.V4 (nextRandom)
import qualified Hasql.Pool as Pool
import Language.Marlowe.Protocol.HeaderSync.Server (marloweHeaderSyncServerPeer)
import Language.Marlowe.Protocol.Sync.Server (marloweSyncServerPeer)
import Language.Marlowe.Runtime.Sync (SyncDependencies(..), sync)
import Language.Marlowe.Runtime.Sync.Database (hoistDatabaseQueries, logDatabaseQueries)
import qualified Language.Marlowe.Runtime.Sync.Database.PostgreSQL as Postgres
import Logging (RootSelector(..), defaultRootSelectorLogConfig, getRootSelectorConfig)
import Network.Protocol.Connection (SomeConnectionSource(..), logConnectionSource)
import Network.Protocol.Driver (TcpServerDependencies(..), tcpServer)
import Network.Protocol.Handshake.Server (handshakeConnectionSource)
import Network.Socket (HostName, PortNumber)
import Observe.Event.Backend (EventBackend, hoistEventBackend, injectSelector)
import Observe.Event.Component (LoggerDependencies(..), withLogger)
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
import UnliftIO (MonadIO, MonadUnliftIO, liftIO, throwIO)

main :: IO ()
main = run =<< getOptions

run :: Options -> IO ()
run Options{..} = bracket (Pool.acquire 100 (Just 5000000) (fromString databaseUri)) Pool.release $
  runComponent_ $ withLogger loggerDependencies runAppM proc pool -> do
    marloweSyncSource <- tcpServer -< TcpServerDependencies
      { host
      , port = marloweSyncPort
      , toPeer = marloweSyncServerPeer
      }

    headerSyncSource <- tcpServer -< TcpServerDependencies
      { host
      , port = marloweHeaderSyncPort
      , toPeer = marloweHeaderSyncServerPeer
      }

    querySource <- tcpServer -< TcpServerDependencies
      { host
      , port = queryPort
      , toPeer = id
      }

    probes <- convertComponent sync -< SyncDependencies
      { databaseQueries = logDatabaseQueries $ hoistDatabaseQueries
            (either throwIO pure <=< liftIO . Pool.use pool)
            Postgres.databaseQueries
      , syncSource = SomeConnectionSource
          $ logConnectionSource (injectSelector MarloweSyncServer)
          $ handshakeConnectionSource marloweSyncSource
      , headerSyncSource = SomeConnectionSource
          $ logConnectionSource (injectSelector MarloweHeaderSyncServer)
          $ handshakeConnectionSource headerSyncSource
      , querySource = SomeConnectionSource
          $ logConnectionSource (injectSelector MarloweQueryServer)
          $ handshakeConnectionSource querySource
      }

    probeServer -< ProbeServerDependencies { port = fromIntegral httpPort, .. }
  where
    loggerDependencies = LoggerDependencies
      { configFilePath = logConfigFile
      , getSelectorConfig = getRootSelectorConfig
      , newRef = nextRandom
      , writeText = TL.hPutStr stderr
      , injectConfigWatcherSelector = injectSelector ConfigWatcher
      }

runAppM :: EventBackend IO r RootSelector -> AppM r a -> IO a
runAppM eventBackend = flip runReaderT (hoistEventBackend liftIO eventBackend) . unAppM

newtype AppM r a = AppM
  { unAppM :: ReaderT (EventBackend (AppM r) r RootSelector) IO a
  } deriving newtype (Functor, Applicative, Monad, MonadBase IO, MonadBaseControl IO, MonadIO, MonadUnliftIO, MonadFail)

instance MonadWith (AppM r) where
  type WithException (AppM r) = WithException IO
  stateThreadingGeneralWith
    :: forall a b releaseReturn
     . GeneralAllocate (AppM r) (WithException IO) releaseReturn b a
    -> (a -> AppM r b)
    -> AppM r (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = AppM . ReaderT $ \r -> do
    let
      allocA' :: (forall x. IO x -> IO x) -> IO (GeneralAllocated IO (WithException IO) releaseReturn b a)
      allocA' restore = do
        let
          restore' :: forall x. AppM r x -> AppM r x
          restore' mx = AppM . ReaderT $ restore . (runReaderT . unAppM) mx
        GeneralAllocated a releaseA <- (runReaderT . unAppM) (allocA restore') r
        let
          releaseA' relTy = (runReaderT . unAppM) (releaseA relTy) r
        pure $ GeneralAllocated a releaseA'
    stateThreadingGeneralWith (GeneralAllocate allocA') (flip (runReaderT . unAppM) r . go)

instance MonadEvent r RootSelector (AppM r) where
  askBackend = askBackendReaderT AppM id
  localBackend = localBackendReaderT AppM unAppM id

data Options = Options
  { databaseUri :: String
  , marloweSyncPort :: PortNumber
  , marloweHeaderSyncPort :: PortNumber
  , queryPort :: PortNumber
  , host :: HostName
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
      <$> databaseUriParser
      <*> marloweSyncPortParser
      <*> marloweHeaderSyncPortParser
      <*> queryPort
      <*> hostParser
      <*> logConfigFileParser
      <*> httpPortParser

    databaseUriParser = strOption $ mconcat
      [ long "database-uri"
      , short 'd'
      , metavar "DATABASE_URI"
      , help "URI of the database where the contract information is saved."
      ]

    marloweSyncPortParser = option auto $ mconcat
      [ long "sync-port"
      , value 3724
      , metavar "PORT_NUMBER"
      , help "The port number to run the sync protocol on."
      , showDefault
      ]

    marloweHeaderSyncPortParser = option auto $ mconcat
      [ long "header-sync-port"
      , value 3725
      , metavar "PORT_NUMBER"
      , help "The port number to run the header sync protocol on."
      , showDefault
      ]

    queryPort = option auto $ mconcat
      [ long "query-port"
      , value 3726
      , metavar "PORT_NUMBER"
      , help "The port number to run the query protocol on."
      , showDefault
      ]

    hostParser = strOption $ mconcat
      [ long "host"
      , short 'h'
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name to run the server on."
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

    infoMod = mconcat
      [ fullDesc
      , progDesc "Contract synchronization and query service for Marlowe Runtime"
      , header "marlowe-sync : a contract synchronization and query service for the Marlowe Runtime."
      ]
