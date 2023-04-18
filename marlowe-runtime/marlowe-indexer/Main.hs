{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad.Base (MonadBase)
import Control.Monad.Event.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.With
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Either (fromRight)
import Data.GeneralAllocate
import qualified Data.Set.NonEmpty as NESet
import Data.String (fromString)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.IO as TL
import Data.UUID.V4 (nextRandom)
import Data.Void (Void)
import qualified Hasql.Pool as Pool
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery(..))
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..))
import qualified Language.Marlowe.Runtime.Core.ScriptRegistry as ScriptRegistry
import Language.Marlowe.Runtime.Indexer (MarloweIndexerDependencies(..), marloweIndexer)
import qualified Language.Marlowe.Runtime.Indexer.Database.PostgreSQL as PostgreSQL
import Logging (RootSelector(..), defaultRootSelectorLogConfig, getRootSelectorConfig)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import Network.Protocol.Connection (ClientConnector, SomeConnector(..), logConnector)
import Network.Protocol.Driver (runConnector, tcpClient)
import Network.Protocol.Handshake.Client (handshakeClientConnector)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Query.Client (QueryClient, liftQuery, queryClientPeer)
import Network.Protocol.Query.Types (Query)
import Network.Socket (AddrInfo(..), HostName, PortNumber, SocketType(..), defaultHints, withSocketsDo)
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
import UnliftIO (MonadUnliftIO)

main :: IO ()
main = run =<< getOptions

clientHints :: AddrInfo
clientHints = defaultHints { addrSocketType = Stream }

run :: Options -> IO ()
run Options{..} = withSocketsDo do
  scripts <- case ScriptRegistry.getScripts MarloweV1 of
    NESet.IsEmpty -> fail "No known marlowe scripts"
    NESet.IsNonEmpty scripts -> pure scripts
  securityParameter <- queryChainSync GetSecurityParameter
  bracket (Pool.acquire 100 (Just 5000000) (fromString databaseUri)) Pool.release
    $ runComponent_
    $ withLogger loggerDependencies runAppM proc pool -> do
        probes <- convertComponent marloweIndexer -< MarloweIndexerDependencies
          { chainSyncConnector = SomeConnector
              $ logConnector (injectSelector ChainSeekClient)
              $ handshakeClientConnector
              $ tcpClient chainSeekHost chainSeekPort chainSeekClientPeer
          , chainSyncQueryConnector = SomeConnector
              $ logConnector (injectSelector ChainQueryClient) chainSyncQueryConnector
          , databaseQueries = PostgreSQL.databaseQueries pool securityParameter
          , pollingInterval = 1
          , marloweScriptHashes = NESet.map ScriptRegistry.marloweScript scripts
          , payoutScriptHashes = NESet.map ScriptRegistry.payoutScript scripts
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

    chainSyncQueryConnector :: (MonadFail m, MonadBase IO m) => ClientConnector (Handshake (Query ChainSyncQuery)) (QueryClient ChainSyncQuery) m
    chainSyncQueryConnector = handshakeClientConnector $ tcpClient chainSeekHost chainSeekQueryPort queryClientPeer

    queryChainSync :: ChainSyncQuery Void e a -> IO a
    queryChainSync = fmap (fromRight $ error "failed to query chain sync server")
      . runConnector chainSyncQueryConnector
      . liftQuery

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
