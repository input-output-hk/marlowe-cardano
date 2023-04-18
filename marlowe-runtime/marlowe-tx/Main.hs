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
import Control.Monad.Base (MonadBase)
import Control.Monad.Event.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.With
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Either (fromRight)
import Data.GeneralAllocate
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
import Observe.Event (EventBackend)
import Observe.Event.Backend (hoistEventBackend, injectSelector)
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

run :: Options -> IO ()
run Options{..} = flip runComponent_ () $ withLogger loggerDependencies runAppM proc _ -> do
  serverSource <- tcpServer -< TcpServerDependencies host port jobServerPeer
  let
    chainSyncConnector :: SomeClientConnector RuntimeChainSeekClient (AppM r)
    chainSyncConnector = SomeConnector
      $ logConnector (injectSelector ChainSeekClient)
      $ handshakeClientConnector
      $ tcpClient chainSeekHost chainSeekPort chainSeekClientPeer

    chainSyncQueryConnector :: SomeClientConnector (QueryClient ChainSyncQuery) (AppM r)
    chainSyncQueryConnector = SomeConnector
      $ logConnector (injectSelector ChainSyncQueryClient)
      $ handshakeClientConnector
      $ tcpClient chainSeekHost chainSeekQueryPort queryClientPeer

    queryChainSync = fmap (fromRight $ error "failed to query chain sync server") . runSomeConnector chainSyncQueryConnector . liftQuery
  probes <- convertComponent transaction -< TransactionDependencies
    { connectionSource = SomeConnectionSource
        $ logConnectionSource (injectSelector Server)
        $ handshakeConnectionSource serverSource
    , mkSubmitJob = Submit.mkSubmitJob Submit.SubmitJobDependencies
        { chainSyncJobConnector = SomeConnector
            $ logConnector (injectSelector ChainSyncJobClient)
            $ handshakeClientConnector
            $ tcpClient chainSeekHost chainSeekCommandPort jobClientPeer
        , ..
        }
    , loadMarloweContext = \version contractId -> do
        networkId <- queryChainSync GetNetworkId
        Query.loadMarloweContext ScriptRegistry.getScripts networkId chainSyncConnector chainSyncQueryConnector version contractId
    , loadWalletContext = Query.loadWalletContext $ queryChainSync . GetUTxOs
    , getCurrentScripts = ScriptRegistry.getCurrentScripts
    , ..
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
