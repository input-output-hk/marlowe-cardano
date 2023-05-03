{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Main
  where

import Control.Concurrent.Component
import Control.Concurrent.Component.Probes (ProbeServerDependencies(..), probeServer)
import Control.Monad.Event.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.With
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (ByteString)
import Data.GeneralAllocate
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.IO as TL
import Data.UUID.V4 (nextRandom)
import Language.Marlowe.Protocol.Server (marloweRuntimeServerPeer)
import Language.Marlowe.Runtime.CLI.Option (optParserWithEnvDefault)
import qualified Language.Marlowe.Runtime.CLI.Option as O
import Language.Marlowe.Runtime.Proxy
import Logging (RootSelector(..), defaultRootSelectorLogConfig, getRootSelectorConfig)
import Network.Channel (hoistChannel, socketAsChannel)
import Network.Protocol.Codec (BinaryMessage)
import Network.Protocol.Connection (SomeConnectionSource(..), logConnectionSource)
import Network.Protocol.Driver (TcpServerDependencies(..), mkDriver, tcpServer)
import Network.Protocol.Handshake.Server (handshakeConnectionSource)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Socket
  ( AddrInfo(addrAddress, addrSocketType)
  , HostName
  , PortNumber
  , SocketType(Stream)
  , close
  , connect
  , defaultHints
  , getAddrInfo
  , openSocket
  )
import Network.TypedProtocol (Driver)
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
import UnliftIO.Resource (MonadResource, allocate)

main :: IO ()
main = run =<< getOptions

run :: Options -> IO ()
run Options{..} = flip runComponent_ () $ withLogger loggerDependencies runAppM proc _ -> do
  connectionSource <- tcpServer -< TcpServerDependencies
    { toPeer = marloweRuntimeServerPeer
    , ..
    }

  probes <- proxy -< ProxyDependencies
    { getMarloweSyncDriver = driverFactory syncHost marloweSyncPort
    , getMarloweHeaderSyncDriver = driverFactory syncHost marloweHeaderSyncPort
    , getMarloweQueryDriver = driverFactory syncHost marloweQueryPort
    , getTxJobDriver = driverFactory txHost txPort
    , connectionSource = SomeConnectionSource
        $ logConnectionSource (injectSelector MarloweRuntimeServer)
        $ handshakeConnectionSource connectionSource
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

driverFactory
  :: (BinaryMessage ps, MonadResource m)
  => HostName
  -> PortNumber
  -> m (Driver (Handshake ps) (Maybe ByteString) m)
driverFactory host port = do
  addr <- liftIO $ head <$> getAddrInfo
    (Just defaultHints { addrSocketType = Stream })
    (Just host)
    (Just $ show port)
  (_, socket) <- allocate (openSocket addr) close
  liftIO $ connect socket $ addrAddress addr
  pure $ mkDriver $ hoistChannel liftIO $ socketAsChannel socket

runAppM :: EventBackend IO r RootSelector -> AppM r a -> IO a
runAppM eventBackend = flip runReaderT (hoistEventBackend liftIO eventBackend) . unAppM

newtype AppM r a = AppM
  { unAppM :: ReaderT (EventBackend (AppM r) r RootSelector) IO a
  } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadFail)

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
  { host :: HostName
  , port :: PortNumber
  , syncHost :: HostName
  , marloweSyncPort :: PortNumber
  , marloweHeaderSyncPort :: PortNumber
  , marloweQueryPort :: PortNumber
  , txHost :: HostName
  , txPort :: PortNumber
  , logConfigFile :: Maybe FilePath
  , httpPort :: PortNumber
  }

getOptions :: IO Options
getOptions = do
  syncHostParser <- optParserWithEnvDefault O.syncHost
  marloweSyncPortParser <- optParserWithEnvDefault O.syncSyncPort
  marloweHeaderSyncPortParser <- optParserWithEnvDefault O.syncHeaderPort
  marloweQueryPortParser <- optParserWithEnvDefault O.syncQueryPort
  txHostParser <- optParserWithEnvDefault O.txHost
  txPortParser <- optParserWithEnvDefault O.txCommandPort
  execParser $ info
    ( helper <*> printLogConfigOption <*>
      ( Options
          <$> hostParser
          <*> portParser
          <*> syncHostParser
          <*> marloweSyncPortParser
          <*> marloweHeaderSyncPortParser
          <*> marloweQueryPortParser
          <*> txHostParser
          <*> txPortParser
          <*> logConfigFileParser
          <*> httpPortParser
      )
    )
    infoMod
  where
    printLogConfigOption = infoOption
      (T.unpack $ decodeUtf8 $ encodePretty defaultRootSelectorLogConfig)
      (long "print-log-config" <> help "Print the default log configuration.")

    hostParser = strOption $ mconcat
      [ long "host"
      , short 'h'
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name to run the server on."
      , showDefault
      ]

    portParser = option auto $ mconcat
      [ long "port"
      , short 'p'
      , value 3700
      , metavar "PORT_NUMBER"
      , help "The port number to run the server on."
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
      , progDesc "API proxy service for Marlowe Runtime"
      , header "marlowe-proxy : an API proxy service for the Marlowe Runtime."
      ]
