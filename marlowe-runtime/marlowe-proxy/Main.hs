{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
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
import Control.Exception (bracket)
import Control.Monad.Event.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.With
import Data.GeneralAllocate
import qualified Data.Text as T
import Data.Version (showVersion)
import Language.Marlowe.Protocol.Server (marloweRuntimeServerPeer)
import Language.Marlowe.Runtime.CLI.Option (optParserWithEnvDefault)
import qualified Language.Marlowe.Runtime.CLI.Option as O
import Language.Marlowe.Runtime.Proxy
import Logging (RootSelector(..), renderRootSelectorOTel)
import Network.Channel.Typed
import Network.Protocol.Connection (SomeConnectionSource(..), SomeConnectionSourceTraced(..))
import Network.Protocol.Driver (TcpServerDependencies(..), tcpServer)
import Network.Protocol.Driver.Trace (tcpServerTraced)
import Network.Protocol.Handshake.Server (handshakeConnectionSource, handshakeConnectionSourceTraced)
import Network.Socket (HostName, PortNumber)
import Observe.Event.Backend (EventBackend, hoistEventBackend, injectSelector)
import Observe.Event.Render.OpenTelemetry (tracerEventBackend)
import OpenTelemetry.Trace
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
  , progDesc
  , short
  , showDefault
  , strOption
  , value
  )
import Paths_marlowe_runtime
import UnliftIO (MonadUnliftIO)

main :: IO ()
main = do
  options <- getOptions
  withTracer \tracer ->
    runAppM (tracerEventBackend tracer renderRootSelectorOTel) $ run options
  where
    withTracer f = bracket
      initializeGlobalTracerProvider
      shutdownTracerProvider
      \provider -> f $ makeTracer provider instrumentationLibrary tracerOptions

    instrumentationLibrary = InstrumentationLibrary
      { libraryName = "marlowe-proxy"
      , libraryVersion = T.pack $ showVersion version
      }

run :: Options -> AppM Span ()
run = runComponent_ proc Options{..} -> do
  connectionSource <- tcpServer -< TcpServerDependencies
    { toPeer = marloweRuntimeServerPeer
    , ..
    }

  connectionSourceTraced <- tcpServerTraced $ injectSelector MarloweRuntimeServer -< TcpServerDependencies
    { toPeer = marloweRuntimeServerPeer
    , port = portTraced
    , ..
    }
  probes <- proxy -< ProxyDependencies
    { router = Router
        { connectMarloweSync = tcpClientChannel (injectSelector MarloweSyncClient) syncHost marloweSyncPort
        , connectMarloweHeaderSync = tcpClientChannel (injectSelector MarloweHeaderSyncClient) syncHost marloweHeaderSyncPort
        , connectMarloweQuery = tcpClientChannel (injectSelector MarloweQueryClient) syncHost marloweQueryPort
        , connectTxJob = tcpClientChannel (injectSelector TxJobClient) txHost txPort
        }
    , connectionSource = SomeConnectionSource
        $ handshakeConnectionSource connectionSource
    , connectionSourceTraced = SomeConnectionSourceTraced (injectSelector MarloweRuntimeServer)
        $ handshakeConnectionSourceTraced connectionSourceTraced
    }

  probeServer -< ProbeServerDependencies { port = fromIntegral httpPort, .. }

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
  , portTraced :: PortNumber
  , syncHost :: HostName
  , marloweSyncPort :: PortNumber
  , marloweHeaderSyncPort :: PortNumber
  , marloweQueryPort :: PortNumber
  , txHost :: HostName
  , txPort :: PortNumber
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
    ( helper <*>
      ( Options
          <$> hostParser
          <*> portParser
          <*> portTracedParser
          <*> syncHostParser
          <*> marloweSyncPortParser
          <*> marloweHeaderSyncPortParser
          <*> marloweQueryPortParser
          <*> txHostParser
          <*> txPortParser
          <*> httpPortParser
      )
    )
    infoMod
  where
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

    portTracedParser = option auto $ mconcat
      [ long "port-traced"
      , value 3701
      , metavar "PORT_NUMBER"
      , help "The port number to run the server with tracing on."
      , showDefault
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
