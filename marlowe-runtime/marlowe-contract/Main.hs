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
import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Event.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.With
import Data.GeneralAllocate
import qualified Data.Text as T
import Data.Version (showVersion)
import Language.Marlowe.Protocol.Load.Server (marloweLoadServerPeer)
import Language.Marlowe.Runtime.Contract
import Language.Marlowe.Runtime.Contract.Store (traceContractStore)
import Language.Marlowe.Runtime.Contract.Store.File
  (ContractStoreOptions(..), createContractStore, defaultContractStoreOptions)
import Logging (RootSelector(..), renderRootSelectorOTel)
import Network.Protocol.Connection (SomeConnectionSourceTraced(..))
import Network.Protocol.Driver (TcpServerDependencies(..))
import Network.Protocol.Driver.Trace (tcpServerTraced)
import Network.Protocol.Handshake.Server (handshakeConnectionSourceTraced)
import Network.Protocol.Query.Server (queryServerPeer)
import Network.Socket (HostName, PortNumber)
import Network.TypedProtocol (unsafeIntToNat)
import Observe.Event.Backend (EventBackend, hoistEventBackend)
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
import UnliftIO (BufferMode(LineBuffering), MonadUnliftIO, hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
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
run Options{..} = do
  contractStore <- traceContractStore inject
    <$> createContractStore ContractStoreOptions{..}
  flip runComponent_ () proc _ -> do
    loadSource <- tcpServerTraced inject -< TcpServerDependencies
      { toPeer = marloweLoadServerPeer
      , ..
      }

    querySource <- tcpServerTraced inject -< TcpServerDependencies
      { toPeer = queryServerPeer
      , port = queryPort
      , ..
      }

    probes <- contract -< ContractDependencies
      { loadSource = SomeConnectionSourceTraced inject
          $ handshakeConnectionSourceTraced loadSource
      , querySource = SomeConnectionSourceTraced inject
          $ handshakeConnectionSourceTraced querySource
      , contractStore
      , batchSize = unsafeIntToNat bufferSize
      }

    probeServer -< ProbeServerDependencies { port = fromIntegral httpPort, .. }

runAppM :: EventBackend IO r RootSelector -> AppM r a -> IO a
runAppM eventBackend = flip runReaderT (hoistEventBackend liftIO eventBackend) . unAppM

newtype AppM r a = AppM
  { unAppM :: ReaderT (EventBackend (AppM r) r RootSelector) IO a
  } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadFail, MonadThrow, MonadCatch, MonadMask)

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
  , queryPort :: PortNumber
  , bufferSize :: Int
  , contractStoreDirectory :: FilePath
  , contractStoreStagingDirectory :: FilePath
  , httpPort :: PortNumber
  }

getOptions :: IO Options
getOptions = do
  ContractStoreOptions{..} <- defaultContractStoreOptions
  execParser $ info
    ( helper <*>
      ( Options
          <$> hostParser
          <*> portParser
          <*> queryPortParser
          <*> bufferSizeParser
          <*> contractStoreDirectoryParser contractStoreDirectory
          <*> contractStoreStagingDirectoryParser contractStoreStagingDirectory
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
      , value 3727
      , metavar "PORT_NUMBER"
      , help "The port number to run the marlowe load server on."
      , showDefault
      ]

    queryPortParser = option auto $ mconcat
      [ long "query-port"
      , value 3728
      , metavar "PORT_NUMBER"
      , help "The port number to run the query server on."
      , showDefault
      ]

    bufferSizeParser = option readOption $ mconcat
      [ long "buffer-size"
      , short 'b'
      , value 512
      , metavar "INTEGER"
      , help "The number of contracts to accept from the client before flushing to disk."
      , showDefault
      ]
      where
        readOption = do
          i <- auto
          when (i <= 0) do
            fail "Positive batch size required"
          pure i

    contractStoreDirectoryParser defaultValue = strOption $ mconcat
      [ long "store-dir"
      , short 's'
      , value defaultValue
      , metavar "DIR"
      , help "The root directory of the contract store"
      , showDefault
      ]

    contractStoreStagingDirectoryParser defaultValue = strOption $ mconcat
      [ long "store-staging-dir"
      , value defaultValue
      , metavar "DIR"
      , help "The root directory of the contract store staging areas"
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
