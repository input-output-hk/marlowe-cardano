{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent.Component
import Control.Concurrent.Component.Probes (ProbeServerDependencies(..), probeServer)
import Control.Concurrent.Component.Run (AppM, runAppMTraced)
import Control.Monad (when)
import Control.Monad.Event.Class
import qualified Data.Text as T
import Data.Version (showVersion)
import Data.Word (Word64)
import Language.Marlowe.Protocol.Load.Server (marloweLoadServerPeer)
import Language.Marlowe.Runtime.Contract
import Language.Marlowe.Runtime.Contract.Store (traceContractStore)
import Language.Marlowe.Runtime.Contract.Store.File
  (ContractStoreOptions(..), createContractStore, defaultContractStoreOptions)
import Logging (RootSelector(..), renderRootSelectorOTel)
import Network.Protocol.Driver (TcpServerDependencies(..))
import Network.Protocol.Driver.Trace (tcpServerTraced)
import Network.Protocol.Query.Server (queryServerPeer)
import Network.Socket (HostName, PortNumber)
import Network.TypedProtocol (unsafeIntToNat)
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

main :: IO ()
main = do
  options <- getOptions
  runAppMTraced instrumentationLibrary renderRootSelectorOTel $ run options
  where
    instrumentationLibrary = InstrumentationLibrary
      { libraryName = "marlowe-proxy"
      , libraryVersion = T.pack $ showVersion version
      }

run :: Options -> AppM Span RootSelector ()
run Options{..} = do
  contractStore <- traceContractStore inject
    <$> createContractStore ContractStoreOptions{..}
  flip runComponent_ () proc _ -> do
    MarloweContract{..} <- contract -< ContractDependencies
      { contractStore
      , batchSize = unsafeIntToNat bufferSize
      }

    tcpServerTraced "contract-load" inject -< TcpServerDependencies
      { toPeer = marloweLoadServerPeer
      , serverSocket = loadSocket
      , ..
      }

    tcpServerTraced "contract-query" inject -< TcpServerDependencies
      { toPeer = queryServerPeer
      , port = queryPort
      , serverSocket = querySocket
      , ..
      }

    probeServer -< ProbeServerDependencies { port = fromIntegral httpPort, .. }

data Options = Options
  { host :: HostName
  , port :: PortNumber
  , queryPort :: PortNumber
  , bufferSize :: Int
  , contractStoreDirectory :: FilePath
  , contractStoreStagingDirectory :: FilePath
  , lockingMicrosecondsBetweenRetries :: Word64
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
          <*> lockingMicrosecondsBetweenRetriesParser lockingMicrosecondsBetweenRetries
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

    lockingMicrosecondsBetweenRetriesParser defaultValue = option auto $ mconcat
      [ long "store-lock-microseconds-between-retries"
      , value defaultValue
      , metavar "MICRO_SECONDS"
      , help "The number of microseconds to wait between retries when acquiring the store lock"
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
