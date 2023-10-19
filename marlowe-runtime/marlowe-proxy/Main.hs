{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent.Component
import Control.Concurrent.Component.Probes (ProbeServerDependencies (..), probeServer)
import Control.Concurrent.Component.Run (AppM, runAppMTraced)
import qualified Data.Text as T
import Data.Version (showVersion)
import Language.Marlowe.Protocol.Server (marloweRuntimeServerPeer)
import Language.Marlowe.Runtime.CLI.Option (optParserWithEnvDefault)
import qualified Language.Marlowe.Runtime.CLI.Option as O
import Language.Marlowe.Runtime.Proxy
import Logging (RootSelector (..), renderRootSelectorOTel)
import Network.Channel.Typed
import Network.Protocol.Driver (TcpServerDependencies (..), tcpServer)
import Network.Protocol.Driver.Trace (tcpServerTraced)
import Network.Socket (HostName, PortNumber)
import Observe.Event.Backend (injectSelector)
import OpenTelemetry.Trace
import Options.Applicative (
  auto,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  infoOption,
  long,
  metavar,
  option,
  progDescDoc,
  short,
  showDefault,
  strOption,
  value,
 )
import Paths_marlowe_runtime
import Prettyprinter

main :: IO ()
main = do
  options <- getOptions
  runAppMTraced instrumentationLibrary renderRootSelectorOTel $ run options
  where
    instrumentationLibrary =
      InstrumentationLibrary
        { libraryName = "marlowe-proxy"
        , libraryVersion = T.pack $ showVersion version
        }

run :: Options -> AppM Span RootSelector ()
run = runComponent_ proc Options{..} -> do
  MarloweProxy{..} <-
    proxy
      -<
        Router
          { connectMarloweSync = tcpClientChannel (injectSelector MarloweSyncClient) syncHost marloweSyncPort
          , connectMarloweHeaderSync = tcpClientChannel (injectSelector MarloweHeaderSyncClient) syncHost marloweHeaderSyncPort
          , connectMarloweBulkSync = tcpClientChannel (injectSelector MarloweBulkSyncClient) syncHost marloweBulkSyncPort
          , connectMarloweQuery = tcpClientChannel (injectSelector MarloweQueryClient) syncHost marloweQueryPort
          , connectMarloweLoad = tcpClientChannel (injectSelector MarloweLoadClient) contractHost marloweLoadPort
          , connectMarloweTransfer = tcpClientChannel (injectSelector MarloweTransferClient) contractHost marloweTransferPort
          , connectTxJob = tcpClientChannel (injectSelector TxJobClient) txHost txPort
          , connectContractQuery = tcpClientChannel (injectSelector ContractQueryClient) contractHost contractQueryPort
          }

  tcpServer "marlowe-runtime"
    -<
      TcpServerDependencies
        { toPeer = marloweRuntimeServerPeer
        , serverSource = proxyServerSource False
        , ..
        }

  tcpServerTraced "marlowe-runtime-traced" $ injectSelector MarloweRuntimeServer
    -<
      TcpServerDependencies
        { toPeer = marloweRuntimeServerPeer
        , port = portTraced
        , serverSource = proxyServerSource True
        , ..
        }

  probeServer -< ProbeServerDependencies{port = fromIntegral httpPort, ..}

data Options = Options
  { host :: HostName
  , port :: PortNumber
  , portTraced :: PortNumber
  , syncHost :: HostName
  , marloweSyncPort :: PortNumber
  , marloweHeaderSyncPort :: PortNumber
  , marloweBulkSyncPort :: PortNumber
  , marloweQueryPort :: PortNumber
  , contractHost :: HostName
  , marloweLoadPort :: PortNumber
  , marloweTransferPort :: PortNumber
  , contractQueryPort :: PortNumber
  , txHost :: HostName
  , txPort :: PortNumber
  , httpPort :: PortNumber
  }

getOptions :: IO Options
getOptions = do
  syncHostParser <- optParserWithEnvDefault O.syncHost
  marloweSyncPortParser <- optParserWithEnvDefault O.syncSyncPort
  marloweHeaderSyncPortParser <- optParserWithEnvDefault O.syncHeaderPort
  marloweBulkSyncPortParser <- optParserWithEnvDefault O.syncBulkPort
  marloweQueryPortParser <- optParserWithEnvDefault O.syncQueryPort
  contractHostParser <- optParserWithEnvDefault O.contractHost
  marloweLoadPortParser <- optParserWithEnvDefault O.loadPort
  marloweTransferPortParser <- optParserWithEnvDefault O.transferPort
  contractQueryPortParser <- optParserWithEnvDefault O.contractQueryPort
  txHostParser <- optParserWithEnvDefault O.txHost
  txPortParser <- optParserWithEnvDefault O.txCommandPort
  execParser $
    info
      ( helper
          <*> versionOption
          <*> ( Options
                  <$> hostParser
                  <*> portParser
                  <*> portTracedParser
                  <*> syncHostParser
                  <*> marloweSyncPortParser
                  <*> marloweHeaderSyncPortParser
                  <*> marloweBulkSyncPortParser
                  <*> marloweQueryPortParser
                  <*> contractHostParser
                  <*> marloweLoadPortParser
                  <*> marloweTransferPortParser
                  <*> contractQueryPortParser
                  <*> txHostParser
                  <*> txPortParser
                  <*> httpPortParser
              )
      )
      infoMod
  where
    versionOption =
      infoOption
        ("marlowe-proxy " <> showVersion version)
        (long "version" <> help "Show version.")

    hostParser =
      strOption $
        mconcat
          [ long "host"
          , short 'h'
          , value "127.0.0.1"
          , metavar "HOST_NAME"
          , help "The host name to run the server on."
          , showDefault
          ]

    portParser =
      option auto $
        mconcat
          [ long "port"
          , short 'p'
          , value 3700
          , metavar "PORT_NUMBER"
          , help "The port number to run the server on."
          , showDefault
          ]

    portTracedParser =
      option auto $
        mconcat
          [ long "port-traced"
          , value 3701
          , metavar "PORT_NUMBER"
          , help "The port number to run the server with tracing on."
          , showDefault
          ]

    httpPortParser =
      option auto $
        mconcat
          [ long "http-port"
          , metavar "PORT_NUMBER"
          , help "Port number to serve the http healthcheck API on"
          , value 8080
          , showDefault
          ]

    infoMod =
      mconcat
        [ fullDesc
        , progDescDoc $ Just description
        , header "marlowe-proxy: The API gateway server for the Marlowe Runtime."
        ]

description :: Doc ann
description =
  concatWith
    (\a b -> a <> line <> line <> b)
    [ vcat
        [ "The API gateway server for the Marlowe Runtime. It exposes all the public protocols of the"
        , "Marlowe runtime as a single multiplexed protocol: marlowe sync, marlowe header sync, marlowe"
        , "bulk sync, marlowe query, marlowe transaction job, marlowe load, marlowe transfer, and contract"
        , "store query. Please consult the help text for the individual services for detailed descriptions"
        , "of these individual protocols."
        ]
    ]
