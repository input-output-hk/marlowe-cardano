{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent.Component
import Control.Concurrent.Component.Probes (ProbeServerDependencies (..), probeServer)
import Control.Concurrent.Component.Run (AppM, runAppMTraced)
import Control.Monad (when)
import Control.Monad.Event.Class
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Data.Version (showVersion)
import Data.Word (Word64)
import Language.Marlowe.Protocol.BulkSync.Client (marloweBulkSyncClientPeer)
import Language.Marlowe.Protocol.Load.Server (marloweLoadServerPeer)
import Language.Marlowe.Protocol.Transfer.Server (marloweTransferServerPeer)
import Language.Marlowe.Runtime.CLI.Option (optParserWithEnvDefault)
import qualified Language.Marlowe.Runtime.CLI.Option as O
import Language.Marlowe.Runtime.Contract
import Language.Marlowe.Runtime.Contract.Store (traceContractStore)
import Language.Marlowe.Runtime.Contract.Store.File (
  ContractStoreOptions (..),
  createContractStore,
  defaultContractStoreOptions,
 )
import Logging (RootSelector (..), renderRootSelectorOTel)
import Network.Protocol.Connection (Connection (..), ConnectionTraced (..), Connector (..), ConnectorTraced (..))
import Network.Protocol.Driver (TcpServerDependencies (..))
import Network.Protocol.Driver.Trace (tcpClientTraced, tcpServerTraced)
import Network.Protocol.Peer.Monad.TCP (tcpClientPeerTTraced)
import Network.Protocol.Query.Client (queryClientPeerT)
import Network.Protocol.Query.Server (queryServerPeer)
import qualified Network.Protocol.Query.Types as Query
import Network.Socket (HostName, PortNumber)
import Network.TypedProtocol (unsafeIntToNat)
import Observe.Event (injectSelector)
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
import Options.Applicative.Help.Pretty
import Paths_marlowe_runtime

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
run Options{..} = do
  contractStore <-
    traceContractStore inject
      <$> createContractStore ContractStoreOptions{..}
  flip runComponent_ () proc _ -> do
    let chainSyncQueryConnector =
          let connectorTraced =
                tcpClientPeerTTraced
                  "chain-query"
                  Query.TokDone
                  (injectSelector ChainSyncQueryClient)
                  chainSyncHost
                  chainSyncQueryPort
           in Connector do
                ConnectionTraced{..} <- openConnectionTraced connectorTraced
                pure $ Connection \client -> runConnectionTraced \inj -> queryClientPeerT inj client

        marloweBulkSyncConnector =
          tcpClientTraced
            (injectSelector MarloweBulkSyncClient)
            marloweSyncHost
            marloweSyncBulkSyncPort
            marloweBulkSyncClientPeer

    MarloweContract{..} <-
      contract
        -<
          ContractDependencies
            { contractStore
            , batchSize = unsafeIntToNat bufferSize
            , marloweBulkSyncConnector
            , chainSyncQueryConnector
            }

    tcpServerTraced "contract-load" inject
      -<
        TcpServerDependencies
          { toPeer = marloweLoadServerPeer
          , serverSource = loadServerSource
          , ..
          }

    tcpServerTraced "contract-query" inject
      -<
        TcpServerDependencies
          { toPeer = queryServerPeer
          , port = queryPort
          , serverSource = queryServerSource
          , ..
          }

    tcpServerTraced "contract-transfer" inject
      -<
        TcpServerDependencies
          { toPeer = marloweTransferServerPeer
          , port = transferPort
          , serverSource = transferServerSource
          , ..
          }

    probeServer -< ProbeServerDependencies{port = fromIntegral httpPort, ..}

data Options = Options
  { host :: HostName
  , port :: PortNumber
  , queryPort :: PortNumber
  , transferPort :: PortNumber
  , chainSyncHost :: HostName
  , chainSyncQueryPort :: PortNumber
  , marloweSyncHost :: HostName
  , marloweSyncBulkSyncPort :: PortNumber
  , bufferSize :: Int
  , contractStoreDirectory :: FilePath
  , contractStoreStagingDirectory :: FilePath
  , lockingMicrosecondsBetweenRetries :: Word64
  , httpPort :: PortNumber
  , minContractAge :: NominalDiffTime
  , maxStoreSize :: Integer
  }

getOptions :: IO Options
getOptions = do
  ContractStoreOptions{..} <- defaultContractStoreOptions
  marloweSyncHostParser <- optParserWithEnvDefault O.syncHost
  marloweSyncBulkSyncPortParser <- optParserWithEnvDefault O.syncBulkPort
  execParser $
    info
      ( helper
          <*> versionOption
          <*> ( Options
                  <$> hostParser
                  <*> portParser
                  <*> queryPortParser
                  <*> transferPortParser
                  <*> chainSyncHostParser
                  <*> chainSyncQueryPortParser
                  <*> marloweSyncHostParser
                  <*> marloweSyncBulkSyncPortParser
                  <*> bufferSizeParser
                  <*> contractStoreDirectoryParser contractStoreDirectory
                  <*> contractStoreStagingDirectoryParser contractStoreStagingDirectory
                  <*> lockingMicrosecondsBetweenRetriesParser lockingMicrosecondsBetweenRetries
                  <*> httpPortParser
                  <*> minContractAgeParser minContractAge
                  <*> maxStoreSizeParser maxStoreSize
              )
      )
      infoMod
  where
    versionOption =
      infoOption
        ("marlowe-contract " <> showVersion version)
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
          , value 3727
          , metavar "PORT_NUMBER"
          , help "The port number to run the marlowe load server on."
          , showDefault
          ]

    queryPortParser =
      option auto $
        mconcat
          [ long "query-port"
          , value 3728
          , metavar "PORT_NUMBER"
          , help "The port number to run the query server on."
          , showDefault
          ]

    transferPortParser =
      option auto $
        mconcat
          [ long "transfer-port"
          , value 3729
          , metavar "PORT_NUMBER"
          , help "The port number to run the transfer server on."
          , showDefault
          ]

    chainSyncHostParser =
      strOption $
        mconcat
          [ long "chain-sync-host"
          , value "127.0.0.1"
          , metavar "HOST_NAME"
          , help "The host name of the chain sync server."
          , showDefault
          ]

    chainSyncQueryPortParser =
      option auto $
        mconcat
          [ long "chain-sync-query-port"
          , value 3716
          , metavar "PORT_NUMBER"
          , help "The chain sync server's query protocol port."
          , showDefault
          ]

    bufferSizeParser =
      option readOption $
        mconcat
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

    contractStoreDirectoryParser defaultValue =
      strOption $
        mconcat
          [ long "store-dir"
          , short 's'
          , value defaultValue
          , metavar "DIR"
          , help "The root directory of the contract store"
          , showDefault
          ]

    contractStoreStagingDirectoryParser defaultValue =
      strOption $
        mconcat
          [ long "store-staging-dir"
          , value defaultValue
          , metavar "DIR"
          , help "The root directory of the contract store staging areas"
          , showDefault
          ]

    lockingMicrosecondsBetweenRetriesParser defaultValue =
      option auto $
        mconcat
          [ long "store-lock-microseconds-between-retries"
          , value defaultValue
          , metavar "MICRO_SECONDS"
          , help "The number of microseconds to wait between retries when acquiring the store lock"
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

    minContractAgeParser def =
      option auto $
        mconcat
          [ long "min-contract-age"
          , metavar "MINUTES"
          , help "The minimum age contracts in the store must reach before they can be garbage collected."
          , value def
          , showDefault
          ]

    maxStoreSizeParser def =
      option auto $
        mconcat
          [ long "max-store-size"
          , metavar "BYTES"
          , help "The maximum allowed size of the contract store, in bytes."
          , value def
          , showDefault
          ]

    infoMod =
      mconcat
        [ fullDesc
        , progDescDoc $ Just description
        , header "marlowe-contract: Contract storage service for the Marlowe Runtime."
        ]

description :: Doc
description =
  concatWith
    (\a b -> a <> line <> line <> b)
    [ vcat
        [ "The contract storage service for the Marlowe Runtime. It manages a crucial component of the"
        , "Marlowe runtime: the contract store. The contract store is a content-addressable store of"
        , "contracts indexed by their hashes. Contracts can refer to sub-contracts via their hashes via"
        , "the MerkleizedCase construct. The contract store is used to store the continuations of a contract"
        , "after it has been merkleized (a process which recursively replaces Case constructs with MerkleizedCase"
        , "constructs). It is also used to lookup continuations when applying inputs to a merkleized contract."
        , "This component exposes three protocols: marlowe load, marlowe transfer, and contract store query."
        ]
    , vcat
        [ "The marlowe load protocol is one way to import a contract incrementally into the store. It presents"
        , "a stack-based interface for pushing a contract depth-first into the store."
        ]
    , vcat
        [ "The marlowe transfer protocol is the other way to import a contract incrementally into the store."
        , "It leverages the Marlowe object model to allow bundles of user-defined marlowe objects to be streamed"
        , "into the store. marlowe-contract will link the contract on-the-fly, merkleize the intermediate contracts,"
        , "and build the final contract incrementally. This protocol is generally more efficient and flexible than"
        , "Marlowe load because it allows duplicate sub-contracts to be pre-abstracted by the user. It also supports"
        , "An export mode which will stream the closure of a contract from the store to the client as a Marlowe object"
        , "bundle. This can be used to export continuations from the store and share it with other contract stores."
        ]
    , vcat
        [ "The contract store query protocol provides queries that allow clients to fetch contracts by their hash,"
        , "fetch the adjacency or closure sets of a contract, or merkleize an input."
        ]
    , vcat
        [ "marlowe-contract depends on a marlowe-sync and marlowe-chain-sync instance to run automatic"
        , "Garbage collection. These must both be running in order for marlowe-contract to run."
        ]
    ]
