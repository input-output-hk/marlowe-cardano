{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Concurrent.Component
import Control.Concurrent.Component.Probes (ProbeServerDependencies (..), probeServer)
import Control.Concurrent.Component.Run (AppM, runAppMTraced)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Data.Version (showVersion)
import Language.Marlowe.Runtime.ChainSync.Api (
  BlockNo (..),
  ChainSyncQuery (..),
  RuntimeChainSeekClient,
 )
import Language.Marlowe.Runtime.Contract.Api (ContractRequest)
import qualified Language.Marlowe.Runtime.Core.ScriptRegistry as ScriptRegistry
import Language.Marlowe.Runtime.Transaction (
  MarloweTx (..),
  TransactionDependencies (..),
  mkCommandLineRoleTokenMintingPolicy,
  transaction,
 )
import qualified Language.Marlowe.Runtime.Transaction.Query as Query
import qualified Language.Marlowe.Runtime.Transaction.Submit as Submit
import Logging (RootSelector (..), renderRootSelectorOTel)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import Network.Protocol.Connection (Connector, runConnector)
import Network.Protocol.Driver (TcpServerDependencies (..), tcpClient)
import Network.Protocol.Driver.Trace (tcpClientTraced, tcpServerTraced)
import Network.Protocol.Job.Client (jobClientPeer)
import Network.Protocol.Job.Server (jobServerPeer)
import Network.Protocol.Query.Client (QueryClient, queryClientPeer, request)
import Network.Socket (HostName, PortNumber)
import Observe.Event.Backend (injectSelector)
import OpenTelemetry.Trace hiding (Server)
import Options.Applicative (
  auto,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
  showDefault,
  strOption,
  value,
 )
import Paths_marlowe_runtime (version)

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
run Options{..} = flip runComponent_ () proc _ -> do
  let chainSyncConnector :: Connector RuntimeChainSeekClient (AppM Span RootSelector)
      chainSyncConnector = tcpClient chainSeekHost chainSeekPort chainSeekClientPeer

      chainSyncQueryConnector :: Connector (QueryClient ChainSyncQuery) (AppM Span RootSelector)
      chainSyncQueryConnector = tcpClientTraced (injectSelector ChainSyncQueryClient) chainSeekHost chainSeekQueryPort queryClientPeer

      contractQueryConnector :: Connector (QueryClient ContractRequest) (AppM Span RootSelector)
      contractQueryConnector = tcpClientTraced (injectSelector ContractQueryClient) contractHost contractQueryPort queryClientPeer

  MarloweTx{..} <-
    transaction
      -<
        TransactionDependencies
          { mkSubmitJob =
              Submit.mkSubmitJob
                Submit.SubmitJobDependencies
                  { chainSyncJobConnector =
                      tcpClientTraced (injectSelector ChainSyncJobClient) chainSeekHost chainSeekCommandPort jobClientPeer
                  , pollingInterval = 1.5
                  , confirmationTimeout = 3600 -- 1 hour
                  , ..
                  }
          , loadMarloweContext = \v contractId -> do
              networkId <- runConnector chainSyncQueryConnector $ request GetNetworkId
              Query.loadMarloweContext ScriptRegistry.getScripts networkId chainSyncConnector chainSyncQueryConnector v contractId
          , loadWalletContext = Query.loadWalletContext $ runConnector chainSyncQueryConnector . request . GetUTxOs
          , loadPayoutContext = \v payouts -> do
              networkId <- runConnector chainSyncQueryConnector $ request GetNetworkId
              Query.loadPayoutContext
                ScriptRegistry.getScripts
                networkId
                (runConnector chainSyncQueryConnector . request . GetUTxOs)
                v
                payouts
          , getCurrentScripts = ScriptRegistry.getCurrentScripts
          , analysisTimeout = analysisTimeout
          , mkRoleTokenMintingPolicy = mkCommandLineRoleTokenMintingPolicy mintingPolicyCmd
          , ..
          }

  tcpServerTraced "tx-job" (injectSelector Server)
    -<
      TcpServerDependencies
        { toPeer = jobServerPeer
        , ..
        }

  probeServer -< ProbeServerDependencies{port = fromIntegral httpPort, ..}

data Options = Options
  { chainSeekPort :: PortNumber
  , chainSeekQueryPort :: PortNumber
  , chainSeekCommandPort :: PortNumber
  , chainSeekHost :: HostName
  , contractQueryPort :: PortNumber
  , contractHost :: HostName
  , port :: PortNumber
  , host :: HostName
  , submitConfirmationBlocks :: BlockNo
  , analysisTimeout :: NominalDiffTime
  , httpPort :: PortNumber
  , mintingPolicyCmd :: FilePath
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> parser) infoMod
  where
    parser =
      Options
        <$> chainSeekPortParser
        <*> chainSeekQueryPortParser
        <*> chainSeekCommandPortParser
        <*> chainSeekHostParser
        <*> contractQueryPortParser
        <*> contractHostParser
        <*> portParser
        <*> hostParser
        <*> submitConfirmationBlocksParser
        <*> analysisTimeoutParser
        <*> httpPortParser
        <*> mintingPolicyCmdParser

    chainSeekPortParser =
      option auto $
        mconcat
          [ long "chain-sync-port"
          , value 3715
          , metavar "PORT_NUMBER"
          , help "The port number of the chain sync server."
          , showDefault
          ]

    chainSeekQueryPortParser =
      option auto $
        mconcat
          [ long "chain-sync-query-port"
          , value 3716
          , metavar "PORT_NUMBER"
          , help "The port number of the chain sync query server."
          , showDefault
          ]

    chainSeekCommandPortParser =
      option auto $
        mconcat
          [ long "chain-sync-command-port"
          , value 3720
          , metavar "PORT_NUMBER"
          , help "The port number of the chain sync job server."
          , showDefault
          ]

    contractQueryPortParser =
      option auto $
        mconcat
          [ long "contract-query-port"
          , value 3728
          , metavar "PORT_NUMBER"
          , help "The port number of the contract query server."
          , showDefault
          ]

    portParser =
      option auto $
        mconcat
          [ long "command-port"
          , value 3723
          , metavar "PORT_NUMBER"
          , help "The port number to run the job server on."
          , showDefault
          ]

    chainSeekHostParser =
      strOption $
        mconcat
          [ long "chain-sync-host"
          , value "127.0.0.1"
          , metavar "HOST_NAME"
          , help "The host name of the chain sync server."
          , showDefault
          ]

    contractHostParser =
      strOption $
        mconcat
          [ long "contract-host"
          , value "127.0.0.1"
          , metavar "HOST_NAME"
          , help "The host name of the contract server."
          , showDefault
          ]

    hostParser =
      strOption $
        mconcat
          [ long "host"
          , short 'h'
          , value "127.0.0.1"
          , metavar "HOST_NAME"
          , help "The host name to run the tx server on."
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

    mintingPolicyCmdParser =
      strOption $
        mconcat
          [ long "minting-policy-cmd"
          , metavar "CMD"
          , help
              "A command which creates the role token minting policy for a contract. It should read the arguments via the command line and output the serialized script binary to stdout."
          ]

    submitConfirmationBlocksParser =
      option (BlockNo <$> auto) $
        mconcat
          [ long "submit-confirmation-blocks"
          , value 0
          , metavar "INTEGER"
          , help
              "The number of blocks after a transaction has been confirmed to wait before displaying the block in which was confirmed."
          , showDefault
          ]

    analysisTimeoutParser =
      option (fromInteger <$> auto) $
        mconcat
          [ long "analysis-timeout"
          , value 15
          , metavar "SECONDS"
          , help "The amount of time allotted for safety analysis of a contract."
          , showDefault
          ]

    infoMod =
      mconcat
        [ fullDesc
        , progDesc "Marlowe runtime transaction creation server"
        , header "marlowe-tx : the transaction creation server of the Marlowe Runtime"
        ]
