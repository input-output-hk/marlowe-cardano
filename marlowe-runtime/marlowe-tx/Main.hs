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
  WithGenesis (..),
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
import qualified Language.Marlowe.Runtime.Transaction.Query.Helper as Helper
import qualified Language.Marlowe.Runtime.Transaction.Submit as Submit
import Logging (RootSelector (..), renderRootSelectorOTel)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import qualified Network.Protocol.ChainSeek.Types as ChainSeek
import Network.Protocol.Connection (
  Connection (..),
  ConnectionTraced (..),
  Connector (..),
  ConnectorTraced (..),
  runConnector,
 )
import Network.Protocol.Driver (TcpServerDependencies (..))
import Network.Protocol.Driver.Trace (tcpClientTraced, tcpServerTraced)
import Network.Protocol.Job.Client (jobClientPeer)
import Network.Protocol.Job.Server (jobServerPeer)
import Network.Protocol.Peer.Monad.TCP (tcpClientPeerTTraced)
import Network.Protocol.Query.Client (QueryClient, queryClientPeer, queryClientPeerT, request)
import qualified Network.Protocol.Query.Types as Query
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
import Paths_marlowe_runtime (version)
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, bold)

main :: IO ()
main = do
  options <- getOptions
  runAppMTraced instrumentationLibrary renderRootSelectorOTel $ run options
  where
    instrumentationLibrary =
      InstrumentationLibrary
        { libraryName = "marlowe-tx"
        , libraryVersion = T.pack $ showVersion version
        }

run :: Options -> AppM Span RootSelector ()
run Options{..} = flip runComponent_ () proc _ -> do
  let chainSyncConnector :: Connector RuntimeChainSeekClient (AppM Span RootSelector)
      chainSyncConnector =
        let connectorTraced =
              tcpClientPeerTTraced
                "chain-seek"
                ChainSeek.TokDone
                (injectSelector ChainSeekClient)
                chainSeekHost
                chainSeekPort
         in Connector do
              ConnectionTraced{..} <- openConnectionTraced connectorTraced
              pure $ Connection \client -> runConnectionTraced \inj -> chainSeekClientPeer Genesis inj client

      chainSyncQueryConnector :: Connector (QueryClient ChainSyncQuery) (AppM Span RootSelector)
      chainSyncQueryConnector =
        let connectorTraced =
              tcpClientPeerTTraced
                "chain-query"
                Query.TokDone
                (injectSelector ChainSyncQueryClient)
                chainSeekHost
                chainSeekQueryPort
         in Connector do
              ConnectionTraced{..} <- openConnectionTraced connectorTraced
              pure $ Connection \client -> runConnectionTraced \inj -> queryClientPeerT inj client

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
          , loadHelpersContext = \v contractId -> do
              networkId <- runConnector chainSyncQueryConnector $ request GetNetworkId
              Helper.loadHelpersContext
                ScriptRegistry.getCurrentScripts
                ScriptRegistry.getScripts
                networkId
                chainSyncConnector
                v
                contractId
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
getOptions = execParser $ info (helper <*> versionOption <*> parser) infoMod
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

    versionOption =
      infoOption
        ("marlowe-tx " <> showVersion version)
        (long "version" <> help "Show version.")

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
        , progDescDoc $ Just description
        , header "marlowe-tx: Transaction creation server for the Marlowe Runtime."
        ]

bullets :: [Doc ann] -> Doc ann
bullets = indent 2 . vcat . fmap (("•" <+>) . align)

description :: Doc AnsiStyle
description =
  concatWith
    (\a b -> a <> line <> line <> b)
    [ vcat
        [ "The transaction creation engine for the Marlowe Runtime. This component exposes"
        , "a job protocol with four commands: create, apply inputs, withdraw, and submit."
        ]
    , annotate bold "Create Command"
    , vcat
        [ "The create command will create an unsigned transaction body that starts a new Marlowe"
        , "contract. The options that can be configured for this command are:"
        ]
    , bullets $
        vsep
          <$> [ ["Adding a staking credential to the Marlowe script address for the contract."]
              , ["What addresses can be used for coin selection."]
              , ["Which address to send change to."]
              ,
                [ "Rules for role tokens in the contract. Includes rules for minting and distributing"
                , "role tokens in the creation transaction and rules for using existing role tokens."
                ]
              , ["Adding metadata to the transaction"]
              ,
                [ "The amount that should be deposited in the contract to cover min UTxO rules"
                , "(can be omitted and the runtime will compute a worst-case value automatically."
                ]
              ,
                [ "The initial contract or the hash of a contract in the contract store to use as the"
                , "initial contract."
                ]
              ]
    , annotate bold "Apply Inputs Command"
    , vcat
        [ "The apply inputs command will create an unsigned transaction body that advances a"
        , "Marlowe contract by applying transaction inputs to it. The options that can be"
        , "configured for this command are:"
        ]
    , bullets $
        vsep
          <$> [ ["What addresses can be used for coin selection."]
              , ["Which address to send change to."]
              , ["Adding metadata to the transaction"]
              ,
                [ "The transaction's validity interval. If omitted, the runtime will compute default"
                , "values from the current contract and the most recent block's slot number."
                ]
              ,
                [ "The initial contract or the hash of a contract in the contract store to use as the"
                , "initial contract."
                ]
              ]
    , annotate bold "Withdraw Command"
    , vcat
        [ "The withdraw command will create an unsigned transaction body that withdraws role payouts"
        , "from Marlowe contracts from the role payout validators that hold them. When a contract"
        , "makes a payment to a role, it is not sent directly to the holder of the role token. Instead,"
        , "it is sent to an auxiliary Plutus script called a payout validator. The holder of the matching"
        , "role token is then able to spend this script output to withdraw the payout, sending it to an"
        , "address of their choice. That is what withdrawal means in this context. The options that can be"
        , "configured for this command are:"
        ]
    , bullets $
        vsep
          <$> [ ["What addresses can be used for coin selection."]
              , ["Which address to send change to."]
              , ["Which payout outputs to withdraw."]
              ]
    , annotate bold "Submit Command"
    , vcat
        [ "The submit command will submit a signed transaction to a cardano node via a marlowe-chain-sync"
        , "instance. It waits until the transaction is confirmed and found in a block by marlowe-chain-sync"
        , "The options that can be configured for this command are:"
        ]
    , bullets $
        vsep
          <$> [ ["The era of the transaction (babbage or conway)"]
              , ["The transaction to submit."]
              ]
    , annotate bold "Dependencies"
    , vcat
        [ "marlowe-tx depends on a marlowe-chain-sync instance at various points. First, it runs a"
        , "chain seek client for the lifetime of the service to keep track of what the current tip"
        , "of the blockchain is. Second, it connects via both chain seek and chain query to fetch"
        , "current information about the UTxO for wallets and marlowe contracts needed to create"
        , "transactions."
        ]
    , vcat
        [ "marlowe-tx also depends on a marlowe-contract instance both to create contracts and"
        , "apply inputs to merkleized contracts. For creation, it queries the store for the initial"
        , "contract if a contract hash was provided in the creation command. It also fetches the"
        , "closure of contract continuations from the store to analyze the contract for safety issues."
        , "Finally, it queries contract hashes found in merkleized cases to build merkleized inputs"
        , "automatically while executing an apply inputs command."
        ]
    , annotate bold "Scaling"
    , vcat
        [ "marlowe-tx is designed to scale horizontally. That is to say, multiple instances can run"
        , "in parallel to scale with demand. A typical setup for this would involve running multiple"
        , "marlowe-tx instances in front of a load balancer."
        ]
    ]
