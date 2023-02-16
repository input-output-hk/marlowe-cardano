{-# LANGUAGE GADTs #-}

module Main
  where

import Control.Arrow (arr, (<<<))
import Control.Concurrent.Component
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Either (fromRight)
import qualified Data.Text.Lazy.IO as TL
import Data.UUID.V4 (nextRandom)
import Data.Void (Void)
import Language.Marlowe.Runtime.ChainSync.Api
  ( BlockNo(..)
  , ChainSyncCommand
  , ChainSyncQuery(..)
  , GetUTxOsQuery
  , RuntimeChainSeekClient
  , UTxOs
  , WithGenesis(..)
  , runtimeChainSeekCodec
  )
import qualified Language.Marlowe.Runtime.Core.ScriptRegistry as ScriptRegistry
import Language.Marlowe.Runtime.Transaction (TransactionDependencies(..), transaction)
import Language.Marlowe.Runtime.Transaction.Query (LoadMarloweContext, LoadWalletContext)
import qualified Language.Marlowe.Runtime.Transaction.Query as Query
import qualified Language.Marlowe.Runtime.Transaction.Submit as Submit
import Logging (RootSelector(..), getRootSelectorConfig)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import Network.Protocol.Driver (RunClient)
import Network.Protocol.Handshake.Client (runClientPeerOverSocketWithLoggingWithHandshake)
import Network.Protocol.Handshake.Server (openServerPortWithHandshake)
import Network.Protocol.Job.Client (JobClient, jobClientPeer)
import Network.Protocol.Job.Codec (codecJob)
import Network.Protocol.Job.Server (jobServerPeer)
import Network.Protocol.Query.Client (QueryClient, liftQuery, queryClientPeer)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Socket (HostName, PortNumber)
import Observe.Event.Backend (narrowEventBackend, newOnceFlagMVar)
import Observe.Event.Component (LoggerDependencies(..), logger)
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
  , optional
  , progDesc
  , short
  , showDefault
  , strOption
  , value
  )
import System.IO (stderr)

main :: IO ()
main = run =<< getOptions

run :: Options -> IO ()
run Options{..} = runResourceT do
  acceptRunTransactionServer' <- openServerPortWithHandshake host port codecJob jobServerPeer
  {- Setup Dependencies -}
  let
    transactionDependencies rootEventBackend =
      let
        acceptRunTransactionServer = acceptRunTransactionServer' $ narrowEventBackend Server rootEventBackend

        connectToChainSeek :: RunClient IO RuntimeChainSeekClient
        connectToChainSeek = runClientPeerOverSocketWithLoggingWithHandshake
          (narrowEventBackend ChainSeekClient rootEventBackend)
          chainSeekHost
          chainSeekPort
          runtimeChainSeekCodec
          (chainSeekClientPeer Genesis)

        runChainSyncJobClient :: RunClient IO (JobClient ChainSyncCommand)
        runChainSyncJobClient = runClientPeerOverSocketWithLoggingWithHandshake
          (narrowEventBackend ChainSyncJobClient rootEventBackend)
          chainSeekHost
          chainSeekCommandPort
          codecJob
          jobClientPeer

        runChainSyncQueryClient :: RunClient IO (QueryClient ChainSyncQuery)
        runChainSyncQueryClient = runClientPeerOverSocketWithLoggingWithHandshake
          (narrowEventBackend ChainSyncQueryClient rootEventBackend)
          chainSeekHost
          chainSeekQueryPort
          codecQuery
          queryClientPeer

        queryChainSync :: ChainSyncQuery Void e a -> IO a
        queryChainSync = fmap (fromRight $ error "failed to query chain sync server") . runChainSyncQueryClient . liftQuery

        mkSubmitJob = Submit.mkSubmitJob Submit.SubmitJobDependencies{..}

        loadMarloweContext :: LoadMarloweContext r
        loadMarloweContext eb version contractId = do
          networkId <- queryChainSync GetNetworkId
          Query.loadMarloweContext ScriptRegistry.getScripts networkId connectToChainSeek runChainSyncQueryClient eb version contractId

        runGetUTxOsQuery :: GetUTxOsQuery -> IO UTxOs
        runGetUTxOsQuery getUTxOsQuery = queryChainSync (GetUTxOs getUTxOsQuery)

        loadWalletContext :: LoadWalletContext r
        loadWalletContext = Query.loadWalletContext runGetUTxOsQuery

        eventBackend = narrowEventBackend App rootEventBackend

        getCurrentScripts = ScriptRegistry.getCurrentScripts
      in TransactionDependencies{..}
    appComponent = transaction <<< arr transactionDependencies <<< logger

  liftIO $ runComponent_ appComponent LoggerDependencies
    { configFilePath = logConfigFile
    , getSelectorConfig = getRootSelectorConfig
    , newRef = nextRandom
    , newOnceFlag = newOnceFlagMVar
    , writeText = TL.hPutStr stderr
    , injectConfigWatcherSelector = ConfigWatcher
    }

data Options = Options
  { chainSeekPort :: PortNumber
  , chainSeekQueryPort :: PortNumber
  , chainSeekCommandPort :: PortNumber
  , chainSeekHost :: HostName
  , port :: PortNumber
  , host :: HostName
  , logConfigFile :: Maybe FilePath
  , submitConfirmationBlocks :: BlockNo
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> parser) infoMod
  where
    parser = Options
      <$> chainSeekPortParser
      <*> chainSeekQueryPortParser
      <*> chainSeekCommandPortParser
      <*> chainSeekHostParser
      <*> portParser
      <*> hostParser
      <*> logConfigFileParser
      <*> submitConfirmationBlocksParser

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
