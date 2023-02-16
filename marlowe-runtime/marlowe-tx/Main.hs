{-# LANGUAGE GADTs #-}

module Main
  where

import Control.Arrow (arr, (<<<))
import Control.Concurrent.Component
import Control.Exception (bracket, bracketOnError)
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
import Network.Protocol.Handshake.Server (acceptRunServerPeerOverSocketWithLoggingWithHandshake)
import Network.Protocol.Job.Client (JobClient, jobClientPeer)
import Network.Protocol.Job.Codec (codecJob)
import Network.Protocol.Job.Server (jobServerPeer)
import Network.Protocol.Query.Client (QueryClient, liftQuery, queryClientPeer)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Socket
  ( AddrInfo(..)
  , AddrInfoFlag(..)
  , HostName
  , PortNumber
  , SocketOption(..)
  , SocketType(..)
  , bind
  , close
  , defaultHints
  , getAddrInfo
  , listen
  , openSocket
  , setCloseOnExecIfNeeded
  , setSocketOption
  , withFdSocket
  , withSocketsDo
  )
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

clientHints :: AddrInfo
clientHints = defaultHints { addrSocketType = Stream }

run :: Options -> IO ()
run Options{..} = withSocketsDo do
  addr <- resolve port
  bracket (openServer addr) close \socket -> do
    {- Setup Dependencies -}
    let
      transactionDependencies rootEventBackend =
        let
          acceptRunTransactionServer = acceptRunServerPeerOverSocketWithLoggingWithHandshake
            (narrowEventBackend Server rootEventBackend)
            socket
            codecJob
            jobServerPeer

          connectToChainSeek :: RunClient IO RuntimeChainSeekClient
          connectToChainSeek client = do
            addr' <- head <$> getAddrInfo (Just clientHints) (Just chainSeekHost) (Just $ show chainSeekPort)
            runClientPeerOverSocketWithLoggingWithHandshake
              (narrowEventBackend ChainSeekClient rootEventBackend)
              addr'
              runtimeChainSeekCodec
              (chainSeekClientPeer Genesis)
              client

          runChainSyncJobClient :: RunClient IO (JobClient ChainSyncCommand)
          runChainSyncJobClient client = do
            addr' <- head <$> getAddrInfo (Just clientHints) (Just chainSeekHost) (Just $ show chainSeekCommandPort)
            runClientPeerOverSocketWithLoggingWithHandshake
              (narrowEventBackend ChainSyncJobClient rootEventBackend)
              addr'
              codecJob
              jobClientPeer
              client

          runChainSyncQueryClient :: RunClient IO (QueryClient ChainSyncQuery)
          runChainSyncQueryClient client = do
            addr' <- head <$> getAddrInfo (Just clientHints) (Just chainSeekHost) (Just $ show chainSeekQueryPort)
            runClientPeerOverSocketWithLoggingWithHandshake
              (narrowEventBackend ChainSyncQueryClient rootEventBackend)
              addr'
              codecQuery
              queryClientPeer
              client

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
    runComponent_ appComponent LoggerDependencies
      { configFilePath = logConfigFile
      , getSelectorConfig = getRootSelectorConfig
      , newRef = nextRandom
      , newOnceFlag = newOnceFlagMVar
      , writeText = TL.hPutStr stderr
      , injectConfigWatcherSelector = ConfigWatcher
      }
  where
    openServer addr = bracketOnError (openSocket addr) close \socket -> do
      setSocketOption socket ReuseAddr 1
      withFdSocket socket setCloseOnExecIfNeeded
      bind socket $ addrAddress addr
      listen socket 2048
      return socket

    resolve p = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) (Just host) (Just $ show p)

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
