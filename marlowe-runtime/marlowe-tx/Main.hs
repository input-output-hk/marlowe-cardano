{-# LANGUAGE GADTs #-}

module Main
  where

import qualified Colog
import Control.Concurrent.Component
import Control.Exception (bracket, bracketOnError, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Either (fromRight)
import Data.Void (Void)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient, marloweSyncClientPeer)
import Language.Marlowe.Protocol.Sync.Codec (codecMarloweSync)
import Language.Marlowe.Runtime.CLI.Option (Verbosity(LogLevel, Silent), verbosityParser)
import Language.Marlowe.Runtime.ChainSync.Api
  ( ChainSyncCommand
  , ChainSyncQuery(..)
  , GetUTxOsQuery
  , RuntimeChainSeekClient
  , UTxOs
  , WithGenesis(..)
  , chainSeekClientPeer
  , runtimeChainSeekCodec
  )
import Language.Marlowe.Runtime.Logging (mkLogger)
import Language.Marlowe.Runtime.Transaction (TransactionDependencies(..), transaction)
import Language.Marlowe.Runtime.Transaction.Query (LoadMarloweContext, LoadWalletContext)
import qualified Language.Marlowe.Runtime.Transaction.Query as Query
import qualified Language.Marlowe.Runtime.Transaction.Submit as Submit
import Network.Protocol.Driver (acceptRunServerPeerOverSocket, runClientPeerOverSocket)
import Network.Protocol.Job.Client (JobClient, jobClientPeer)
import Network.Protocol.Job.Codec (codecJob)
import Network.Protocol.Job.Server (jobServerPeer)
import Network.Protocol.Query.Client (liftQuery, queryClientPeer)
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

main :: IO ()
main = run =<< getOptions

clientHints :: AddrInfo
clientHints = defaultHints { addrSocketType = Stream }

run :: Options -> IO ()
run Options{..} = withSocketsDo do
  addr <- resolve port

  let
    mainLogAction :: Colog.LogAction IO Colog.Message
    mainLogAction = mkLogger $ case verbosity of
      Silent -> Nothing
      LogLevel severity -> Just severity

  bracket (openServer addr) close \socket -> do
    Colog.withBackgroundLogger Colog.defCapacity mainLogAction \logAction -> do
      {- Setup Dependencies -}
      let
        acceptRunTransactionServer = acceptRunServerPeerOverSocket (liftIO . throwIO) socket codecJob jobServerPeer

        runHistorySyncClient :: MarloweSyncClient IO a -> IO a
        runHistorySyncClient client = do
          addr' <- head <$> getAddrInfo (Just clientHints) (Just historyHost) (Just $ show historySyncPort)
          runClientPeerOverSocket throwIO addr' codecMarloweSync marloweSyncClientPeer client

        connectToChainSeek :: RuntimeChainSeekClient IO a -> IO a
        connectToChainSeek client = do
          addr' <- head <$> getAddrInfo (Just clientHints) (Just chainSeekHost) (Just $ show chainSeekPort)
          runClientPeerOverSocket throwIO addr' runtimeChainSeekCodec (chainSeekClientPeer Genesis) client

        runChainSyncJobClient :: JobClient ChainSyncCommand IO a -> IO a
        runChainSyncJobClient client = do
          addr' <- head <$> getAddrInfo (Just clientHints) (Just chainSeekHost) (Just $ show chainSeekCommandPort)
          runClientPeerOverSocket throwIO addr' codecJob jobClientPeer client

      let mkSubmitJob = Submit.mkSubmitJob Submit.SubmitJobDependencies{..}
      let
        loadMarloweContext :: LoadMarloweContext
        loadMarloweContext version contractId = do
          networkId <- queryChainSync GetNetworkId
          Query.loadMarloweContext networkId runHistorySyncClient version contractId

        loadWalletContext :: LoadWalletContext
        loadWalletContext = Query.loadWalletContext runGetUTxOsQuery

      runComponent_ transaction TransactionDependencies{..}
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

    queryChainSync :: ChainSyncQuery Void e a -> IO a
    queryChainSync query = do
      addr <- head <$> getAddrInfo (Just clientHints) (Just chainSeekHost) (Just $ show chainSeekQueryPort)
      result <- runClientPeerOverSocket throwIO addr codecQuery queryClientPeer $ liftQuery query
      pure $ fromRight (error "failed to query chain seek server") result

    runGetUTxOsQuery :: GetUTxOsQuery -> IO UTxOs
    runGetUTxOsQuery getUTxOsQuery = queryChainSync (GetUTxOs getUTxOsQuery)

data Options = Options
  { chainSeekPort      :: PortNumber
  , chainSeekQueryPort :: PortNumber
  , chainSeekCommandPort :: PortNumber
  , chainSeekHost      :: HostName
  , port               :: PortNumber
  , host               :: HostName
  , historySyncPort :: PortNumber
  , historyHost :: HostName
  , verbosity  :: Verbosity
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
      <*> historySyncPortParser
      <*> historyHostParser
      <*> verbosityParser (LogLevel Colog.Error)

    chainSeekPortParser = option auto $ mconcat
      [ long "chain-seek-port-number"
      , value 3715
      , metavar "PORT_NUMBER"
      , help "The port number of the chain seek server."
      , showDefault
      ]

    chainSeekQueryPortParser = option auto $ mconcat
      [ long "chain-seek-query-port-number"
      , value 3716
      , metavar "PORT_NUMBER"
      , help "The port number of the chain sync query server."
      , showDefault
      ]

    chainSeekCommandPortParser = option auto $ mconcat
      [ long "chain-seek-command-port-number"
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
      [ long "chain-seek-host"
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name of the chain seek server."
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

    historySyncPortParser = option auto $ mconcat
      [ long "history-sync-port"
      , value 3719
      , metavar "PORT_NUMBER"
      , help "The port number of the history sync server."
      , showDefault
      ]

    historyHostParser = strOption $ mconcat
      [ long "history-host"
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name of the history server."
      , showDefault
      ]

    infoMod = mconcat
      [ fullDesc
      , progDesc "Marlowe runtime transaction creation server"
      , header "marlowe-tx : the transaction creation server of the Marlowe Runtime"
      ]
