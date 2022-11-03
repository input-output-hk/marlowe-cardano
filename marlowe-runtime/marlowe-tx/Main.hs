{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Main
  where

import Cardano.Api (NetworkId(Mainnet))
import Colog (logDebug, logError)
import qualified Colog
import Control.Concurrent.STM (atomically)
import Control.Exception.Base
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Stack (HasCallStack)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient, marloweSyncClientPeer)
import Language.Marlowe.Protocol.Sync.Codec (codecMarloweSync)
import Language.Marlowe.Runtime.CLI.Option.Colog (Verbosity(LogLevel), logActionParser)
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
import qualified Language.Marlowe.Runtime.ChainSync.Api as ChainSync
import Language.Marlowe.Runtime.Logging.Colog (prefixLogger)
import Language.Marlowe.Runtime.Logging.Colog.LogIO (LogIO, dieLogIO, runLogIO)
import Language.Marlowe.Runtime.Logging.Colog.LogIO.Network
  (ProtocolName(ProtocolName), clientPrefix, withClientSocket, withServerSocket)
import Language.Marlowe.Runtime.Transaction.Constraints (SolveConstraints)
import qualified Language.Marlowe.Runtime.Transaction.Constraints as Constraints
import Language.Marlowe.Runtime.Transaction.Query (LoadMarloweContext, LoadWalletContext)
import qualified Language.Marlowe.Runtime.Transaction.Query as Query
import Language.Marlowe.Runtime.Transaction.Server
  (RunTransactionServer(..), TransactionServer(..), TransactionServerDependencies(..), mkTransactionServer)
import qualified Language.Marlowe.Runtime.Transaction.Submit as Submit
import Network.Channel (Channel, hoistChannel, socketAsChannel)
import Network.Protocol.Driver (hoistDriver, mkDriver)
import Network.Protocol.Job.Client (JobClient, jobClientPeer)
import qualified Network.Protocol.Job.Client as Job.Client
import Network.Protocol.Job.Codec (codecJob)
import Network.Protocol.Job.Server (jobServerPeer)
import Network.Protocol.Query.Client (liftQuery, queryClientPeer)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Socket
  ( AddrInfo(..)
  , AddrInfoFlag(..)
  , HostName
  , PortNumber
  , SockAddr
  , SocketType(..)
  , accept
  , defaultHints
  , getAddrInfo
  , withSocketsDo
  )
import Network.TypedProtocol (runPeerWithDriver, startDState)
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
run Options{logAction=mainLogAction, ..} = withSocketsDo do
  addr <- resolve port
  Colog.withBackgroundLogger Colog.defCapacity mainLogAction \logAction -> do
    runLogIO logAction $ withServerSocket (ProtocolName "Marlowe Tx") addr \socket -> do
      {- Setup Dependencies -}
      let
        acceptRunTransactionServer = do
          (conn, _ :: SockAddr) <- liftIO $ accept socket

          let
            chann :: Channel LogIO LB.ByteString
            chann = hoistChannel liftIO $ socketAsChannel conn
            driver = mkDriver (liftIO . throwIO) codecJob chann
          pure $ RunTransactionServer \server -> do
            let peer = jobServerPeer server
            fst <$> runPeerWithDriver driver peer (startDState driver)

        runHistorySyncClient :: MarloweSyncClient LogIO a -> LogIO a
        runHistorySyncClient client = do
          historySyncAddr <- liftIO $ head <$> getAddrInfo (Just clientHints) (Just historyHost) (Just $ show historySyncPort)
          withClientSocket (ProtocolName "HistorySync") historySyncAddr \historySyncSocket -> do
            let driver = hoistDriver liftIO $ mkDriver throwIO codecMarloweSync $ socketAsChannel historySyncSocket
            let peer = marloweSyncClientPeer client
            fst <$> runPeerWithDriver driver peer (startDState driver)

        connectToChainSeek :: RuntimeChainSeekClient LogIO a -> LogIO a
        connectToChainSeek client = do
          chainSeekAddr <- liftIO $ head <$> getAddrInfo (Just clientHints) (Just chainSeekHost) (Just $ show chainSeekPort)
          withClientSocket (ProtocolName "ChainSeek") chainSeekAddr \chainSeekSocket -> do
            let driver = hoistDriver liftIO (mkDriver throwIO runtimeChainSeekCodec $ socketAsChannel chainSeekSocket)
            let peer = chainSeekClientPeer Genesis client
            fst <$> runPeerWithDriver driver peer (startDState driver)

        runChainSyncJobClient :: JobClient ChainSyncCommand LogIO a -> LogIO a
        runChainSyncJobClient client = do
          chainSeekAddr <- liftIO $ head <$> getAddrInfo (Just clientHints) (Just chainSeekHost) (Just $ show chainSeekCommandPort)
          withClientSocket (ProtocolName "ChainSyncJob") chainSeekAddr \chainSeekSocket -> do
            let driver = hoistDriver liftIO $ mkDriver throwIO codecJob $ socketAsChannel chainSeekSocket
            let peer = jobClientPeer client
            fst <$> runPeerWithDriver driver peer (startDState driver)

      let
        mkSubmitJob = Submit.mkSubmitJob Submit.SubmitJobDependencies{..}
        queryChainSync' s q = queryChainSync s q >>= \case
          Just res -> pure res
          Nothing -> error "Initialization failed on chain sync query"

      systemStart <- queryChainSync' show GetSystemStart
      eraHistory <- queryChainSync' (const "EraHistory") GetEraHistory
      protocolParameters <- queryChainSync' show GetProtocolParameters
      slotConfig <- queryChainSync' show GetSlotConfig
      networkId <- queryChainSync' show GetNetworkId
      when (networkId == Mainnet) do
        dieLogIO "Mainnet support is currently disabled."
      let
        solveConstraints :: SolveConstraints
        solveConstraints = Constraints.solveConstraints
          systemStart
          eraHistory
          protocolParameters

      let
        loadMarloweContext :: LoadMarloweContext
        loadMarloweContext = Query.loadMarloweContext networkId runHistorySyncClient

        loadWalletContext :: LoadWalletContext
        loadWalletContext = Query.loadWalletContext runGetUTxOsQuery

      TransactionServer{..} <- liftIO $ atomically do
        mkTransactionServer TransactionServerDependencies{..}

      runChainSyncJobClient (Job.Client.doHandshake ChainSync.commandSchema)
      {- Run the server -}
      runTransactionServer
  where
    resolve p = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) (Just host) (Just $ show p)

    queryChainSync :: Show e => (a -> String) -> ChainSyncQuery Void e a -> LogIO (Maybe a)
    queryChainSync showResult query = do
      addr <- liftIO $ head <$> getAddrInfo (Just clientHints) (Just chainSeekHost) (Just $ show chainSeekQueryPort)
      let pn = ProtocolName "Chain Sync Query"
      withClientSocket pn addr \socket -> prefixLogger (T.pack $ clientPrefix pn addr) do
        let driver = hoistDriver liftIO $ mkDriver throwIO codecQuery $ socketAsChannel socket
        let client = liftQuery ChainSync.querySchema (pure query)
        let peer = queryClientPeer client
        result <- fst <$> runPeerWithDriver driver peer (startDState driver)
        case result of
          Right r -> do
            logDebug . T.pack $ "Query result:" <> showResult r
            pure $ Just r
          Left (Left _) -> do
            logError "Chain Sync Query handshake error.."
            pure Nothing
          Left (Right err) -> do
            logError . T.pack . mappend "Chain Sync Query error..." $ show err
            pure Nothing

    runGetUTxOsQuery :: HasCallStack => GetUTxOsQuery -> LogIO (Maybe UTxOs)
    runGetUTxOsQuery getUTxOsQuery = do
      logDebug . T.pack $ show getUTxOsQuery
      queryChainSync show (GetUTxOs getUTxOsQuery)

data Options = Options
  { chainSeekPort      :: PortNumber
  , chainSeekQueryPort :: PortNumber
  , chainSeekCommandPort :: PortNumber
  , chainSeekHost      :: HostName
  , port               :: PortNumber
  , host               :: HostName
  , historySyncPort :: PortNumber
  , historyHost :: HostName
  , logAction :: Colog.LogAction IO Colog.Message
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
      <*> logActionParser (LogLevel Colog.Error)

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
