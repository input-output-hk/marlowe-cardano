{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Test.Integration.Marlowe.Local
  ( MarloweRuntime(..)
  , withLocalMarloweRuntime
  ) where

import Cardano.Api
  ( BabbageEra
  , CardanoEra(..)
  , CardanoMode
  , ChainPoint
  , ConsensusModeParams(CardanoModeParams)
  , EpochSlots(EpochSlots)
  , EraInMode(..)
  , LocalNodeConnectInfo(..)
  , NetworkId(..)
  , NetworkMagic(..)
  , QueryInMode
  , Tx
  , TxInMode(..)
  , TxValidationErrorInMode
  , queryNodeLocalState
  )
import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Byron
import Cardano.Api.Shelley (AcquiringFailure)
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Chain.UTxO (defaultUTxOConfiguration)
import Cardano.Crypto (abstractHashToBytes)
import Control.Concurrent.Async (race_)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (bracket, throwIO)
import Control.Monad ((<=<))
import Control.Monad.Trans.Except (runExceptT)
import Data.Either (fromRight)
import Data.String (fromString)
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Void (Void)
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Language.Marlowe.Protocol.HeaderSync.Client (MarloweHeaderSyncClient, marloweHeaderSyncClientPeer)
import Language.Marlowe.Protocol.HeaderSync.Codec (codecMarloweHeaderSync)
import Language.Marlowe.Protocol.HeaderSync.Server (MarloweHeaderSyncServer, marloweHeaderSyncServerPeer)
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient, marloweSyncClientPeer)
import Language.Marlowe.Protocol.Sync.Codec (codecMarloweSync)
import Language.Marlowe.Protocol.Sync.Server (MarloweSyncServer, marloweSyncServerPeer)
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import Language.Marlowe.Runtime.ChainIndexer (ChainIndexerDependencies(..), ChainIndexerSelector, chainIndexer)
import qualified Language.Marlowe.Runtime.ChainIndexer.Database as ChainIndexer
import qualified Language.Marlowe.Runtime.ChainIndexer.Database.PostgreSQL as ChainIndexer
import Language.Marlowe.Runtime.ChainIndexer.Genesis (GenesisBlock, computeByronGenesisBlock)
import Language.Marlowe.Runtime.ChainIndexer.NodeClient (CostModel(CostModel))
import Language.Marlowe.Runtime.ChainSync (ChainSyncDependencies(..), chainSync)
import Language.Marlowe.Runtime.ChainSync.Api
  ( ChainSyncCommand
  , ChainSyncQuery(GetUTxOs)
  , RuntimeChainSeek
  , RuntimeChainSeekClient
  , RuntimeChainSeekServer
  , WithGenesis(Genesis)
  )
import qualified Language.Marlowe.Runtime.ChainSync.Database as ChainSync
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as ChainSync
import Language.Marlowe.Runtime.Discovery
import Language.Marlowe.Runtime.Discovery.Api (DiscoveryQuery)
import Language.Marlowe.Runtime.History
import Language.Marlowe.Runtime.History.Api (HistoryCommand, HistoryQuery)
import Language.Marlowe.Runtime.History.Store (HistoryQueries, hoistHistoryQueries)
import Language.Marlowe.Runtime.History.Store.Memory (mkHistoryQueriesInMemory)
import Language.Marlowe.Runtime.Transaction
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Language.Marlowe.Runtime.Transaction.Query (LoadMarloweContext)
import qualified Language.Marlowe.Runtime.Transaction.Query as Query
import Language.Marlowe.Runtime.Transaction.Server (TransactionServerSelector)
import Language.Marlowe.Runtime.Transaction.Submit (SubmitJob, SubmitJobDependencies(..))
import qualified Language.Marlowe.Runtime.Transaction.Submit as Submit
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import Network.Protocol.ChainSeek.Codec (codecChainSeek)
import Network.Protocol.ChainSeek.Server (chainSeekServerPeer)
import Network.Protocol.Driver
import Network.Protocol.Job.Client (JobClient, jobClientPeer)
import Network.Protocol.Job.Codec (codecJob)
import Network.Protocol.Job.Server (JobServer, jobServerPeer)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Query.Client (QueryClient, liftQuery, queryClientPeer)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Protocol.Query.Server (QueryServer, queryServerPeer)
import Network.Protocol.Query.Types (Query)
import Observe.Event (EventBackend, narrowEventBackend)
import Observe.Event.Backend (noopEventBackend)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult)
import Test.Integration.Cardano
import qualified Test.Integration.Cardano as SpoNode (SpoNode(..))

data MarloweRuntime = MarloweRuntime
  { runDiscoveryQueryClient :: RunClient IO (QueryClient DiscoveryQuery)
  , runDiscoverySyncClient :: RunClient IO MarloweHeaderSyncClient
  , runHistoryJobClient :: RunClient IO (JobClient HistoryCommand)
  , runHistoryQueryClient :: RunClient IO (QueryClient HistoryQuery)
  , runHistorySyncClient :: RunClient IO MarloweSyncClient
  , runTxJobClient :: RunClient IO (JobClient MarloweTxCommand)
  , testnet :: LocalTestnet
  }

withLocalMarloweRuntime :: String -> (MarloweRuntime -> IO ()) -> LocalTestnet -> IO ()
withLocalMarloweRuntime databaseUri test testnet@LocalTestnet{..} = bracket acquirePool Pool.release \pool -> do
  let rootEventBackend = noopEventBackend () -- TODO move Logging code to libs and reuse here to write logs to workspace file.
  Channels{..} <- atomically $ setupChannels rootEventBackend
  historyQueries <- hoistHistoryQueries atomically <$> atomically mkHistoryQueriesInMemory
  let localConsensusModeParams = CardanoModeParams $ EpochSlots 500
  let localNodeNetworkId = Testnet $ NetworkMagic $ fromIntegral testnetMagic
  let localNodeSocketPath = SpoNode.socket . head $ spoNodes
  let localNodeConnectInfo = LocalNodeConnectInfo{..}
  genesisConfigResult <- runExceptT . Byron.readGenesisData $ byronGenesisJson network
  (genesisData, genesisHash) <- case genesisConfigResult of
    Left e -> fail $ show e
    Right a -> pure a
  let
    connectToChainSeek :: RunClient IO RuntimeChainSeekClient
    connectToChainSeek = runChainSeekClient

    byronGenesisConfig = Byron.Config
      genesisData
      genesisHash
      (Byron.toByronRequiresNetworkMagic localNodeNetworkId)
      defaultUTxOConfiguration

    genesisBlock = computeByronGenesisBlock
      (abstractHashToBytes $ Byron.unGenesisHash genesisHash)
      byronGenesisConfig

    chainIndexerDatabaseQueries = ChainIndexer.hoistDatabaseQueries
      (either throwUsageError pure <=< Pool.use pool)
      (ChainIndexer.databaseQueries genesisBlock)
    chainSeekDatabaseQueries = ChainSync.hoistDatabaseQueries
      (either throwUsageError pure <=< Pool.use pool)
      ChainSync.databaseQueries

  let mkSubmitJob = Submit.mkSubmitJob SubmitJobDependencies{..}
  race_ (runComponent_ runtime RuntimeDependencies{..}) $ test MarloweRuntime{..}
  where
    acquirePool = Pool.acquire (100, secondsToNominalDiffTime 5, fromString databaseUri)
    throwUsageError (Pool.ConnectionError err)                       = error $ show err
    throwUsageError (Pool.SessionError (Session.QueryError _ _ err)) = error $ show err

data RuntimeSelector f where
  ChainSeekClientEvent :: ConnectSocketDriverSelector RuntimeChainSeek f -> RuntimeSelector f
  ChainSeekServerEvent :: AcceptSocketDriverSelector RuntimeChainSeek f -> RuntimeSelector f
  ChainSyncJobClientEvent :: ConnectSocketDriverSelector (Job ChainSyncCommand) f -> RuntimeSelector f
  ChainSyncJobServerEvent :: AcceptSocketDriverSelector (Job ChainSyncCommand) f -> RuntimeSelector f
  ChainSyncQueryClientEvent :: ConnectSocketDriverSelector (Query ChainSyncQuery) f -> RuntimeSelector f
  ChainSyncQueryServerEvent :: AcceptSocketDriverSelector (Query ChainSyncQuery) f -> RuntimeSelector f
  DiscoveryQueryClientEvent :: ConnectSocketDriverSelector (Query DiscoveryQuery) f -> RuntimeSelector f
  DiscoveryQueryServerEvent :: AcceptSocketDriverSelector (Query DiscoveryQuery) f -> RuntimeSelector f
  DiscoverySyncClientEvent :: ConnectSocketDriverSelector MarloweHeaderSync f -> RuntimeSelector f
  DiscoverySyncServerEvent :: AcceptSocketDriverSelector MarloweHeaderSync f -> RuntimeSelector f
  HistoryJobClientEvent :: ConnectSocketDriverSelector (Job HistoryCommand) f -> RuntimeSelector f
  HistoryJobServerEvent :: AcceptSocketDriverSelector (Job HistoryCommand) f -> RuntimeSelector f
  HistoryQueryClientEvent :: ConnectSocketDriverSelector (Query HistoryQuery) f -> RuntimeSelector f
  HistoryQueryServerEvent :: AcceptSocketDriverSelector (Query HistoryQuery) f -> RuntimeSelector f
  HistorySyncClientEvent :: ConnectSocketDriverSelector MarloweSync f -> RuntimeSelector f
  HistorySyncServerEvent :: AcceptSocketDriverSelector MarloweSync f -> RuntimeSelector f
  TxJobClientEvent :: ConnectSocketDriverSelector (Job MarloweTxCommand) f -> RuntimeSelector f
  TxJobServerEvent :: AcceptSocketDriverSelector (Job MarloweTxCommand) f -> RuntimeSelector f
  TxEvent :: TransactionServerSelector f -> RuntimeSelector f
  ChainIndexerEvent :: ChainIndexerSelector f -> RuntimeSelector f

data RuntimeDependencies r = RuntimeDependencies
  { acceptRunChainSeekServer :: IO (RunServer IO RuntimeChainSeekServer)
  , acceptRunChainSyncJobServer :: IO (RunServer IO (JobServer ChainSyncCommand))
  , acceptRunChainSyncQueryServer :: IO (RunServer IO (QueryServer ChainSyncQuery))
  , acceptRunDiscoveryQueryServer :: IO (RunServer IO (QueryServer DiscoveryQuery))
  , acceptRunDiscoverySyncServer :: IO (RunServer IO MarloweHeaderSyncServer)
  , acceptRunHistoryJobServer :: IO (RunServer IO (JobServer HistoryCommand))
  , acceptRunHistoryQueryServer :: IO (RunServer IO (QueryServer HistoryQuery))
  , acceptRunHistorySyncServer :: IO (RunServer IO MarloweSyncServer)
  , acceptRunTxJobServer :: IO (RunServer IO (JobServer MarloweTxCommand))
  , chainIndexerDatabaseQueries :: ChainIndexer.DatabaseQueries IO
  , chainSeekDatabaseQueries :: ChainSync.DatabaseQueries IO
  , genesisBlock :: !GenesisBlock
  , historyQueries :: HistoryQueries IO
  , localNodeConnectInfo :: LocalNodeConnectInfo CardanoMode
  , mkSubmitJob :: Tx BabbageEra -> STM SubmitJob
  , rootEventBackend :: EventBackend IO r RuntimeSelector
  , runChainSeekClient :: RunClient IO RuntimeChainSeekClient
  , runChainSyncJobClient :: RunClient IO (JobClient ChainSyncCommand)
  , runChainSyncQueryClient :: RunClient IO (QueryClient ChainSyncQuery)
  , securityParameter :: Int
  }

runtime :: Component IO (RuntimeDependencies r) ()
runtime = proc RuntimeDependencies{..} -> do
  let
    connectToChainSeek :: RunClient IO RuntimeChainSeekClient
    connectToChainSeek = runChainSeekClient

    LocalNodeConnectInfo{..} = localNodeConnectInfo

  chainIndexer -<
    let
      maxCost = 100_000
      costModel = CostModel 1 10
      persistRateLimit = secondsToNominalDiffTime 0.1
      databaseQueries = chainIndexerDatabaseQueries
      eventBackend = narrowEventBackend ChainIndexerEvent rootEventBackend
      connectToLocalNode = Cardano.connectToLocalNode localNodeConnectInfo
     in
      ChainIndexerDependencies{..}

  chainSync -<
    let
      acceptRunJobServer = acceptRunChainSyncJobServer
      acceptRunQueryServer = acceptRunChainSyncQueryServer
      databaseQueries = chainSeekDatabaseQueries

      queryLocalNodeState :: Maybe ChainPoint -> QueryInMode CardanoMode result -> IO (Either AcquiringFailure result)
      queryLocalNodeState = queryNodeLocalState localNodeConnectInfo

      submitTxToNodeLocal :: CardanoEra era -> Tx era -> IO (SubmitResult (TxValidationErrorInMode CardanoMode))
      submitTxToNodeLocal era tx = Cardano.submitTxToNodeLocal localNodeConnectInfo $ TxInMode tx case era of
        ByronEra -> ByronEraInCardanoMode
        ShelleyEra -> ShelleyEraInCardanoMode
        AllegraEra -> AllegraEraInCardanoMode
        MaryEra -> MaryEraInCardanoMode
        AlonzoEra -> AlonzoEraInCardanoMode
        BabbageEra -> BabbageEraInCardanoMode
     in
      ChainSyncDependencies{..}

  history -<
    let
      acceptRunJobServer = acceptRunHistoryJobServer
      acceptRunQueryServer = acceptRunHistoryQueryServer
      acceptRunSyncServer = acceptRunHistorySyncServer
      followerPageSize = 1024

      queryChainSeek :: ChainSyncQuery Void err results -> IO (Either err results)
      queryChainSeek = runChainSyncQueryClient . liftQuery

    in
      HistoryDependencies{..}

  discovery -<
    let
      acceptRunQueryServer = acceptRunDiscoveryQueryServer
      acceptRunSyncServer = acceptRunDiscoverySyncServer
      pageSize = 1024
    in
      DiscoveryDependencies{..}

  transaction -<
    let
      acceptRunTransactionServer = acceptRunTxJobServer

      queryChainSync :: ChainSyncQuery Void err results -> IO results
      queryChainSync = fmap (fromRight $ error "failed to query chain seek server")
        . runChainSyncQueryClient
        . liftQuery

      loadWalletContext = Query.loadWalletContext $ queryChainSync . GetUTxOs

      networkId = localNodeNetworkId

      loadMarloweContext :: LoadMarloweContext r
      loadMarloweContext = Query.loadMarloweContext networkId connectToChainSeek runChainSyncQueryClient

      eventBackend = narrowEventBackend TxEvent rootEventBackend
    in
      TransactionDependencies{..}

data Channels = Channels
  { acceptRunChainSeekServer :: IO (RunServer IO RuntimeChainSeekServer)
  , acceptRunChainSyncJobServer :: IO (RunServer IO (JobServer ChainSyncCommand))
  , acceptRunChainSyncQueryServer :: IO (RunServer IO (QueryServer ChainSyncQuery))
  , acceptRunDiscoveryQueryServer :: IO (RunServer IO (QueryServer DiscoveryQuery))
  , acceptRunDiscoverySyncServer :: IO (RunServer IO MarloweHeaderSyncServer)
  , acceptRunHistoryJobServer :: IO (RunServer IO (JobServer HistoryCommand))
  , acceptRunHistoryQueryServer :: IO (RunServer IO (QueryServer HistoryQuery))
  , acceptRunHistorySyncServer :: IO (RunServer IO MarloweSyncServer)
  , acceptRunTxJobServer :: IO (RunServer IO (JobServer MarloweTxCommand))
  , runChainSeekClient :: RunClient IO RuntimeChainSeekClient
  , runChainSyncJobClient :: RunClient IO (JobClient ChainSyncCommand)
  , runChainSyncQueryClient :: RunClient IO (QueryClient ChainSyncQuery)
  , runDiscoveryQueryClient :: RunClient IO (QueryClient DiscoveryQuery)
  , runDiscoverySyncClient :: RunClient IO MarloweHeaderSyncClient
  , runHistoryJobClient :: RunClient IO (JobClient HistoryCommand)
  , runHistoryQueryClient :: RunClient IO (QueryClient HistoryQuery)
  , runHistorySyncClient :: RunClient IO MarloweSyncClient
  , runTxJobClient :: RunClient IO (JobClient MarloweTxCommand)
  }

setupChannels :: EventBackend IO r RuntimeSelector -> STM Channels
setupChannels eventBackend = do
  ClientServerPair acceptRunChainSeekServer runChainSeekClient <- clientServerPair
    (narrowEventBackend ChainSeekServerEvent eventBackend)
    (narrowEventBackend ChainSeekClientEvent eventBackend)
    throwIO
    codecChainSeek
    (chainSeekServerPeer Genesis)
    (chainSeekClientPeer Genesis)
  ClientServerPair acceptRunChainSyncJobServer runChainSyncJobClient <- clientServerPair
    (narrowEventBackend ChainSyncJobServerEvent eventBackend)
    (narrowEventBackend ChainSyncJobClientEvent eventBackend)
    throwIO
    codecJob
    jobServerPeer
    jobClientPeer
  ClientServerPair acceptRunChainSyncQueryServer runChainSyncQueryClient <- clientServerPair
    (narrowEventBackend ChainSyncQueryServerEvent eventBackend)
    (narrowEventBackend ChainSyncQueryClientEvent eventBackend)
    throwIO
    codecQuery
    queryServerPeer
    queryClientPeer
  ClientServerPair acceptRunDiscoveryQueryServer runDiscoveryQueryClient <- clientServerPair
    (narrowEventBackend DiscoveryQueryServerEvent eventBackend)
    (narrowEventBackend DiscoveryQueryClientEvent eventBackend)
    throwIO
    codecQuery
    queryServerPeer
    queryClientPeer
  ClientServerPair acceptRunDiscoverySyncServer runDiscoverySyncClient <- clientServerPair
    (narrowEventBackend DiscoverySyncServerEvent eventBackend)
    (narrowEventBackend DiscoverySyncClientEvent eventBackend)
    throwIO
    codecMarloweHeaderSync
    marloweHeaderSyncServerPeer
    marloweHeaderSyncClientPeer
  ClientServerPair acceptRunHistoryJobServer runHistoryJobClient <- clientServerPair
    (narrowEventBackend HistoryJobServerEvent eventBackend)
    (narrowEventBackend HistoryJobClientEvent eventBackend)
    throwIO
    codecJob
    jobServerPeer
    jobClientPeer
  ClientServerPair acceptRunHistoryQueryServer runHistoryQueryClient <- clientServerPair
    (narrowEventBackend HistoryQueryServerEvent eventBackend)
    (narrowEventBackend HistoryQueryClientEvent eventBackend)
    throwIO
    codecQuery
    queryServerPeer
    queryClientPeer
  ClientServerPair acceptRunHistorySyncServer runHistorySyncClient <- clientServerPair
    (narrowEventBackend HistorySyncServerEvent eventBackend)
    (narrowEventBackend HistorySyncClientEvent eventBackend)
    throwIO
    codecMarloweSync
    marloweSyncServerPeer
    marloweSyncClientPeer
  ClientServerPair acceptRunTxJobServer runTxJobClient <- clientServerPair
    (narrowEventBackend TxJobServerEvent eventBackend)
    (narrowEventBackend TxJobClientEvent eventBackend)
    throwIO
    codecJob
    jobServerPeer
    jobClientPeer
  pure Channels{..}
