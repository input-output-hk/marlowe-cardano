{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Marlowe.Local
  ( MarloweRuntime(..)
  , RuntimeRef
  , RuntimeSelector
  , module Test.Integration.Cardano
  , Test.Integration.Cardano.exec
  , defaultMarloweRuntimeOptions
  , withLocalMarloweRuntime
  , withLocalMarloweRuntime'
  ) where

import Cardano.Api
  ( AsType(..)
  , BabbageEra
  , CardanoEra(..)
  , CardanoMode
  , ChainPoint
  , ConsensusModeParams(..)
  , EpochSlots(..)
  , EraInMode(..)
  , LocalNodeConnectInfo(..)
  , NetworkId(..)
  , NetworkMagic(..)
  , QueryInMode
  , ScriptDataSupportedInEra(ScriptDataInBabbageEra)
  , StakeAddressReference(..)
  , Tx
  , TxBody
  , TxInMode(..)
  , TxValidationErrorInMode
  , deserialiseFromBech32
  , deserialiseFromTextEnvelope
  , getTxId
  , queryNodeLocalState
  , shelleyAddressInEra
  )
import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Byron
import Cardano.Api.Shelley (AcquiringFailure)
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Chain.UTxO (defaultUTxOConfiguration)
import Cardano.Crypto (abstractHashToBytes)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM)
import Control.Exception (bracketOnError, catch, onException, throw, try)
import Control.Monad (when, (<=<))
import Control.Monad.Catch hiding (bracketOnError, catch, onException, try)
import Control.Monad.Event.Class
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Marlowe (MarloweTracedContext(MarloweTracedContext))
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Control.Monad.Trans.Resource (ResourceT, allocate, runResourceT, unprotect)
import Control.Monad.With
import Data.Aeson (eitherDecodeFileStrict)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Functor (void)
import Data.GeneralAllocate
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import Data.String (fromString)
import qualified Data.Text as T
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Units (Second)
import Data.Word (Word16)
import Database.PostgreSQL.LibPQ (connectdb, errorMessage, exec, finish, resultErrorMessage)
import Hasql.Connection (settings)
import qualified Hasql.Pool as Pool
import Language.Marlowe.CLI.Transaction (buildPublishingImpl, submitBody)
import Language.Marlowe.CLI.Types
  ( CliEnv(..)
  , MarlowePlutusVersion
  , PrintStats(..)
  , PublishMarloweScripts(..)
  , PublishScript(..)
  , PublishingStrategy(..)
  , ValidatorInfo(..)
  , defaultCoinSelectionStrategy
  )
import Language.Marlowe.Protocol.Client (MarloweRuntimeClient, hoistMarloweRuntimeClient, marloweRuntimeClientPeer)
import Language.Marlowe.Protocol.HeaderSync.Client (MarloweHeaderSyncClient, marloweHeaderSyncClientPeer)
import Language.Marlowe.Protocol.HeaderSync.Server (MarloweHeaderSyncServer, marloweHeaderSyncServerPeer)
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import Language.Marlowe.Protocol.Load.Client (MarloweLoadClient, marloweLoadClientPeer)
import Language.Marlowe.Protocol.Load.Server (MarloweLoadServer, marloweLoadServerPeer)
import Language.Marlowe.Protocol.Load.Types (MarloweLoad)
import Language.Marlowe.Protocol.Query.Client (MarloweQueryClient)
import Language.Marlowe.Protocol.Query.Server (MarloweQueryServer)
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import Language.Marlowe.Protocol.Server (MarloweRuntimeServer, marloweRuntimeServerPeer)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient, marloweSyncClientPeer)
import Language.Marlowe.Protocol.Sync.Server (MarloweSyncServer, marloweSyncServerPeer)
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import qualified Language.Marlowe.Protocol.Types as Protocol
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoAddressInEra, fromCardanoLovelace, fromCardanoTxId)
import Language.Marlowe.Runtime.ChainIndexer (ChainIndexerDependencies(..), chainIndexer)
import Language.Marlowe.Runtime.ChainIndexer.Database (CommitGenesisBlock(..), DatabaseQueries(..))
import qualified Language.Marlowe.Runtime.ChainIndexer.Database as ChainIndexer
import qualified Language.Marlowe.Runtime.ChainIndexer.Database.PostgreSQL as ChainIndexer
import Language.Marlowe.Runtime.ChainIndexer.Genesis (GenesisBlock, computeGenesisBlock)
import Language.Marlowe.Runtime.ChainIndexer.NodeClient (CostModel(CostModel))
import Language.Marlowe.Runtime.ChainSync (ChainSyncDependencies(..), chainSync)
import Language.Marlowe.Runtime.ChainSync.Api
  ( Assets(..)
  , BlockNo(..)
  , ChainSyncCommand
  , ChainSyncQuery(..)
  , RuntimeChainSeek
  , RuntimeChainSeekClient
  , RuntimeChainSeekServer
  , TransactionOutput(..)
  , TxOutRef(TxOutRef)
  , fromCardanoScriptHash
  )
import qualified Language.Marlowe.Runtime.ChainSync.Database as ChainSync
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as ChainSync
import Language.Marlowe.Runtime.Contract (ContractDependencies(..), contract)
import Language.Marlowe.Runtime.Contract.Api (ContractRequest)
import Language.Marlowe.Runtime.Contract.Store (ContractStore)
import Language.Marlowe.Runtime.Contract.Store.File (ContractStoreOptions(..), createContractStore)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..))
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts(..), ReferenceScriptUtxo(..))
import qualified Language.Marlowe.Runtime.Core.ScriptRegistry as ScriptRegistry
import Language.Marlowe.Runtime.Indexer (MarloweIndexerDependencies(..), marloweIndexer)
import qualified Language.Marlowe.Runtime.Indexer.Database as Indexer
import qualified Language.Marlowe.Runtime.Indexer.Database.PostgreSQL as IndexerDB
import Language.Marlowe.Runtime.Proxy (ProxyDependencies(..), Router(..), proxy)
import Language.Marlowe.Runtime.Sync (SyncDependencies(..), sync)
import qualified Language.Marlowe.Runtime.Sync.Database as Sync
import qualified Language.Marlowe.Runtime.Sync.Database.PostgreSQL as Sync
import Language.Marlowe.Runtime.Transaction
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Language.Marlowe.Runtime.Transaction.Query (LoadMarloweContext)
import qualified Language.Marlowe.Runtime.Transaction.Query as Query
import Language.Marlowe.Runtime.Transaction.Submit (SubmitJob, SubmitJobDependencies(..))
import qualified Language.Marlowe.Runtime.Transaction.Submit as Submit
import Language.Marlowe.Runtime.Web.Client (healthcheck)
import Language.Marlowe.Runtime.Web.Server (ServerDependencies(..), ServerSelector(RuntimeClient), server)
import Language.Marlowe.Runtime.Web.Server.Monad (BackendM(runBackendM))
import Network.Channel (hoistChannel)
import Network.Channel.Typed (Channel, driverToChannel)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import Network.Protocol.ChainSeek.Server (chainSeekServerPeer)
import Network.Protocol.Codec (BinaryMessage)
import Network.Protocol.Connection
import qualified Network.Protocol.Connection as Connection
import Network.Protocol.Driver
import Network.Protocol.Driver.Trace (HasSpanContext(..), mkDriverTraced, runSomeConnectorTraced)
import Network.Protocol.Handshake.Server (handshakeClientServerPair, handshakeConnectionSource)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Job.Client (JobClient, jobClientPeer)
import Network.Protocol.Job.Server (JobServer, jobServerPeer)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Peer.Trace (TypedProtocolsSelector, defaultSpanContext)
import Network.Protocol.Query.Client (QueryClient, queryClientPeer, request)
import Network.Protocol.Query.Server (QueryServer, queryServerPeer)
import Network.Protocol.Query.Types (Query)
import Network.Socket
  ( AddrInfo(..)
  , SocketOption(ReuseAddr)
  , SocketType(..)
  , bind
  , close
  , defaultHints
  , getAddrInfo
  , openSocket
  , setCloseOnExecIfNeeded
  , setSocketOption
  , withFdSocket
  )
import Network.TypedProtocol (unsafeIntToNat)
import Network.Wai.Handler.Warp (run)
import Observe.Event.Backend (noopEventBackend)
import Observe.Event.Explicit (injectSelector)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult)
import Servant.Client (BaseUrl(..), ClientError, ClientM, Scheme(..), mkClientEnv, runClientM)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.IO (BufferMode(..), IOMode(..), hSetBuffering)
import System.Process (readCreateProcessWithExitCode)
import qualified System.Process as SP
import System.Random (randomRIO)
import Test.Integration.Cardano hiding (exec)
import qualified Test.Integration.Cardano (exec)
import qualified Test.Integration.Cardano as SpoNode (SpoNode(..))
import Text.Read (readMaybe)
import UnliftIO (MonadIO, MonadUnliftIO, atomically, throwIO, withRunInIO)

data RuntimeRef = RuntimeRef

instance Semigroup RuntimeRef where
  (<>) = const

instance Monoid RuntimeRef where
  mempty = RuntimeRef

instance HasSpanContext RuntimeRef where
  context _ = pure defaultSpanContext
  wrapContext _ = RuntimeRef

data MarloweRuntime = MarloweRuntime
  { protocolConnector :: ClientConnectorTraced
      (Handshake Protocol.MarloweRuntime)
      MarloweRuntimeClient
      RuntimeRef
      STMConnectorSelector
      (NoopEventT RuntimeRef RuntimeSelector IO)
  , proxyPort :: Int
  , runWebClient :: forall a. ClientM a -> IO (Either ClientError a)
  , marloweScripts :: MarloweScripts
  , testnet :: LocalTestnet
  }

data MarloweRuntimeOptions = MarloweRuntimeOptions
  { databaseHost :: ByteString
  , databasePort :: Word16
  , databaseUser :: ByteString
  , databasePassword :: ByteString
  , tempDatabase :: ByteString
  , cleanup :: Bool
  , submitConfirmationBlocks :: BlockNo
  , localTestnetOptions :: LocalTestnetOptions
  }

defaultMarloweRuntimeOptions :: IO MarloweRuntimeOptions
defaultMarloweRuntimeOptions = do
  databaseHost <- lookupEnv "MARLOWE_RT_TEST_DB_HOST"
  databasePort <- lookupEnv "MARLOWE_RT_TEST_DB_PORT"
  databaseUser <- lookupEnv "MARLOWE_RT_TEST_DB_USER"
  databasePassword <- lookupEnv "MARLOWE_RT_TEST_DB_PASSWORD"
  tempDatabase <- lookupEnv "MARLOWE_RT_TEST_TEMP_DB"
  cleanupDatabase <- lookupEnv "MARLOWE_RT_TEST_CLEANUP_DATABASE"
  submitConfirmationBlocks <- lookupEnv "MARLOWE_RT_TEST_SUBMIT_CONFIRMATION_BLOCKS"
  pure $ MarloweRuntimeOptions
    (maybe "127.0.0.1" fromString databaseHost)
    (fromMaybe 5432 $ readMaybe =<< databasePort)
    (maybe "postgres" fromString databaseUser)
    (maybe "" fromString databasePassword)
    (maybe "template1" fromString tempDatabase)
    (fromMaybe True $ readMaybe =<< cleanupDatabase)
    (BlockNo $ fromMaybe 2 $ readMaybe =<< submitConfirmationBlocks)
    defaultOptions

withLocalMarloweRuntime :: MonadUnliftIO m => (MarloweRuntime -> m ()) -> m ()
withLocalMarloweRuntime test = do
  options <- liftIO defaultMarloweRuntimeOptions
  withLocalMarloweRuntime' options test

withLocalMarloweRuntime' :: MonadUnliftIO m => MarloweRuntimeOptions -> (MarloweRuntime -> m ()) -> m ()
withLocalMarloweRuntime' MarloweRuntimeOptions{..} test = withRunInIO \runInIO ->
  withLocalTestnet' localTestnetOptions \testnet@LocalTestnet{..} -> runResourceT do
    let localConsensusModeParams = CardanoModeParams $ EpochSlots 500
    let localNodeNetworkId = Testnet $ NetworkMagic $ fromIntegral testnetMagic
    let localNodeSocketPath = SpoNode.socket . head $ spoNodes
    let localNodeConnectInfo = LocalNodeConnectInfo{..}
    marloweScripts <- liftIO $ publishCurrentScripts testnet localNodeConnectInfo
    (dbReleaseKey, dbName) <- allocate (createDatabase workspace) (cleanupDatabase 10_000)
    liftIO $ migrateDatabase dbName
    let connectionString = settings databaseHost databasePort databaseUser databasePassword dbName
    let acquirePool = Pool.acquire 100 (Just 5000000) connectionString
    (_, pool) <- allocate acquirePool Pool.release
    logFileHandle <- openWorkspaceFile workspace "logs/runtime.log" WriteMode
    liftIO $ hSetBuffering logFileHandle LineBuffering
    Channels{..} <- atomically setupChannels
    genesisConfigResult <- runExceptT . Byron.readGenesisData $ byronGenesisJson network
    (genesisData, genesisHash) <- case genesisConfigResult of
      Left e -> fail $ show e
      Right a -> pure a
    shelleyGenesisConfig <- liftIO $ either error id <$> eitherDecodeFileStrict (shelleyGenesisJson network)
    let
      byronGenesisConfig = Byron.Config
        genesisData
        genesisHash
        (Byron.toByronRequiresNetworkMagic localNodeNetworkId)
        defaultUTxOConfiguration

      genesisBlock = computeGenesisBlock
        (abstractHashToBytes $ Byron.unGenesisHash genesisHash)
        byronGenesisConfig
        shelleyGenesisConfig

      chainIndexerDatabaseQueries = ChainIndexer.databaseQueries pool genesisBlock

      chainSeekDatabaseQueries = ChainSync.databaseQueries pool localNodeNetworkId

      marloweIndexerDatabaseQueries = IndexerDB.databaseQueries pool securityParameter

      marloweSyncDatabaseQueries = Sync.logDatabaseQueries $ Sync.hoistDatabaseQueries
        (either throwIO pure <=< liftIO . Pool.use pool)
        Sync.databaseQueries

    webPort <- liftIO $ randomPort 4000 4999
    proxyPort <- liftIO $ randomPort 5000 5999
    manager <- liftIO $ newManager defaultManagerSettings

    contractStore <- createContractStore ContractStoreOptions
      { contractStoreDirectory = resolveWorkspacePath workspace "contract-store"
      , contractStoreStagingDirectory = resolveWorkspacePath workspace "contract-staging-area"
      , lockingMicrosecondsBetweenRetries = 100_000
      }

    let chainSyncConnector = SomeConnectorTraced inject $ clientConnector chainSyncPair
    let chainSyncJobConnector = SomeConnectorTraced inject $ clientConnector chainSyncJobPair
    let mkSubmitJob = Submit.mkSubmitJob SubmitJobDependencies{..}
    let baseUrl = BaseUrl Http "localhost" webPort ""
    let clientEnv = mkClientEnv manager baseUrl
    let
      runWebClient :: ClientM a -> IO (Either ClientError a)
      runWebClient = flip runClientM clientEnv
    let
      waitForWebServer :: Int -> IO ()
      waitForWebServer counter
        | counter < 10 = void $ runWebClient do
            result <- healthcheck
            if result then pure ()
            else liftIO $ threadDelay 1000 *> waitForWebServer (counter + 1)
        | otherwise = fail "Unable to connect to web server"

    let
      protocolConnector = ihoistConnectorTraced
        hoistMarloweRuntimeClient
        (NoopEventT . runRuntimeM @RuntimeRef . runResourceT)
        (liftIO . runNoopEventT)
        (clientConnector marloweRuntimePair)

    -- Persist the genesis block before starting the services so that they
    -- exist already and no database queries fail.
    liftIO do
      runRuntimeM @RuntimeRef
        $ runCommitGenesisBlock (commitGenesisBlock chainIndexerDatabaseQueries) genesisBlock
      onException
        ( runRuntimeM (runComponent_  runtime RuntimeDependencies{..})
          `race_` (waitForWebServer 0 *> runInIO (test MarloweRuntime{..}))
        )
        (unprotect dbReleaseKey)
  where
    rootConnectionString = settings databaseHost databasePort databaseUser databasePassword tempDatabase

    checkResult connection = \case
      Nothing -> do
        msg <- errorMessage connection
        fail $ "Fatal database error: " <> show msg
      Just r -> do
        resultErrorMessage r >>= \case
          Nothing -> pure ()
          Just "" -> pure ()
          Just msg -> fail $ "Error creating database: " <> show msg

    createDatabase workspace = do
      connection <- connectdb rootConnectionString
      let dbName = fromString $ "chain_test_" <> show (workspaceId workspace)
      result1 <- exec connection $ "CREATE DATABASE \"" <> dbName <> "\"" <> ";"
      result2 <- exec connection $ "GRANT ALL PRIVILEGES ON DATABASE \"" <> dbName <> "\" TO " <> databaseUser <> ";"
      checkResult connection result1
      checkResult connection result2
      finish connection
      pure dbName

    cleanupDatabase retryDelay dbName = when cleanup do
      catch
        ( do
          connection <- connectdb rootConnectionString
          result <- exec connection $ "DROP DATABASE \"" <> dbName <> "\";"
          checkResult connection result
          finish connection
        )
        ( \(SomeException e) -> if retryDelay > 1_000_000
            then throw e
            else do
              threadDelay retryDelay
              cleanupDatabase (retryDelay * 10) dbName
        )

    migrateDatabase dbName = do
      (exitCode, _, stderr) <- flip readCreateProcessWithExitCode "" $ (SP.proc "sqitch"
        [ "deploy"
        , "-h", BS.unpack databaseHost
        , "-p", show databasePort
        , "-u", BS.unpack databaseUser
        , "-d", BS.unpack dbName
        ])
        { SP.cwd = Just "./marlowe-chain-sync"
        }
      case exitCode of
        ExitFailure _ -> fail $ "chain sqitch failed: \n" <> stderr
        ExitSuccess -> do
          (exitCode', _, stderr') <- flip readCreateProcessWithExitCode "" $ (SP.proc "sqitch"
            [ "deploy"
            , "-h", BS.unpack databaseHost
            , "-p", show databasePort
            , "-u", BS.unpack databaseUser
            , "-d", BS.unpack dbName
            ])
            { SP.cwd = Just "./marlowe-runtime/marlowe-indexer"
            }
          case exitCode' of
            ExitFailure _ -> fail $ "marlowe sqitch failed: \n" <> stderr'
            ExitSuccess -> pure ()

randomPort :: Int -> Int -> IO Int
randomPort lo hi = do
  candidate <- randomRIO (lo, hi)
  result <- try @SomeException do
    let hints = defaultHints { addrSocketType = Stream }
    addr <- head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just $ show candidate)
    close =<< open addr
  case result of
    Left _ -> randomPort lo hi
    _ -> pure candidate
  where
    open addr = bracketOnError (openSocket addr) close \socket -> do
      setSocketOption socket ReuseAddr 1
      withFdSocket socket setCloseOnExecIfNeeded
      bind socket $ addrAddress addr
      pure socket


publishCurrentScripts :: LocalTestnet -> LocalNodeConnectInfo CardanoMode -> IO MarloweScripts
publishCurrentScripts LocalTestnet{..} localNodeConnectInfo = do
  let Delegator{..} = head delegators
  let PaymentKeyPair{..} = paymentKeyPair
  let StakingKeyPair{..} = stakingKeyPair
  signingKey <- Left
      . either (error . show) id
      . deserialiseFromTextEnvelope (AsSigningKey AsPaymentKey)
      . either error id
    <$> eitherDecodeFileStrict paymentSKey
  changeAddress <-
    either (error . show) shelleyAddressInEra
        . deserialiseFromBech32 (AsAddress AsShelleyAddr)
        . T.pack
      <$> execCli
        [ "address", "build"
        , "--payment-verification-key-file", paymentVKey
        , "--stake-verification-key-file", stakingVKey
        , "--testnet-magic", show testnetMagic
        ]
  let publishingStrategy = PublishPermanently NoStakeAddress
  let coinSelectionStrategy = defaultCoinSelectionStrategy
  either throwIO pure =<< runExceptT do
    flip runReaderT (CliEnv ScriptDataInBabbageEra) do
      (txBody, publishScripts) <- buildPublishingImpl
        localNodeConnectInfo
        signingKey
        Nothing
        changeAddress
        publishingStrategy
        coinSelectionStrategy
        (PrintStats False)
      void $ submitBody localNodeConnectInfo txBody [signingKey] (30 :: Second)
      pure $ toMarloweScripts testnetMagic txBody publishScripts

toMarloweScripts :: Int -> TxBody BabbageEra -> PublishMarloweScripts MarlowePlutusVersion BabbageEra -> MarloweScripts
toMarloweScripts testnetMagic txBody PublishMarloweScripts{..} = MarloweScripts{..}
  where
    marloweValidatorInfo = psReferenceValidator pmsMarloweScript
    payoutValidatorInfo = psReferenceValidator pmsRolePayoutScript
    marloweScript = fromCardanoScriptHash $ viHash marloweValidatorInfo
    payoutScript = fromCardanoScriptHash $ viHash payoutValidatorInfo
    networkId = Testnet $ NetworkMagic $ fromIntegral testnetMagic
    publishTxId = fromCardanoTxId $ getTxId txBody
    marloweTxOutRef = TxOutRef publishTxId 1
    payoutTxOutRef = TxOutRef publishTxId 2
    marloweReferenceScriptUTxO = ReferenceScriptUtxo
      { txOutRef = marloweTxOutRef
      , txOut = TransactionOutput
        { address = fromCardanoAddressInEra BabbageEra $ psPublisher pmsMarloweScript
        , assets = Assets (fromCardanoLovelace $ psMinAda pmsMarloweScript) mempty
        , datumHash = Nothing
        , datum = Nothing
        }
      , script = viScript marloweValidatorInfo
      }
    payoutReferenceScriptUTxO = ReferenceScriptUtxo
      { txOutRef = payoutTxOutRef
      , txOut = TransactionOutput
        { address = fromCardanoAddressInEra BabbageEra $ psPublisher pmsRolePayoutScript
        , assets = Assets (fromCardanoLovelace $ psMinAda pmsRolePayoutScript) mempty
        , datumHash = Nothing
        , datum = Nothing
        }
      , script = viScript payoutValidatorInfo
      }
    marloweScriptUTxOs = Map.singleton networkId marloweReferenceScriptUTxO
    payoutScriptUTxOs = Map.singleton networkId payoutReferenceScriptUTxO

data RuntimeSelector f where
  AnyEvent :: s f -> RuntimeSelector f

instance Inject s RuntimeSelector where
  inject = injectSelector AnyEvent

data RuntimeDependencies r m = RuntimeDependencies
  { chainSyncPair :: ClientServerPair (Handshake RuntimeChainSeek) RuntimeChainSeekServer RuntimeChainSeekClient r m
  , chainSyncJobPair :: ClientServerPair (Handshake (Job ChainSyncCommand)) (JobServer ChainSyncCommand) (JobClient ChainSyncCommand) r m
  , chainSyncQueryPair :: ClientServerPair (Handshake (Query ChainSyncQuery)) (QueryServer ChainSyncQuery) (QueryClient ChainSyncQuery) r m
  , contractQueryPair :: ClientServerPair (Handshake (Query ContractRequest)) (QueryServer ContractRequest) (QueryClient ContractRequest) r m
  , marloweHeaderSyncPair :: ClientServerPair (Handshake MarloweHeaderSync) MarloweHeaderSyncServer MarloweHeaderSyncClient r m
  , marloweSyncPair :: ClientServerPair (Handshake MarloweSync) MarloweSyncServer MarloweSyncClient r m
  , marloweRuntimePair :: ClientServerPair (Handshake Protocol.MarloweRuntime) MarloweRuntimeServer MarloweRuntimeClient r (ResourceT m)
  , marloweQueryPair :: ClientServerPair (Handshake MarloweQuery) MarloweQueryServer MarloweQueryClient r m
  , marloweLoadPair :: ClientServerPair (Handshake MarloweLoad) MarloweLoadServer MarloweLoadClient r m
  , txJobPair :: ClientServerPair (Handshake (Job MarloweTxCommand)) (JobServer MarloweTxCommand) (JobClient MarloweTxCommand) r m
  , contractStore :: ContractStore m
  , chainIndexerDatabaseQueries :: ChainIndexer.DatabaseQueries m
  , chainSeekDatabaseQueries :: ChainSync.DatabaseQueries m
  , genesisBlock :: !GenesisBlock
  , localNodeConnectInfo :: LocalNodeConnectInfo CardanoMode
  , marloweIndexerDatabaseQueries :: Indexer.DatabaseQueries m
  , marloweSyncDatabaseQueries :: Sync.DatabaseQueries m
  , mkSubmitJob :: Tx BabbageEra -> STM (SubmitJob m)
  , securityParameter :: Int
  , marloweScripts :: MarloweScripts
  , webPort :: Int
  , proxyPort :: Int
  }

runtime :: forall r. (Monoid r, HasSpanContext r) => Component (RuntimeM r) (RuntimeDependencies r (RuntimeM r)) ()
runtime = proc RuntimeDependencies{..} -> do
  let
    getScripts :: MarloweVersion v -> Set MarloweScripts
    getScripts MarloweV1 = Set.singleton marloweScripts

    getCurrentScripts :: MarloweVersion v -> MarloweScripts
    getCurrentScripts MarloweV1 = marloweScripts

    LocalNodeConnectInfo{..} = localNodeConnectInfo

  marloweRuntimeServerSource <- handshakeConnectionSource <$> tcpServer -< TcpServerDependencies "127.0.0.1" (fromIntegral proxyPort) marloweRuntimeServerPeer

  chainIndexer -<
    let
      maxCost = 100_000
      costModel = CostModel 1 10
      persistRateLimit = secondsToNominalDiffTime 0.1
      databaseQueries = chainIndexerDatabaseQueries
      connectToLocalNode client = liftIO $ Cardano.connectToLocalNode localNodeConnectInfo $ client mempty
     in
      ChainIndexerDependencies{..}

  marloweIndexer -< MarloweIndexerDependencies
    { databaseQueries = marloweIndexerDatabaseQueries
    , chainSyncConnector = SomeConnectorTraced inject $ clientConnector chainSyncPair
    , chainSyncQueryConnector = SomeConnectorTraced inject $ clientConnector chainSyncQueryPair
    , pollingInterval = secondsToNominalDiffTime 0.01
    , marloweScriptHashes = NESet.singleton $ ScriptRegistry.marloweScript marloweScripts
    , payoutScriptHashes = NESet.singleton $ ScriptRegistry.payoutScript marloweScripts
    }

  sync -< SyncDependencies
    { databaseQueries = marloweSyncDatabaseQueries
    , syncSource = SomeConnectionSourceTraced inject $ Connection.connectionSource marloweSyncPair
    , headerSyncSource = SomeConnectionSourceTraced inject $ Connection.connectionSource marloweHeaderSyncPair
    , querySource = SomeConnectionSourceTraced inject $ Connection.connectionSource marloweQueryPair
    }

  chainSync -<
    let
      databaseQueries = chainSeekDatabaseQueries

      queryLocalNodeState :: Maybe ChainPoint -> QueryInMode CardanoMode result -> RuntimeM r (Either AcquiringFailure result)
      queryLocalNodeState = fmap liftIO . queryNodeLocalState localNodeConnectInfo

      submitTxToNodeLocal :: CardanoEra era -> Tx era -> RuntimeM r (SubmitResult (TxValidationErrorInMode CardanoMode))
      submitTxToNodeLocal era tx = liftIO $ Cardano.submitTxToNodeLocal localNodeConnectInfo $ TxInMode tx case era of
        ByronEra -> ByronEraInCardanoMode
        ShelleyEra -> ShelleyEraInCardanoMode
        AllegraEra -> AllegraEraInCardanoMode
        MaryEra -> MaryEraInCardanoMode
        AlonzoEra -> AlonzoEraInCardanoMode
        BabbageEra -> BabbageEraInCardanoMode
     in
      ChainSyncDependencies
        { syncSource = SomeConnectionSourceTraced inject $ Connection.connectionSource chainSyncPair
        , querySource = SomeConnectionSourceTraced inject $ Connection.connectionSource chainSyncQueryPair
        , jobSource = SomeConnectionSourceTraced inject $ Connection.connectionSource chainSyncJobPair
        , ..
        }

  transaction -<
    let
      chainSyncQueryConnector = SomeConnectorTraced inject (clientConnector chainSyncQueryPair)

      loadWalletContext = Query.loadWalletContext $ runSomeConnectorTraced chainSyncQueryConnector . request . GetUTxOs

      networkId = localNodeNetworkId

      loadMarloweContext :: LoadMarloweContext (RuntimeM r)
      loadMarloweContext = Query.loadMarloweContext
        getScripts
        networkId
        (SomeConnectorTraced inject $ clientConnector chainSyncPair)
        (SomeConnectorTraced inject $ clientConnector chainSyncQueryPair)

    in
      TransactionDependencies
        { chainSyncConnector = SomeConnectorTraced inject $ clientConnector chainSyncPair
        , contractQueryConnector = SomeConnectorTraced inject $ clientConnector contractQueryPair
        , connectionSource = SomeConnectionSourceTraced inject $ Connection.connectionSource txJobPair
        , ..
        }

  contract -< ContractDependencies
    { batchSize = unsafeIntToNat 1024
    , loadSource = SomeConnectionSourceTraced inject $ Connection.connectionSource marloweLoadPair
    , querySource = SomeConnectionSourceTraced inject $ Connection.connectionSource contractQueryPair
    , ..
    }

  proxy -< ProxyDependencies
    { router = Router
        { connectMarloweSync = driverFactory $ clientConnector marloweSyncPair
        , connectMarloweHeaderSync = driverFactory $ clientConnector marloweHeaderSyncPair
        , connectMarloweQuery = driverFactory $ clientConnector marloweQueryPair
        , connectMarloweLoad = driverFactory $ clientConnector marloweLoadPair
        , connectTxJob = driverFactory $ clientConnector txJobPair
        , connectContractQuery = driverFactory $ clientConnector contractQueryPair
        }
    , connectionSource = SomeConnectionSource marloweRuntimeServerSource
    , connectionSourceTraced = SomeConnectionSourceTraced inject $ Connection.connectionSource marloweRuntimePair
    }

  hoistComponent (liftIO . flip runReaderT (noopEventBackend mempty) . runBackendM) server -< ServerDependencies
    { openAPIEnabled = False
    , accessControlAllowOriginAll = False
    , runApplication = run webPort
    , marloweTracedContext = MarloweTracedContext (injectSelector RuntimeClient) $ ihoistConnectorTraced hoistMarloweRuntimeClient
        (liftIO . runRuntimeM . runResourceT)
        (liftIO . flip runReaderT (noopEventBackend mempty) . runBackendM)
        (clientConnector marloweRuntimePair)
    }

driverFactory
  :: (BinaryMessage ps, MonadUnliftIO m, HasSpanContext r, Monoid r, MonadEvent r s m, Inject (TypedProtocolsSelector ps) s, Inject (DriverSelector ps) s)
  => ClientConnectorTraced ps client r STMConnectorSelector m
  -> ResourceT m (Channel ps pr st (ResourceT m), r)
driverFactory ConnectorTraced{..} = withRunInIO \runInIO -> do
  (_, ConnectionTraced{..}) <- runInIO $ allocate (runInIO $ lift openConnectionTraced) \ConnectionTraced{..} -> runInIO $ lift $ closeConnection Nothing
  pure (driverToChannel inject $ mkDriverTraced inject $ hoistChannel lift channel, mempty)

data Channels r m = Channels
  { chainSyncPair :: ClientServerPair (Handshake RuntimeChainSeek) RuntimeChainSeekServer RuntimeChainSeekClient r m
  , chainSyncJobPair :: ClientServerPair (Handshake (Job ChainSyncCommand)) (JobServer ChainSyncCommand) (JobClient ChainSyncCommand) r m
  , chainSyncQueryPair :: ClientServerPair (Handshake (Query ChainSyncQuery)) (QueryServer ChainSyncQuery) (QueryClient ChainSyncQuery) r m
  , contractQueryPair :: ClientServerPair (Handshake (Query ContractRequest)) (QueryServer ContractRequest) (QueryClient ContractRequest) r m
  , marloweHeaderSyncPair :: ClientServerPair (Handshake MarloweHeaderSync) MarloweHeaderSyncServer MarloweHeaderSyncClient r m
  , marloweSyncPair :: ClientServerPair (Handshake MarloweSync) MarloweSyncServer MarloweSyncClient r m
  , marloweQueryPair :: ClientServerPair (Handshake MarloweQuery) MarloweQueryServer MarloweQueryClient r m
  , marloweLoadPair :: ClientServerPair (Handshake MarloweLoad) MarloweLoadServer MarloweLoadClient r m
  , txJobPair :: ClientServerPair (Handshake (Job MarloweTxCommand)) (JobServer MarloweTxCommand) (JobClient MarloweTxCommand) r m
  , marloweRuntimePair :: ClientServerPair (Handshake Protocol.MarloweRuntime) MarloweRuntimeServer MarloweRuntimeClient r (ResourceT m)
  }

setupChannels :: Monoid r => STM (Channels r (RuntimeM r))
setupChannels = do
  chainSyncPair <- handshakeClientServerPair <$> clientServerPair
    chainSeekServerPeer
    chainSeekClientPeer
  chainSyncJobPair <- handshakeClientServerPair <$> clientServerPair
    jobServerPeer
    jobClientPeer
  chainSyncQueryPair <- handshakeClientServerPair <$> clientServerPair
    queryServerPeer
    queryClientPeer
  contractQueryPair <- handshakeClientServerPair <$> clientServerPair
    queryServerPeer
    queryClientPeer
  marloweHeaderSyncPair <- handshakeClientServerPair <$> clientServerPair
    marloweHeaderSyncServerPeer
    marloweHeaderSyncClientPeer
  marloweSyncPair <- handshakeClientServerPair <$> clientServerPair
    marloweSyncServerPeer
    marloweSyncClientPeer
  marloweQueryPair <- handshakeClientServerPair <$> clientServerPair
    queryServerPeer
    queryClientPeer
  marloweLoadPair <- handshakeClientServerPair <$> clientServerPair
    marloweLoadServerPeer
    marloweLoadClientPeer
  txJobPair <- handshakeClientServerPair <$> clientServerPair
    jobServerPeer
    jobClientPeer
  marloweRuntimePair <- handshakeClientServerPair <$> clientServerPair
    marloweRuntimeServerPeer
    marloweRuntimeClientPeer
  pure Channels{..}

newtype RuntimeM r a = RuntimeM { runRuntimeM :: IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadFail, MonadThrow, MonadCatch, MonadMask)

instance MonadWith (RuntimeM r) where
  type WithException (RuntimeM r) = WithException IO
  stateThreadingGeneralWith
    :: forall a b releaseReturn
     . GeneralAllocate (RuntimeM r) (WithException IO) releaseReturn b a
    -> (a -> RuntimeM r b)
    -> RuntimeM r (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = RuntimeM do
    let
      allocA' :: (forall x. IO x -> IO x) -> IO (GeneralAllocated IO (WithException IO) releaseReturn b a)
      allocA' restore = do
        let
          restore' :: forall x. RuntimeM r x -> RuntimeM r x
          restore' mx = RuntimeM $ restore $ runRuntimeM mx
        GeneralAllocated a releaseA <- runRuntimeM (allocA restore')
        let
          releaseA' relTy = runRuntimeM (releaseA relTy)
        pure $ GeneralAllocated a releaseA'
    stateThreadingGeneralWith (GeneralAllocate allocA') (runRuntimeM . go)

instance Monoid r => MonadEvent r RuntimeSelector (RuntimeM r) where
  askBackend = pure $ noopEventBackend mempty
  localBackend _ = id
