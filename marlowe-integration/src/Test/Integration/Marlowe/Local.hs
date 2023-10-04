{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Marlowe.Local (
  MarloweRuntime (..),
  RuntimeRef,
  RuntimeSelector,
  module Test.Integration.Cardano,
  Test.Integration.Cardano.exec,
  defaultMarloweRuntimeOptions,
  withLocalMarloweRuntime,
  withLocalMarloweRuntime',
) where

import Cardano.Api (
  AsType (..),
  BabbageEra,
  CardanoEra (..),
  CardanoMode,
  ConsensusModeParams (..),
  EpochSlots (..),
  File (..),
  LocalNodeConnectInfo (..),
  NetworkId (..),
  NetworkMagic (..),
  ScriptDataSupportedInEra (ScriptDataInBabbageEra),
  StakeAddressReference (..),
  deserialiseFromBech32,
  deserialiseFromTextEnvelope,
  shelleyAddressInEra,
 )
import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Byron as Byron
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Chain.UTxO (defaultUTxOConfiguration)
import Cardano.Crypto (abstractHashToBytes)
import Colog (cmap, fmtMessage, logTextHandle)
import Control.Arrow (returnA)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.Component
import Control.Concurrent.Component.Run (AppM, runAppM)
import Control.DeepSeq (NFData)
import Control.Exception (bracketOnError, catch, onException, throw, try)
import Control.Monad (when, (<=<))
import Control.Monad.Catch hiding (bracketOnError, catch, onException, try)
import Control.Monad.Event.Class
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import Control.Monad.Trans.Resource (allocate, runResourceT, unprotect)
import Data.Aeson (eitherDecodeFileStrict)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (for_)
import Data.Functor (void)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import Data.String (fromString)
import qualified Data.Text as T
import Data.Time.Units (Second)
import Data.Version (Version (Version))
import Data.Word (Word16)
import Database.PostgreSQL.LibPQ (connectdb, errorMessage, exec, finish, resultErrorMessage)
import Hasql.Connection (settings)
import qualified Hasql.Pool as Pool
import Language.Marlowe.CLI.IO (submitTxBody)
import Language.Marlowe.CLI.Transaction (buildPublishingImpl)
import Language.Marlowe.CLI.Types (
  AnUTxO (..),
  CliEnv (..),
  MarlowePlutusVersion,
  MarloweScriptsRefs (..),
  PrintStats (..),
  PublishingStrategy (..),
  SomePaymentSigningKey (SomePaymentSigningKeyGenesisUTxO),
  SubmitMode (..),
  TxBuildupContext (..),
  ValidatorInfo (..),
  defaultCoinSelectionStrategy,
 )
import Language.Marlowe.Protocol.Client (MarloweRuntimeClient, hoistMarloweRuntimeClient)
import Language.Marlowe.Protocol.Server (marloweRuntimeServerDirectPeer, serveMarloweRuntimeClientDirect)
import Language.Marlowe.Runtime (MarloweRuntimeDependencies (..), marloweRuntime)
import qualified Language.Marlowe.Runtime as Runtime
import Language.Marlowe.Runtime.Cardano.Api (assetsFromCardanoValue, fromCardanoAddressInEra, fromCardanoTxIn)
import Language.Marlowe.Runtime.ChainIndexer.Database (CommitGenesisBlock (..), DatabaseQueries (..))
import qualified Language.Marlowe.Runtime.ChainIndexer.Database as ChainIndexer
import qualified Language.Marlowe.Runtime.ChainIndexer.Database.PostgreSQL as ChainIndexer
import Language.Marlowe.Runtime.ChainIndexer.Genesis (GenesisBlock, computeGenesisBlock)
import Language.Marlowe.Runtime.ChainIndexer.NodeClient (CostModel (CostModel))
import Language.Marlowe.Runtime.ChainSync.Api (
  BlockNo (..),
  TransactionOutput (..),
  fromCardanoScriptHash,
 )
import qualified Language.Marlowe.Runtime.ChainSync.Database as ChainSync
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as ChainSync
import Language.Marlowe.Runtime.Contract.Store (ContractStore)
import Language.Marlowe.Runtime.Contract.Store.File (ContractStoreOptions (..), createContractStore)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (..))
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts (..), ReferenceScriptUtxo (..))
import qualified Language.Marlowe.Runtime.Indexer.Database as Indexer
import qualified Language.Marlowe.Runtime.Indexer.Database.PostgreSQL as IndexerDB
import qualified Language.Marlowe.Runtime.Sync.Database as Sync
import qualified Language.Marlowe.Runtime.Sync.Database.PostgreSQL as Sync
import Language.Marlowe.Runtime.Transaction (mkCommandLineRoleTokenMintingPolicy)
import Language.Marlowe.Runtime.Web.Client (healthcheck)
import Language.Marlowe.Runtime.Web.Server (ServerDependencies (..), server)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Protocol.Connection
import Network.Protocol.Driver (TcpServerDependencies (TcpServerDependencies), tcpServer)
import Network.Protocol.Driver.Trace (HasSpanContext (..))
import Network.Protocol.Peer.Trace (defaultSpanContext)
import Network.Socket (
  AddrInfo (..),
  SocketOption (ReuseAddr),
  SocketType (..),
  bind,
  close,
  defaultHints,
  getAddrInfo,
  openSocket,
  setCloseOnExecIfNeeded,
  setSocketOption,
  withFdSocket,
 )
import Network.TypedProtocol.Pipelined (unsafeIntToNat)
import Network.Wai.Handler.Warp (run)
import Observe.Event.Explicit (injectSelector, noopEventBackend)
import Servant.Client (BaseUrl (..), ClientError, Scheme (..), mkClientEnv)
import Servant.Client.Internal.HttpClient.Streaming (ClientM)
import Servant.Client.Streaming (runClientM)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (BufferMode (..), IOMode (..), hSetBuffering)
import System.Process (readCreateProcessWithExitCode)
import qualified System.Process as SP
import System.Random (randomRIO)
import Test.Integration.Cardano hiding (exec)
import qualified Test.Integration.Cardano (exec)
import qualified Test.Integration.Cardano as SpoNode (SpoNode (..))
import Text.Read (readMaybe)
import UnliftIO (Concurrently (..), MonadUnliftIO, atomically, throwIO, withRunInIO)

data RuntimeRef = RuntimeRef

instance Semigroup RuntimeRef where
  (<>) = const

instance Monoid RuntimeRef where
  mempty = RuntimeRef

instance HasSpanContext RuntimeRef where
  context _ = pure defaultSpanContext
  wrapContext _ = RuntimeRef

data MarloweRuntime = MarloweRuntime
  { protocolConnector :: Connector MarloweRuntimeClient (NoopEventT RuntimeRef RuntimeSelector IO)
  , proxyPort :: Int
  , runWebClient :: forall a. (NFData a) => ClientM a -> IO (Either ClientError a)
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
  pure $
    MarloweRuntimeOptions
      (maybe "127.0.0.1" fromString databaseHost)
      (fromMaybe 5432 $ readMaybe =<< databasePort)
      (maybe "postgres" fromString databaseUser)
      (maybe "" fromString databasePassword)
      (maybe "template1" fromString tempDatabase)
      (fromMaybe True $ readMaybe =<< cleanupDatabase)
      (BlockNo $ fromMaybe 2 $ readMaybe =<< submitConfirmationBlocks)
      defaultOptions

withLocalMarloweRuntime :: (MonadUnliftIO m) => (MarloweRuntime -> m ()) -> m ()
withLocalMarloweRuntime test = do
  options <- liftIO defaultMarloweRuntimeOptions
  withLocalMarloweRuntime' options test

withLocalMarloweRuntime' :: (MonadUnliftIO m) => MarloweRuntimeOptions -> (MarloweRuntime -> m ()) -> m ()
withLocalMarloweRuntime' MarloweRuntimeOptions{..} test = withRunInIO \runInIO ->
  withLocalTestnet' localTestnetOptions \testnet@LocalTestnet{..} -> runResourceT do
    let localConsensusModeParams = CardanoModeParams $ EpochSlots 500
    let localNodeNetworkId = Testnet $ NetworkMagic $ fromIntegral testnetMagic
    let localNodeSocketPath = File . SpoNode.socket . head $ spoNodes
    let localNodeConnectInfo = LocalNodeConnectInfo{..}
    marloweScripts <- liftIO $ publishCurrentScripts testnet localNodeConnectInfo
    (dbReleaseKey, dbName) <- allocate (createDatabase workspace) (cleanupDatabase 10_000)
    liftIO $ migrateDatabase dbName
    let connectionString = settings databaseHost databasePort databaseUser databasePassword dbName
    let acquirePool = Pool.acquire 100 (Just 5000000) connectionString
    (_, pool) <- allocate acquirePool Pool.release
    logFileHandle <- openWorkspaceFile workspace "logs/runtime.log" WriteMode
    liftIO $ hSetBuffering logFileHandle LineBuffering
    genesisConfigResult <- runExceptT . Byron.readGenesisData $ byronGenesisJson network
    (genesisData, genesisHash) <- case genesisConfigResult of
      Left e -> fail $ show e
      Right a -> pure a
    shelleyGenesisConfig <- liftIO $ either error id <$> eitherDecodeFileStrict (shelleyGenesisJson network)
    let byronGenesisConfig =
          Byron.Config
            genesisData
            genesisHash
            (Byron.toByronRequiresNetworkMagic localNodeNetworkId)
            defaultUTxOConfiguration

        genesisBlock =
          computeGenesisBlock
            (abstractHashToBytes $ Byron.unGenesisHash genesisHash)
            byronGenesisConfig
            shelleyGenesisConfig

        chainIndexerDatabaseQueries = ChainIndexer.databaseQueries pool genesisBlock

        chainSyncDatabaseQueries = ChainSync.databaseQueries pool localNodeNetworkId

        marloweIndexerDatabaseQueries = IndexerDB.databaseQueries pool securityParameter

        marloweSyncDatabaseQueries =
          Sync.logDatabaseQueries $
            Sync.hoistDatabaseQueries
              (either throwIO pure <=< liftIO . Pool.use pool)
              Sync.databaseQueries

    webPort <- liftIO $ randomPort 4000 4999
    proxyPort <- liftIO $ randomPort 5000 5999
    manager <- liftIO $ newManager defaultManagerSettings

    contractStore <-
      createContractStore
        ContractStoreOptions
          { contractStoreDirectory = resolveWorkspacePath workspace "contract-store"
          , contractStoreStagingDirectory = resolveWorkspacePath workspace "contract-staging-area"
          , lockingMicrosecondsBetweenRetries = 100_000
          , minContractAge = 60 -- In seconds
          , maxStoreSize = 4 * 1024 * 1024 * 1024 -- 4 GB
          }

    let baseUrl = BaseUrl Http "localhost" webPort ""
    let clientEnv = mkClientEnv manager baseUrl
    let runWebClient :: (NFData a) => ClientM a -> IO (Either ClientError a)
        runWebClient = flip runClientM clientEnv

        logAction = cmap fmtMessage $ logTextHandle logFileHandle
        eventBackend = noopEventBackend RuntimeRef

        waitForWebServer :: Int -> IO ()
        waitForWebServer counter
          | counter < 10 = void $ runWebClient do
              result <- healthcheck
              if result
                then pure ()
                else liftIO $ threadDelay 1000 *> waitForWebServer (counter + 1)
          | otherwise = fail "Unable to connect to web server"

        networkId = Testnet $ NetworkMagic $ fromIntegral testnetMagic

    (Concurrently testAction, Runtime.MarloweRuntime{..}) <-
      atomically $ unComponent testContainer TestContainerDependencies{..}

    let protocolConnector =
          ihoistConnector
            hoistMarloweRuntimeClient
            (NoopEventT . runAppM @RuntimeRef eventBackend logAction)
            (liftIO . runNoopEventT)
            (directConnector serveMarloweRuntimeClientDirect serverSource)

    -- Persist the genesis block before starting the services so that they
    -- exist already and no database queries fail.
    liftIO do
      runAppM eventBackend logAction $
        runCommitGenesisBlock (commitGenesisBlock chainIndexerDatabaseQueries) genesisBlock
      onException
        ( runAppM eventBackend logAction testAction
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
        ( \(SomeException e) ->
            if retryDelay > 1_000_000
              then throw e
              else do
                threadDelay retryDelay
                cleanupDatabase (retryDelay * 10) dbName
        )

    migrateDatabase dbName = do
      (exitCode, _, stderr) <-
        flip readCreateProcessWithExitCode "" $
          ( SP.proc
              "sqitch"
              [ "deploy"
              , "-h"
              , BS.unpack databaseHost
              , "-p"
              , show databasePort
              , "-u"
              , BS.unpack databaseUser
              , "-d"
              , BS.unpack dbName
              ]
          )
            { SP.cwd = Just "./marlowe-chain-sync"
            }
      case exitCode of
        ExitFailure _ -> fail $ "chain sqitch failed: \n" <> stderr
        ExitSuccess -> do
          (exitCode', _, stderr') <-
            flip readCreateProcessWithExitCode "" $
              ( SP.proc
                  "sqitch"
                  [ "deploy"
                  , "-h"
                  , BS.unpack databaseHost
                  , "-p"
                  , show databasePort
                  , "-u"
                  , BS.unpack databaseUser
                  , "-d"
                  , BS.unpack dbName
                  ]
              )
                { SP.cwd = Just "./marlowe-runtime/marlowe-indexer"
                }
          case exitCode' of
            ExitFailure _ -> fail $ "marlowe sqitch failed: \n" <> stderr'
            ExitSuccess -> pure ()

randomPort :: Int -> Int -> IO Int
randomPort lo hi = do
  candidate <- randomRIO (lo, hi)
  result <- try @SomeException do
    let hints = defaultHints{addrSocketType = Stream}
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
  let PaymentKeyPair{..} = head wallets
  signingKey <-
    SomePaymentSigningKeyGenesisUTxO
      . either (error . show) id
      . deserialiseFromTextEnvelope (AsSigningKey AsGenesisUTxOKey)
      . either error id
      <$> eitherDecodeFileStrict paymentSKey
  changeAddress <-
    either (error . show) shelleyAddressInEra
      . deserialiseFromBech32 (AsAddress AsShelleyAddr)
      . T.pack
      <$> execCli
        [ "address"
        , "build"
        , "--payment-verification-key-file"
        , paymentVKey
        , "--testnet-magic"
        , show testnetMagic
        ]
  let publishingStrategy = PublishPermanently NoStakeAddress
  let coinSelectionStrategy = defaultCoinSelectionStrategy
  either throwIO pure =<< runExceptT do
    flip runReaderT (CliEnv ScriptDataInBabbageEra) do
      let submitCtx = NodeTxBuildup localNodeConnectInfo (DoSubmit (30 :: Second))
      (txBodies, publishedScripts) <-
        buildPublishingImpl
          submitCtx
          signingKey
          Nothing
          changeAddress
          publishingStrategy
          coinSelectionStrategy
          (PrintStats False)
      for_ txBodies \txBody -> do
        submitTxBody submitCtx txBody [signingKey]
      pure $ toMarloweScripts testnetMagic publishedScripts

toMarloweScripts :: Int -> MarloweScriptsRefs MarlowePlutusVersion BabbageEra -> MarloweScripts
toMarloweScripts testnetMagic MarloweScriptsRefs{..} = MarloweScripts{..}
  where
    marloweValidatorInfo = snd mrMarloweValidator
    payoutValidatorInfo = snd mrRolePayoutValidator
    marloweScript = fromCardanoScriptHash $ viHash marloweValidatorInfo
    payoutScript = fromCardanoScriptHash $ viHash payoutValidatorInfo
    networkId = Testnet $ NetworkMagic $ fromIntegral testnetMagic
    marloweTxOutRef = fromCardanoTxIn $ fst $ unAnUTxO $ fst mrMarloweValidator
    payoutTxOutRef = fromCardanoTxIn $ fst $ unAnUTxO $ fst mrRolePayoutValidator
    refScriptPublisher (AnUTxO (_, Cardano.TxOut addr _ _ _), _) = addr
    refScriptValue (AnUTxO (_, Cardano.TxOut _ value _ _), _) = Cardano.txOutValueToValue value
    helperScript = undefined
    helperScriptUTxOs = undefined

    marloweReferenceScriptUTxO =
      ReferenceScriptUtxo
        { txOutRef = marloweTxOutRef
        , txOut =
            TransactionOutput
              { address = fromCardanoAddressInEra BabbageEra $ refScriptPublisher mrMarloweValidator
              , assets = assetsFromCardanoValue $ refScriptValue mrMarloweValidator
              , datumHash = Nothing
              , datum = Nothing
              }
        , script = viScript marloweValidatorInfo
        }
    payoutReferenceScriptUTxO =
      ReferenceScriptUtxo
        { txOutRef = payoutTxOutRef
        , txOut =
            TransactionOutput
              { address = fromCardanoAddressInEra BabbageEra $ refScriptPublisher mrRolePayoutValidator
              , assets = assetsFromCardanoValue $ refScriptValue mrRolePayoutValidator
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

data TestContainerDependencies r m = TestContainerDependencies
  { chainIndexerDatabaseQueries :: ChainIndexer.DatabaseQueries m
  , chainSyncDatabaseQueries :: ChainSync.DatabaseQueries m
  , contractStore :: ContractStore m
  , genesisBlock :: GenesisBlock
  , marloweIndexerDatabaseQueries :: Indexer.DatabaseQueries m
  , marloweSyncDatabaseQueries :: Sync.DatabaseQueries m
  , submitConfirmationBlocks :: BlockNo
  , networkId :: NetworkId
  , localNodeConnectInfo :: LocalNodeConnectInfo CardanoMode
  , securityParameter :: Int
  , marloweScripts :: MarloweScripts
  , webPort :: Int
  , proxyPort :: Int
  }

testContainer
  :: Component
      (AppM r RuntimeSelector)
      (TestContainerDependencies r (AppM r RuntimeSelector))
      (Runtime.MarloweRuntime (AppM r RuntimeSelector))
testContainer = proc TestContainerDependencies{..} -> do
  let getScripts :: MarloweVersion v -> Set MarloweScripts
      getScripts MarloweV1 = Set.singleton marloweScripts

      getCurrentScripts :: MarloweVersion v -> MarloweScripts
      getCurrentScripts MarloweV1 = marloweScripts

  runtime@Runtime.MarloweRuntime{..} <-
    marloweRuntime
      -<
        let maxCost = 100_000
            costModel = CostModel 1 10
            persistRateLimit = 0.1
            connectToLocalNode = liftIO . Cardano.connectToLocalNode localNodeConnectInfo
            batchSize = unsafeIntToNat 10
            marloweScriptHashes = NESet.singleton $ marloweScript marloweScripts
            payoutScriptHashes = NESet.singleton $ payoutScript marloweScripts
            pollingInterval = 0.01
            confirmationTimeout = 60
            runtimeVersion = Version [0] []
            indexParties = pure ()
            mkRoleTokenMintingPolicy = mkCommandLineRoleTokenMintingPolicy "marlowe-minting-validator"
         in MarloweRuntimeDependencies{..}

  tcpServer "marlowe-runtime"
    -<
      TcpServerDependencies "127.0.0.1" (fromIntegral proxyPort) serverSource marloweRuntimeServerDirectPeer

  server
    -<
      ServerDependencies
        { openAPIEnabled = False
        , accessControlAllowOriginAll = False
        , runApplication = run webPort
        , connector = directConnector serveMarloweRuntimeClientDirect serverSource
        }

  returnA -< runtime
