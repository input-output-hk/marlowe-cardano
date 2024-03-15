{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Integration.Marlowe.Local (
  Attempts (..),
  MarloweRuntime (..),
  RuntimeRef,
  module Test.Integration.Cardano,
  Test.Integration.Cardano.exec,
  defaultMarloweRuntimeOptions,
  localTestnetToLocalNodeConnectInfo,
  retryTillTrue,
  withLocalMarloweRuntime,
  withLocalMarloweRuntime',
) where

import Cardano.Api (
  AsType (..),
  BabbageEra,
  BabbageEraOnwards (..),
  CardanoEra (..),
  ConsensusModeParams (..),
  EpochSlots (..),
  File (..),
  LocalNodeConnectInfo (..),
  NetworkId (..),
  NetworkMagic (..),
  ShelleyBasedEra (..),
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
import Colog (LogAction (LogAction), Message, cmap, fmtMessage, logTextHandle, logTextStderr)
import Control.Arrow (returnA)
import Control.Concurrent (newMVar, threadDelay, withMVar)
import Control.Concurrent.Async (race_)
import Control.Concurrent.Component (Component (unComponent))
import Control.Concurrent.Component.Run (AppM, runAppM)
import Control.DeepSeq (NFData)
import Control.Exception (bracketOnError, catch, onException, throw, try)
import Control.Monad (when, (<=<))
import Control.Monad.Catch (SomeException (..))
import Control.Monad.Event.Class (Inject (..), NoopEventT (..))
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Component
import Control.Concurrent.Component.Run (AppM, TracingConfig (UseHandleDebugTracerProvider), mkEventBackend, runAppM)
import Control.DeepSeq (NFData)
import Control.Exception (bracketOnError, catch, onException, throw, try)
import Control.Monad (when, (<=<))
import Control.Monad.Catch hiding (bracketOnError, catch, onException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import Control.Monad.Trans.Resource (allocate, register, runResourceT, unprotect)
import Data.Aeson (eitherDecodeFileStrict)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (Foldable (fold), for_)
import Data.Functor (void)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import Data.String (fromString)
import qualified Data.Text as T
import Data.Time.Units (Second)
import Data.Version (Version (Version), showVersion)
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
import Language.Marlowe.Runtime (MarloweRuntimeDependencies (..), marloweRuntime, supervisor, unnestNodeClient)
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
  mkTxOutAssets,
 )
import qualified Language.Marlowe.Runtime.ChainSync.Api as CS
import qualified Language.Marlowe.Runtime.ChainSync.Database as ChainSync.Database
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as ChainSync.Database
import qualified Language.Marlowe.Runtime.ChainSync.NodeClient as ChainSync
import Language.Marlowe.Runtime.ChainSync.QueryServer (ChainSyncQueryServerDependencies (..), chainSyncQueryServer)
import Language.Marlowe.Runtime.Contract.Store (ContractStore)
import Language.Marlowe.Runtime.Contract.Store.File (ContractStoreOptions (..), createContractStore)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (..))
import Language.Marlowe.Runtime.Core.ScriptRegistry (HelperScript (..), MarloweScripts (..), ReferenceScriptUtxo (..))
import qualified Language.Marlowe.Runtime.Indexer.Database as Indexer
import qualified Language.Marlowe.Runtime.Indexer.Database.PostgreSQL as IndexerDB
import Language.Marlowe.Runtime.Logging (RootSelector (..))
import qualified Language.Marlowe.Runtime.Sync.Database as Sync
import qualified Language.Marlowe.Runtime.Sync.Database.PostgreSQL as Sync
import Language.Marlowe.Runtime.Transaction (mkCommandLineRoleTokenMintingPolicy)
import Language.Marlowe.Runtime.Web.Client (healthcheck)
import Language.Marlowe.Runtime.Web.RuntimeServer (ServerDependencies (..), runtimeServer)
import Language.Marlowe.Runtime.Web.Server (ServerDependencies (..), server)
import Language.Marlowe.Runtime.Web.Server.Logging (renderServeRequestOTel)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Protocol.Connection (
  Connector,
  directConnector,
  ihoistConnector,
 )
import Network.Protocol.Driver (TcpServerDependencies (TcpServerDependencies), tcpServer)
import Network.Protocol.Driver.Trace (HasSpanContext (..))
import Network.Protocol.Peer.Trace (defaultSpanContext)
import Network.Protocol.Query.Client (QueryClient, hoistQueryClient, serveQueryClient)
import Network.Socket (
  AddrInfo (..),
  PortNumber,
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
import Observe.Event.Explicit (idInjectSelector, injectSelector)
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
import Text.Read (readMaybe)
import UnliftIO (Concurrently (..), MonadUnliftIO, atomically, throwIO, withRunInIO)
import UnliftIO.Retry (constantDelay, limitRetries, retrying)

import Control.Monad.Event.Class (Inject (inject), composeInjectSelector)
import qualified Language.Marlowe.Runtime.ChainSync.Api as ChainSync
import qualified Language.Marlowe.Runtime.Logging as Runtime.Logging
import qualified Language.Marlowe.Runtime.Web.Server as Web.Server
import qualified Network.Protocol.Query.Client as Query.Client
import Observe.Event.Render.OpenTelemetry (OTelRendered (..))
import OpenTelemetry.Trace.Core (InstrumentationLibrary (InstrumentationLibrary, libraryName, libraryVersion), Span)

data RuntimeRef = RuntimeRef

instance Semigroup RuntimeRef where
  (<>) = const

instance Monoid RuntimeRef where
  mempty = RuntimeRef

instance HasSpanContext RuntimeRef where
  context _ = pure defaultSpanContext
  wrapContext _ = RuntimeRef

data MarloweRuntime = MarloweRuntime
  { protocolConnector :: Connector MarloweRuntimeClient IO
  , chainSyncQueryConnector :: Connector (QueryClient CS.ChainSyncQuery) IO
  , proxyPort :: Int
  , runWebClient :: forall a. (NFData a) => ClientM a -> IO (Either ClientError a)
  , marloweScripts :: MarloweScripts
  , testnet :: LocalTestnet
  }

localTestnetToLocalNodeConnectInfo :: LocalTestnet -> LocalNodeConnectInfo
localTestnetToLocalNodeConnectInfo LocalTestnet{..} =
  LocalNodeConnectInfo
    { localConsensusModeParams = CardanoModeParams $ EpochSlots 500
    , localNodeNetworkId = Testnet $ NetworkMagic $ fromIntegral testnetMagic
    , localNodeSocketPath = File . socket . head $ spoNodes
    }

data MarloweRuntimeOptions = MarloweRuntimeOptions
  { databaseHost :: ByteString
  , databasePort :: Word16
  , databaseUser :: ByteString
  , databasePassword :: ByteString
  , tempDatabase :: ByteString
  , testDatabase :: Maybe ByteString
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
  testDatabase <- lookupEnv "MARLOWE_RT_TEST_DB"
  cleanupDatabase <- lookupEnv "MARLOWE_RT_TEST_CLEANUP_DATABASE"
  submitConfirmationBlocks <- lookupEnv "MARLOWE_RT_TEST_SUBMIT_CONFIRMATION_BLOCKS"
  pure $
    MarloweRuntimeOptions
      (maybe "127.0.0.1" fromString databaseHost)
      (fromMaybe 5432 $ readMaybe =<< databasePort)
      (maybe "postgres" fromString databaseUser)
      (maybe "" fromString databasePassword)
      (maybe "postgres" fromString tempDatabase)
      (fromString <$> testDatabase)
      (fromMaybe True $ readMaybe =<< cleanupDatabase)
      (BlockNo $ fromMaybe 2 $ readMaybe =<< submitConfirmationBlocks)
      defaultOptions

withLocalMarloweRuntime :: (MonadUnliftIO m) => (MarloweRuntime -> m ()) -> m ()
withLocalMarloweRuntime test = do
  options <- liftIO defaultMarloweRuntimeOptions
  withLocalMarloweRuntime' options test

-- This logger enforces sequencing of log messages, so that they are not interleaved.
toPseudoConcurrentLogger :: (MonadUnliftIO m) => LogAction m Message -> IO (LogAction m Message)
toPseudoConcurrentLogger (LogAction baseAction) = do
  lock <- newMVar ()
  pure $ LogAction $ \msg -> withRunInIO \runInIO -> do
    withMVar lock . const $ runInIO (baseAction msg)

withLocalMarloweRuntime' :: (MonadUnliftIO m) => MarloweRuntimeOptions -> (MarloweRuntime -> m ()) -> m ()
withLocalMarloweRuntime' MarloweRuntimeOptions{..} test = withRunInIO \runInIO ->
  withLocalTestnet' localTestnetOptions \testnet@LocalTestnet{..} -> runResourceT do
    let localNodeNetworkId = Testnet $ NetworkMagic $ fromIntegral testnetMagic
    let localNodeConnectInfo = localTestnetToLocalNodeConnectInfo testnet
    marloweScripts <- liftIO $ publishCurrentScripts testnet localNodeConnectInfo
    let dbName = fromMaybe (fromString $ "chain_test_" <> show (workspaceId workspace)) testDatabase
    (dbReleaseKey, _) <- allocate (createDatabase dbName) (const $ cleanupDatabase 10_000 dbName)
    liftIO $ migrateDatabase dbName
    let connectionString = settings databaseHost databasePort databaseUser databasePassword dbName
    let acquirePool = Pool.acquire 100 (Just 5000000) connectionString
    (_, pool) <- allocate acquirePool Pool.release
    logFileHandle <- openWorkspaceFile workspace "logs/runtime.log" WriteMode
    traceFileHandle <- openWorkspaceFile workspace "logs/runtime.trace" WriteMode
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

        chainIndexerDatabaseQueries = ChainIndexer.databaseQueries pool

        chainSyncDatabaseQueries = ChainSync.Database.databaseQueries pool localNodeNetworkId

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

        waitForWebServer :: Int -> IO ()
        waitForWebServer counter
          | counter < 10 = void $ runWebClient do
              result <- healthcheck
              if result
                then pure ()
                else liftIO $ threadDelay 1000 *> waitForWebServer (counter + 1)
          | otherwise = fail "Unable to connect to web server"

        networkId = Testnet $ NetworkMagic $ fromIntegral testnetMagic

    logAction <-
      liftIO $ do
        stderrLogger <- do
          v <- lookupEnv "MARLOWE_RT_LOG_STDERR"
          let shouldLog = fromMaybe False $ readMaybe =<< v
          if shouldLog
            then pure logTextStderr
            else pure mempty

        toPseudoConcurrentLogger $
          cmap fmtMessage $
            logTextHandle logFileHandle
              <> stderrLogger

    (eventBackend, shutdownTracerProvider) <- liftIO do
      let runtimeVersion = Version [0] []
          library =
            InstrumentationLibrary
              { libraryName = "marlowe-runtime"
              , libraryVersion = T.pack $ showVersion runtimeVersion
              }
          tracingConfig =
            UseHandleDebugTracerProvider
              traceFileHandle
              library
              (renderTestContainerSelector (fromIntegral webPort) (DbName dbName))
      mkEventBackend tracingConfig
    void $ register shutdownTracerProvider

    (Concurrently testAction, (Runtime.MarloweRuntime{..}, nodeClient)) <-
      atomically $ unComponent testContainer TestContainerDependencies{..}

    let protocolConnector =
          ihoistConnector
            hoistMarloweRuntimeClient
            (runAppM eventBackend logAction)
            liftIO
            (directConnector serveMarloweRuntimeClientDirect serverSource)
        chainSyncQueryServerSource = do
          let ChainSync.NodeClient{queryNode = queryLocalNodeState, nodeTip} = nodeClient
              ChainSync.Database.DatabaseQueries{..} = chainSyncDatabaseQueries
          chainSyncQueryServer $ ChainSyncQueryServerDependencies{..}
        chainSyncQueryConnector =
          ihoistConnector
            hoistQueryClient
            (runAppM eventBackend logAction)
            liftIO
            (directConnector serveQueryClient chainSyncQueryServerSource)

    liftIO do
      let test' = do
            -- Await for the chain indexer to process the scripts publishing
            -- transaction. The scripts should be available in the UTxOs and the
            -- wallet state which paid for publishing should be up to date.
            let MarloweScripts{marloweScriptUTxOs, payoutScriptUTxOs, helperScriptUTxOs} = marloweScripts
                txOutRefs = Set.fromList $ do
                  refsUTxOs <- [Map.elems marloweScriptUTxOs, Map.elems payoutScriptUTxOs, Map.elems helperScriptUTxOs]
                  ReferenceScriptUtxo{txOutRef} <- refsUTxOs
                  pure txOutRef
            waitForUTxOs chainSyncQueryConnector txOutRefs
            waitForWebServer 0
            runInIO (test MarloweRuntime{..})
      -- Persist the genesis block before starting the services so that they
      -- exist already and no database queries fail.
      runAppM eventBackend logAction $
        runCommitGenesisBlock (commitGenesisBlock chainIndexerDatabaseQueries) genesisBlock
      onException
        (runAppM eventBackend logAction testAction `race_` test')
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

    createDatabase dbName = do
      connection <- connectdb rootConnectionString
      result1 <- exec connection $ "CREATE DATABASE \"" <> dbName <> "\"" <> ";"
      result2 <- exec connection $ "GRANT ALL PRIVILEGES ON DATABASE \"" <> dbName <> "\" TO " <> databaseUser <> ";"
      checkResult connection result1
      checkResult connection result2
      finish connection

    cleanupDatabase retryDelay dbName = when cleanup do
      catch
        ( do
            connection <- connectdb rootConnectionString
            void $
              exec connection $
                " SELECT pg_terminate_backend(pg_stat_activity.pid) \
                \ FROM pg_stat_activity \
                \ WHERE pid <> pg_backend_pid() AND \
                \   pg_stat_activity.datname = \""
                  <> dbName
                  <> "\";"

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

newtype Attempts = Attempts Int

retryTillTrue :: (MonadFail m) => (MonadIO m) => Attempts -> m Bool -> m ()
retryTillTrue (Attempts n) action = do
  when (n <= 0) do
    fail "Exceeded number of attempts"
  let retryPolicy = constantDelay 1_000_000 <> limitRetries 60
  void $ retrying retryPolicy (\_ res -> return (not res)) $ const do
    action

-- ChainSync.GetUTxOs (ChainSync.GetUTxOsForTxOutRefs (Set TxOutRef))
queryUTxOs :: (Monad m) => Connector (QueryClient CS.ChainSyncQuery) m -> Set CS.TxOutRef -> m CS.UTxOs
queryUTxOs chainSyncQueryConnector txOutRefs = do
  runConnector chainSyncQueryConnector . Query.Client.request . ChainSync.GetUTxOs . ChainSync.GetUTxOsForTxOutRefs $
    txOutRefs

waitForUTxOs
  :: (MonadFail m, MonadIO m)
  => Connector (QueryClient CS.ChainSyncQuery) m
  -> Set CS.TxOutRef
  -> m ()
waitForUTxOs connector txOutRefs = do
  retryTillTrue (Attempts 60) do
    CS.UTxOs utxos <- queryUTxOs connector txOutRefs
    -- check if all the txOutRefs are present in the UTxOs
    pure $ txOutRefs `Set.isSubsetOf` Map.keysSet utxos

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

publishCurrentScripts :: LocalTestnet -> LocalNodeConnectInfo -> IO MarloweScripts
publishCurrentScripts LocalTestnet{..} localNodeConnectInfo = do
  let PaymentKeyPair{..} = head wallets
  signingKey <-
    SomePaymentSigningKeyGenesisUTxO
      . either (error . show) id
      . deserialiseFromTextEnvelope (AsSigningKey AsGenesisUTxOKey)
      . either error id
      <$> eitherDecodeFileStrict paymentSKey
  changeAddress <-
    either (error . show) (shelleyAddressInEra ShelleyBasedEraBabbage)
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
    flip runReaderT (CliEnv BabbageEraOnwardsBabbage) do
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
    openRoleValidatorInfo = snd mrOpenRoleValidator
    marloweScript = fromCardanoScriptHash $ viHash marloweValidatorInfo
    payoutScript = fromCardanoScriptHash $ viHash payoutValidatorInfo
    openRoleScript = fromCardanoScriptHash $ viHash openRoleValidatorInfo
    networkId = Testnet $ NetworkMagic $ fromIntegral testnetMagic
    marloweTxOutRef = fromCardanoTxIn $ fst $ unAnUTxO $ fst mrMarloweValidator
    payoutTxOutRef = fromCardanoTxIn $ fst $ unAnUTxO $ fst mrRolePayoutValidator
    openRoleTxOutRef = fromCardanoTxIn $ fst $ unAnUTxO $ fst mrOpenRoleValidator
    refScriptPublisher (AnUTxO (_, Cardano.TxOut addr _ _ _), _) = addr
    refScriptValue (AnUTxO (_, Cardano.TxOut _ value _ _), _) = Cardano.txOutValueToValue value
    helperScripts = Map.singleton OpenRoleScript openRoleScript

    marloweReferenceScriptUTxO =
      ReferenceScriptUtxo
        { txOutRef = marloweTxOutRef
        , txOut =
            TransactionOutput
              { address = fromCardanoAddressInEra BabbageEra $ refScriptPublisher mrMarloweValidator
              , assets = fold $ mkTxOutAssets $ assetsFromCardanoValue $ refScriptValue mrMarloweValidator
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
              , assets = fold $ mkTxOutAssets $ assetsFromCardanoValue $ refScriptValue mrRolePayoutValidator
              , datumHash = Nothing
              , datum = Nothing
              }
        , script = viScript payoutValidatorInfo
        }
    marloweScriptUTxOs = Map.singleton networkId marloweReferenceScriptUTxO
    payoutScriptUTxOs = Map.singleton networkId payoutReferenceScriptUTxO
    helperScriptUTxOs =
      Map.singleton
        (OpenRoleScript, networkId)
        $ ReferenceScriptUtxo
          { txOutRef = openRoleTxOutRef
          , txOut =
              TransactionOutput
                { address = fromCardanoAddressInEra BabbageEra $ refScriptPublisher mrOpenRoleValidator
                , assets = fold $ mkTxOutAssets $ assetsFromCardanoValue $ refScriptValue mrOpenRoleValidator
                , datumHash = Nothing
                , datum = Nothing
                }
          , script = viScript openRoleValidatorInfo
          }

data TestContainerSelector f where
  RootSelector :: Runtime.Logging.RootSelector f -> TestContainerSelector f
  ServeRequest :: Web.Server.ServeRequest f -> TestContainerSelector f

instance Inject TestContainerSelector TestContainerSelector where
  inject = idInjectSelector

instance Inject RootSelector TestContainerSelector where
  inject = injectSelector RootSelector

instance Inject Web.Server.ServeRequest TestContainerSelector where
  inject = injectSelector ServeRequest

instance {-# OVERLAPPABLE #-} (Inject event RootSelector) => Inject event TestContainerSelector where
  inject = composeInjectSelector (injectSelector RootSelector) inject

newtype DbName = DbName ByteString

renderTestContainerSelector :: forall s. PortNumber -> DbName -> TestContainerSelector s -> OTelRendered s
renderTestContainerSelector portNumber (DbName dbName) = \case
  RootSelector sel ->
    Runtime.Logging.renderRootSelectorOTel
      (Just dbName)
      Nothing
      Nothing
      Nothing
      sel
  ServeRequest sel -> renderServeRequestOTel portNumber sel

data TestContainerDependencies m = TestContainerDependencies
  { chainIndexerDatabaseQueries :: ChainIndexer.DatabaseQueries m
  , chainSyncDatabaseQueries :: ChainSync.Database.DatabaseQueries m
  , contractStore :: ContractStore m
  , genesisBlock :: GenesisBlock
  , marloweIndexerDatabaseQueries :: Indexer.DatabaseQueries m
  , marloweSyncDatabaseQueries :: Sync.DatabaseQueries m
  , submitConfirmationBlocks :: BlockNo
  , networkId :: NetworkId
  , localNodeConnectInfo :: LocalNodeConnectInfo
  , securityParameter :: Int
  , marloweScripts :: MarloweScripts
  , webPort :: Int
  , proxyPort :: Int
  }

testContainer
  :: Component
      (AppM Span TestContainerSelector)
      (TestContainerDependencies (AppM Span TestContainerSelector))
      ( Runtime.MarloweRuntime (AppM Span TestContainerSelector)
      , ChainSync.NodeClient (AppM Span TestContainerSelector)
      )
testContainer = proc TestContainerDependencies{..} -> do
  let getScripts :: MarloweVersion v -> Set MarloweScripts
      getScripts MarloweV1 = Set.singleton marloweScripts

      getCurrentScripts :: MarloweVersion v -> MarloweScripts
      getCurrentScripts MarloweV1 = marloweScripts

      connectToLocalNode = liftIO . Cardano.connectToLocalNode localNodeConnectInfo

  runtime@Runtime.MarloweRuntime{..} <-
    marloweRuntime
      -<
        let maxCost = 100_000
            costModel = CostModel 1 10
            persistRateLimit = 0.1
            batchSize = unsafeIntToNat 10
            marloweScriptHashes = NESet.singleton $ marloweScript marloweScripts
            payoutScriptHashes = NESet.singleton $ payoutScript marloweScripts
            pollingInterval = 0.01
            confirmationTimeout = 60
            runtimeVersion = Version [0] []
            indexParties = pure ()
            mkRoleTokenMintingPolicy = mkCommandLineRoleTokenMintingPolicy "marlowe-minting-validator"
         in MarloweRuntimeDependencies{..}

  nodeClient <-
    unnestNodeClient <$> supervisor "node-client" ChainSync.nodeClient
      -<
        ChainSync.NodeClientDependencies connectToLocalNode

  tcpServer "marlowe-runtime"
    -<
      TcpServerDependencies "127.0.0.1" (fromIntegral proxyPort) serverSource marloweRuntimeServerDirectPeer

  runtimeServer
    -<
      ServerDependencies
        { openAPIEnabled = False
        , accessControlAllowOriginAll = False
        , runApplication = run webPort
        , connector = directConnector serveMarloweRuntimeClientDirect serverSource
        }

  returnA -< (runtime, nodeClient)
