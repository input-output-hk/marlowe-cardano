-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Marlowe test DSL main runner.
module Language.Marlowe.CLI.Test (
  -- * Types
  FundingFaucet (..),

  -- * Testing
  runTestSuite,
  runOperations,
) where

import Cardano.Api (
  BabbageEraOnwards,
  CardanoEra,
  ConsensusModeParams (CardanoModeParams),
  EpochSlots (..),
  File (..),
  IsCardanoEra (cardanoEra),
  IsShelleyBasedEra,
  Key (verificationKeyHash),
  LocalNodeConnectInfo (..),
  PaymentCredential (PaymentCredentialByKey),
  StakeAddressReference (NoStakeAddress),
  makeShelleyAddressInEra,
 )
import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as CS
import Control.Concurrent.STM (
  newTVarIO,
 )
import Control.Error (ExceptT (ExceptT))
import Control.Lens ((^.))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (MonadError, MonadIO, catchError, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Reader.Class (MonadReader (..))
import Data.Aeson.Encode.Pretty qualified as A
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as B
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import GHC.IO.Handle.FD (stderr)
import Language.Marlowe.CLI.IO (
  decodeFileStrict,
  getEraHistory,
  getPV2CostModelParams,
  getProtocolParams,
  getSystemStart,
  liftCli,
  queryInEra,
  readSigningKey,
 )
import Language.Marlowe.CLI.Test.ExecutionMode (
  UseExecutionMode (UseOnChainMode, UseSimulationMode),
 )
import Language.Marlowe.CLI.Test.Runner (
  Env (..),
  TestSuiteRunnerEnv,
  runTests,
  testSuiteResultToJSON,
  tsResult,
 )
import Language.Marlowe.CLI.Test.Types (
  MaxConcurrentRunners (MaxConcurrentRunners),
  ReportingStrategy (..),
  RuntimeConfig,
  TestCase (TestCase),
  TestName (TestName),
  TestOperation,
  TestResult (TestFailed),
  TestSuite (..),
 )
import Language.Marlowe.CLI.Transaction (
  mkTxOutValue,
  querySlotConfig,
 )
import Language.Marlowe.CLI.Types (
  CliEnv (CliEnv),
  CliError (..),
  MarlowePlutusVersion,
  NodeStateInfo (..),
  PrintStats (PrintStats),
  QueryExecutionContext (..),
  SigningKeyFile (SigningKeyFile),
  SomePaymentSigningKey,
  TxBuildupContext (..),
  getVerificationKey,
  toPaymentVerificationKey,
 )
import Language.Marlowe.CLI.Types qualified as T
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn)
import System.IO.Temp (createTempDirectory)

-- | Run tests of a Marlowe contract.
runTestSuite
  :: forall era m
   . (IsShelleyBasedEra era)
  => (MonadError CliError m)
  => (MonadReader (CliEnv era) m)
  => (MonadIO m)
  => BabbageEraOnwards era
  -> TestSuite era FilePath
  -- ^ The tests.
  -> m ()
  -- ^ Action for running the tests.
runTestSuite era TestSuite{..} = do
  faucetSigningKey <- readSigningKey (SigningKeyFile tsFaucetSigningKeyFile)
  tests <- do
    let step fileName = do
          content <-
            decodeFileStrict fileName `catchError` \e ->
              throwError $ CliError $ "Failed to decode test case file: " <> fileName <> " " <> show e
          pure (fileName, content)
    mapM step tsTests

  let printStats = PrintStats True
      connection =
        LocalNodeConnectInfo
          { localConsensusModeParams = CardanoModeParams $ EpochSlots 21_600
          , localNodeNetworkId = tsNetwork
          , localNodeSocketPath = File tsSocketPath
          }
      resource = (tsFaucetAddress, faucetSigningKey)

  txBuildupContext <- do
    case tsUseExecutionMode of
      UseSimulationMode -> do
        txId <-
          liftCli $
            C.deserialiseFromRawBytesHex C.AsTxId $
              BS8.pack "b1c9a36fff21ab3941e63e3ff15351a2f4459d8e055521a4b581044789e3a0db"
        let txIx = C.TxIx 0
            txIn = C.TxIn txId txIx
            txOutValue = mkTxOutValue era $ C.lovelaceToValue $ Ledger.Coin 1000_000 * 1000_000
            txOut = C.TxOut tsFaucetAddress txOutValue C.TxOutDatumNone CS.ReferenceScriptNone
            utxo = C.UTxO $ Map.singleton txIn txOut
            queryCtx = QueryNode connection
        utxoVar <- liftIO $ newTVarIO utxo
        protocolParams <- getProtocolParams queryCtx
        systemStart <- getSystemStart queryCtx
        eraHistory <- getEraHistory queryCtx

        let nodeStateInfo =
              NodeStateInfo
                tsNetwork
                protocolParams
                systemStart
                eraHistory

        pure $ PureTxBuildup utxoVar nodeStateInfo
      UseOnChainMode timeout ->
        pure $ NodeTxBuildup connection (T.DoSubmit timeout)

  protocolParameters <-
    C.fromLedgerPParams (C.babbageEraOnwardsToShelleyBasedEra era) <$> queryInEra connection C.QueryProtocolParameters
  slotConfig <- querySlotConfig connection
  costModel <- getPV2CostModelParams (QueryNode connection)
  -- FIXME: This should be configurable.
  reportDir <- liftIO $ createTempDirectory "/tmp" "marlowe-cli-test-report"

  let env :: TestSuiteRunnerEnv MarlowePlutusVersion era
      env =
        Env
          { _envEra = era
          , _envConnection = connection
          , _envSlotConfig = slotConfig
          , _envProtocolParams = protocolParameters
          , _envCostModelParams = costModel
          , _envTxBuildupContext = txBuildupContext
          , _envResource = resource
          , _envRuntimeConfig = Just tsRuntime
          , _envPrintStats = printStats
          , _envStreamJSON = True
          , _envMaxRetries = tsMaxRetries
          , _envReportDir = reportDir
          }

  testSuiteResult <- liftIO $ flip runReaderT env $ runTests tests tsConcurrentRunners

  let writeReportFile filePath = do
        json <- testSuiteResultToJSON testSuiteResult
        liftIO $ B.writeFile filePath (A.encodePretty json)

  for_ tsReportingStrategy \case
    StreamAndWriteJSONFile filePath -> writeReportFile filePath
    WriteJSONFile filePath -> writeReportFile filePath
    StreamJSON -> pure ()

  let failures = do
        let testResults = Map.toList $ testSuiteResult ^. tsResult
            failedTestName = \case
              ((name, _), TestFailed{}) -> Just name
              _ -> Nothing
        mapMaybe failedTestName testResults
  if not (null failures)
    then liftIO $ do
      hPutStrLn stderr $ "Some tests failed: " <> intercalate ", " failures
      exitFailure
    else liftIO do
      hPutStrLn stderr "All tests succeeded."
      exitSuccess

mkPureTxBuildupContext
  :: forall era m
   . (MonadIO m)
  => (IsCardanoEra era)
  => (MonadError CliError m)
  => LocalNodeConnectInfo
  -> C.AddressInEra era
  -> m (TxBuildupContext era)
mkPureTxBuildupContext connection faucetAddress = do
  let onOldEra _ = throwError "Unsupported era..."
      onBabbageEraOnwards era = do
        txId <-
          liftCli $
            C.deserialiseFromRawBytesHex C.AsTxId $
              BS8.pack "b1c9a36fff21ab3941e63e3ff15351a2f4459d8e055521a4b581044789e3a0db"
        let txIx = C.TxIx 0
            txIn = C.TxIn txId txIx
            -- era' = toMultiAssetSupportedInEra era
            -- txOutValue = C.TxOutValue era' $ C.lovelaceToValue $ Ledger.Coin 1000_000 * 1000_000
            txOutValue = mkTxOutValue era $ C.lovelaceToValue $ Ledger.Coin 1000_000 * 1000_000
            txOut = C.TxOut faucetAddress txOutValue C.TxOutDatumNone CS.ReferenceScriptNone
            utxo = C.UTxO $ Map.singleton txIn txOut
            LocalNodeConnectInfo _ networkId _ = connection
            queryCtx = QueryNode connection
        utxoVar <- liftIO $ newTVarIO utxo
        (protocolParameters, systemStart, eraHistory) <- flip runReaderT (CliEnv era) do
          pp <- getProtocolParams queryCtx
          ss <- getSystemStart queryCtx
          eh <- getEraHistory queryCtx
          pure (pp, ss, eh)

        let nodeStateInfo =
              NodeStateInfo
                networkId
                protocolParameters
                systemStart
                eraHistory
        pure $ PureTxBuildup utxoVar nodeStateInfo

  C.caseByronToAlonzoOrBabbageEraOnwards onOldEra onBabbageEraOnwards (cardanoEra :: CardanoEra era)

newtype MaxRetries = MaxRetries Int

mkTestSuiteRunnerEnv
  :: forall era m
   . (MonadIO m)
  => (IsCardanoEra era)
  => LocalNodeConnectInfo
  -> UseExecutionMode
  -> CS.AddressInEra era
  -> SomePaymentSigningKey
  -> MaxRetries
  -> PrintStats
  -> Maybe RuntimeConfig
  -> m (Either CliError (TestSuiteRunnerEnv MarlowePlutusVersion era))
mkTestSuiteRunnerEnv connection useExecutionMode faucetAddress faucetSigningKey (MaxRetries maxRetries) printStats runtimeConfig = runExceptT do
  txBuildupContext <- case useExecutionMode of
    UseSimulationMode -> mkPureTxBuildupContext connection faucetAddress
    UseOnChainMode timeout -> pure $ NodeTxBuildup connection (T.DoSubmit timeout)

  let onOldEra _ = throwError "Unsupported era..."
      onBabbageEraOnwards era = do
        let reportDir = "/tmp"
            resource = (faucetAddress, faucetSigningKey)
            shelleyBasedEra = C.babbageEraOnwardsToShelleyBasedEra era

        (costModel, protocolParameters, slotConfig) <- flip runReaderT (CliEnv era) do
          sc <- querySlotConfig connection
          cm <- getPV2CostModelParams (QueryNode connection)
          pp <- C.fromLedgerPParams shelleyBasedEra <$> queryInEra connection C.QueryProtocolParameters
          pure (cm, pp, sc)

        pure $
          Env
            { _envEra = era
            , _envConnection = connection
            , _envSlotConfig = slotConfig
            , _envProtocolParams = protocolParameters
            , _envCostModelParams = costModel
            , _envTxBuildupContext = txBuildupContext
            , _envResource = resource
            , _envRuntimeConfig = runtimeConfig
            , _envPrintStats = printStats
            , _envMaxRetries = maxRetries
            , _envReportDir = reportDir
            , _envStreamJSON = True
            }
  C.caseByronToAlonzoOrBabbageEraOnwards onOldEra onBabbageEraOnwards (cardanoEra :: CardanoEra era)

newtype FundingFaucet = FundingFaucet SomePaymentSigningKey

-- | Simple runner for CLI operations.
-- | TODO: Simplify this approach - this spawns a subthread and creates subfaucet which is not necessary for a single threaded operation runner.
runOperations
  :: forall era
   . (IsShelleyBasedEra era)
  => LocalNodeConnectInfo
  -> UseExecutionMode
  -> FundingFaucet
  -> [TestOperation]
  -> IO (Either CliError (TestResult MarlowePlutusVersion era))
runOperations connection useExecutionMode (FundingFaucet faucetSigningKey) operations = do
  let maxRetries = 1
      printStats = PrintStats False
      runtimeConfig = Nothing

  let LocalNodeConnectInfo _ networkId _ = connection
      era = C.shelleyBasedEra :: C.ShelleyBasedEra era
      faucetAddress :: C.AddressInEra era = makeShelleyAddressInEra era networkId (PaymentCredentialByKey (verificationKeyHash vkey)) NoStakeAddress
      vkey = toPaymentVerificationKey $ getVerificationKey faucetSigningKey

  print $ "Faucet address: " <> C.serialiseAddress faucetAddress

  runExceptT $ do
    env <-
      ExceptT $
        mkTestSuiteRunnerEnv
          connection
          useExecutionMode
          faucetAddress
          faucetSigningKey
          (MaxRetries maxRetries)
          printStats
          runtimeConfig
    let test = TestCase (TestName "test") operations
    testSuiteResult <- liftIO $ flip runReaderT env do
      runTests [("test", test)] (MaxConcurrentRunners 1)
    let resultsMap = testSuiteResult ^. tsResult
    case Map.toList resultsMap of
      [(_, result)] -> pure result
      _ -> throwError "Expected a single test result"
