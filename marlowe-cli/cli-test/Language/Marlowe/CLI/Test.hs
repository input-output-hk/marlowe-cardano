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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Marlowe test DSL main runner.
module Language.Marlowe.CLI.Test (
  -- * Testing
  runTestSuite,
) where

import Cardano.Api (
  ConsensusModeParams (CardanoModeParams),
  EpochSlots (..),
  IsShelleyBasedEra,
  Key (getVerificationKey, verificationKeyHash),
  LocalNodeConnectInfo (..),
  Lovelace (Lovelace),
  ScriptDataSupportedInEra,
 )
import Cardano.Api qualified as C
import Cardano.Api.Shelley (protocolParamProtocolVersion)
import Contrib.Cardano.Api (lovelaceFromInt, lovelaceToInt)
import Contrib.Control.Concurrent.Async (altIO)
import Contrib.Control.Monad.Trans.State.IO (IOStateT, unsafeExecIOStateT)
import Contrib.Data.Foldable (foldMapMFlipped)
import Contrib.Monad.Loops (MaxRetries (MaxRetries), RetryCounter (RetryCounter), retryTillJust, retryTillRight)
import Control.Concurrent.Async (race)
import Control.Concurrent.Async.Pool (mapTasks, mapTasks_, withTaskGroup)
import Control.Concurrent.STM (
  TVar,
  atomically,
  modifyTVar',
  newTVar,
  newTVarIO,
  readTVar,
  readTVarIO,
  retry,
  writeTVar,
 )
import Control.Error (ExceptT, hush, note)
import Control.Lens (At (at), Bifunctor (bimap), Each (each), coerced, non, traversed, (^.), (^..), _2, _Just)
import Control.Lens qualified as L
import Control.Lens qualified as Lens
import Control.Monad (replicateM, void, when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (MonadError, MonadIO, catchError, liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (runReaderT), runReader)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State (execStateT)
import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty qualified as A
import Data.Aeson.OneLine qualified as A
import Data.ByteString.Lazy qualified as B
import Data.Coerce (coerce)
import Data.Default (Default (def))
import Data.Foldable (for_, toList)
import Data.Functor ((<&>))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as S (singleton)
import Data.Text qualified as Text
import Data.Time.Units (Second, TimeUnit (toMicroseconds))
import Data.Traversable (for)
import GHC.IO (bracket)
import GHC.IO.Handle.FD (stderr)
import Language.Marlowe.CLI.Cardano.Api (toPlutusProtocolVersion, txOutValueValue)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (IsPlutusScriptLanguage)
import Language.Marlowe.CLI.Cardano.Api.Value (toPlutusValue)
import Language.Marlowe.CLI.Cardano.Api.Value qualified as CV
import Language.Marlowe.CLI.IO (decodeFileStrict, getDefaultCostModel, queryInEra, readSigningKey)
import Language.Marlowe.CLI.Test.CLI.Types (
  CLIContractInfo (CLIContractInfo),
  CLIContracts (CLIContracts),
  CLITxInfo,
  ciPlan,
  ciThread,
  unCLIContracts,
 )
import Language.Marlowe.CLI.Test.ExecutionMode (ExecutionMode (OnChainMode), toSubmitMode)
import Language.Marlowe.CLI.Test.Interpret (interpret)
import Language.Marlowe.CLI.Test.Log (logLabeledMsg)
import Language.Marlowe.CLI.Test.Runner (
  Env (..),
  TestSuiteResult (TestSuiteResult),
  TestSuiteRunnerEnv,
  runTests,
  testSuiteResultToJSON,
  tsResult,
 )
import Language.Marlowe.CLI.Test.Runtime.Monitor qualified as Runtime.Monitor
import Language.Marlowe.CLI.Test.Runtime.Types (RuntimeError (RuntimeRollbackError))
import Language.Marlowe.CLI.Test.Runtime.Types qualified as R
import Language.Marlowe.CLI.Test.Runtime.Types qualified as Runtime
import Language.Marlowe.CLI.Test.Runtime.Types qualified as Runtime.Monitor
import Language.Marlowe.CLI.Test.TestCase qualified as TestCase
import Language.Marlowe.CLI.Test.Types (
  ConcurrentRunners (ConcurrentRunners),
  FailureReport (FailureReport),
  InterpretEnv (..),
  InterpretState (..),
  ReportingStrategy (..),
  RuntimeConfig (..),
  TestCase (..),
  TestName (TestName),
  TestResult (TestFailed, TestSucceeded),
  TestSuite (..),
  failureReportToJSON,
  ieConnection,
  ieEra,
  ieExecutionMode,
  iePrintStats,
  isCLIContracts,
  isCurrencies,
  isKnownContracts,
  isLogs,
  isWallets,
  testResultToJSON,
 )
import Language.Marlowe.CLI.Test.Wallet.Interpret (createWallet)
import Language.Marlowe.CLI.Test.Wallet.Types (
  Currencies (Currencies),
  Currency (Currency),
  CurrencyNickname (CurrencyNickname),
  Wallet (Wallet, _waAddress, _waBalanceCheckBaseline),
  WalletNickname (WalletNickname),
  waSubmittedTransactions,
 )
import Language.Marlowe.CLI.Test.Wallet.Types qualified as Wallet
import Language.Marlowe.CLI.Transaction (
  buildBodyWithContent,
  buildFaucetImpl,
  buildMintingImpl,
  querySlotConfig,
  queryUtxos,
  submitBody',
 )
import Language.Marlowe.CLI.Types (
  CliEnv (CliEnv),
  CliError (..),
  MarlowePlutusVersion,
  MarloweTransaction (MarloweTransaction),
  PayFromScript (PayFromScript),
  PrintStats (PrintStats),
  SigningKeyFile (SigningKeyFile),
  SomePaymentSigningKey,
  defaultCoinSelectionStrategy,
 )
import Language.Marlowe.CLI.Types qualified as T
import Language.Marlowe.Cardano.Thread (marloweThreadTxInfos, overAnyMarloweThread)
import Language.Marlowe.Protocol.Client qualified as Marlowe.Protocol
import Language.Marlowe.Protocol.Types qualified as Marlowe.Protocol
import Language.Marlowe.Runtime.App.Types qualified as Apps
import Network.Protocol.Connection qualified as Network.Protocol
import Network.Protocol.Driver qualified as Network.Protocol
import Network.Protocol.Handshake.Client qualified as Network.Protocol
import Network.Protocol.Handshake.Types (Handshake)
import Observe.Event.Render.JSON (defaultRenderSelectorJSON)
import Observe.Event.Render.JSON.Handle (simpleJsonStderrBackend)
import Plutus.V1.Ledger.Ada qualified as PV
import Plutus.V1.Ledger.Value qualified as PV
import PlutusCore (defaultCostModelParams)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPrint, hPutStrLn)

-- | Run tests of a Marlowe contract.
runTestSuite
  :: forall era m
   . (IsShelleyBasedEra era)
  => (MonadError CliError m)
  => (MonadReader (CliEnv era) m)
  => (MonadIO m)
  => ScriptDataSupportedInEra era
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
          { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
          , localNodeNetworkId = tsNetwork
          , localNodeSocketPath = tsSocketPath
          }
      resource = (tsFaucetAddress, faucetSigningKey)

  protocolParameters <- queryInEra connection C.QueryProtocolParameters
  slotConfig <- querySlotConfig connection
  costModel <- getDefaultCostModel

  let env :: TestSuiteRunnerEnv MarlowePlutusVersion era
      env =
        Env
          { _envEra = era
          , _envConnection = connection
          , _envSlotConfig = slotConfig
          , _envProtocolParams = protocolParameters
          , _envCostModelParams = costModel
          , _envExecutionMode = tsExecutionMode
          , _envResource = resource
          , _envRuntimeConfig = Just tsRuntime
          , _envPrintStats = printStats
          , _envStreamJSON = True
          , _envMaxRetries = tsMaxRetries
          }

  testSuiteResult <- liftIO $ flip runReaderT env $ runTests tests tsConcurrentRunners

  let writeReportFile filePath = do
        let json = testSuiteResultToJSON testSuiteResult
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
