-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Marlowe test DSL main runner.
--
-----------------------------------------------------------------------------

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE LambdaCase #-}

module Language.Marlowe.CLI.Test
  ( -- * Testing
    runTestSuite
  ) where


import Cardano.Api
  ( ConsensusModeParams(CardanoModeParams)
  , EpochSlots(..)
  , IsShelleyBasedEra
  , Key(getVerificationKey, verificationKeyHash)
  , LocalNodeConnectInfo(..)
  , Lovelace(Lovelace)
  , ScriptDataSupportedInEra
  )
import qualified Cardano.Api as C
import Cardano.Api.Shelley (protocolParamProtocolVersion)
import Contrib.Cardano.Api (lovelaceFromInt, lovelaceToInt)
import Contrib.Control.Concurrent.Async (altIO)
import Contrib.Control.Monad.Trans.State.IO (IOStateT, unsafeExecIOStateT)
import Contrib.Data.Foldable (foldMapMFlipped)
import Contrib.Monad.Loops (MaxRetries(MaxRetries), RetryCounter(RetryCounter), retryTillJust, retryTillRight)
import Control.Concurrent.Async (race)
import Control.Concurrent.Async.Pool (mapTasks, mapTasks_, withTaskGroup)
import Control.Concurrent.STM
  (TVar, atomically, modifyTVar', newTVar, newTVarIO, readTVar, readTVarIO, retry, writeTVar)
import Control.Error (ExceptT, hush, note)
import Control.Lens (At(at), Bifunctor(bimap), Each(each), _2, _Just, coerced, non, traversed, (^.), (^..))
import qualified Control.Lens as L
import qualified Control.Lens as Lens
import Control.Monad (replicateM, void, when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (MonadError, MonadIO, catchError, liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT(runReaderT), runReader)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State (execStateT)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson.OneLine as A
import qualified Data.ByteString.Lazy as B
import Data.Coerce (coerce)
import Data.Default (Default(def))
import Data.Foldable (for_, toList)
import Data.Functor ((<&>))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as S (singleton)
import qualified Data.Text as Text
import Data.Time.Units (Second, TimeUnit(toMicroseconds))
import Data.Traversable (for)
import GHC.IO (bracket)
import GHC.IO.Handle.FD (stderr)
import Language.Marlowe.CLI.Cardano.Api (toPlutusProtocolVersion, txOutValueValue)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (IsPlutusScriptLanguage)
import Language.Marlowe.CLI.Cardano.Api.Value (toPlutusValue)
import qualified Language.Marlowe.CLI.Cardano.Api.Value as CV
import Language.Marlowe.CLI.IO (decodeFileStrict, getDefaultCostModel, queryInEra, readSigningKey)
import Language.Marlowe.CLI.Test.CLI.Types
  (CLIContractInfo(CLIContractInfo), CLIContracts(CLIContracts), CLITxInfo, ciPlan, ciThread, unCLIContracts)
import Language.Marlowe.CLI.Test.ExecutionMode (ExecutionMode(OnChainMode), toSubmitMode)
import Language.Marlowe.CLI.Test.Interpret (interpret)
import Language.Marlowe.CLI.Test.Log (logLabeledMsg)
import Language.Marlowe.CLI.Test.Runner
  (Env(..), TestSuiteResult(TestSuiteResult), TestSuiteRunnerEnv, runTests, testSuiteResultToJSON, tsResult)
import qualified Language.Marlowe.CLI.Test.Runtime.Monitor as Runtime.Monitor
import Language.Marlowe.CLI.Test.Runtime.Types (RuntimeError(RuntimeRollbackError))
import qualified Language.Marlowe.CLI.Test.Runtime.Types as R
import qualified Language.Marlowe.CLI.Test.Runtime.Types as Runtime
import qualified Language.Marlowe.CLI.Test.Runtime.Types as Runtime.Monitor
import qualified Language.Marlowe.CLI.Test.TestCase as TestCase
import Language.Marlowe.CLI.Test.Types
  ( ConcurrentRunners(ConcurrentRunners)
  , FailureReport(FailureReport)
  , InterpretEnv(..)
  , InterpretState(..)
  , ReportingStrategy(..)
  , RuntimeConfig(..)
  , TestCase(..)
  , TestName(TestName)
  , TestResult(TestFailed, TestSucceeded)
  , TestSuite(..)
  , failureReportToJSON
  , ieConnection
  , ieEra
  , ieExecutionMode
  , iePrintStats
  , isCLIContracts
  , isCurrencies
  , isKnownContracts
  , isLogs
  , isWallets
  , testResultToJSON
  )
import Language.Marlowe.CLI.Test.Wallet.Interpret (createWallet)
import Language.Marlowe.CLI.Test.Wallet.Types
  ( Currencies(Currencies)
  , Currency(Currency)
  , CurrencyNickname(CurrencyNickname)
  , Wallet(Wallet, _waAddress, _waBalanceCheckBaseline)
  , WalletNickname(WalletNickname)
  , waSubmittedTransactions
  )
import qualified Language.Marlowe.CLI.Test.Wallet.Types as Wallet
import Language.Marlowe.CLI.Transaction
  (buildBodyWithContent, buildFaucetImpl, buildMintingImpl, querySlotConfig, queryUtxos, submitBody')
import Language.Marlowe.CLI.Types
  ( CliEnv(CliEnv)
  , CliError(..)
  , MarlowePlutusVersion
  , MarloweTransaction(MarloweTransaction)
  , PayFromScript(PayFromScript)
  , PrintStats(PrintStats)
  , SigningKeyFile(SigningKeyFile)
  , SomePaymentSigningKey
  , defaultCoinSelectionStrategy
  )
import qualified Language.Marlowe.CLI.Types as T
import Language.Marlowe.Cardano.Thread (marloweThreadTxInfos, overAnyMarloweThread)
import qualified Language.Marlowe.Protocol.Client as Marlowe.Protocol
import qualified Language.Marlowe.Protocol.Types as Marlowe.Protocol
import qualified Language.Marlowe.Runtime.App.Types as Apps
import qualified Network.Protocol.Connection as Network.Protocol
import qualified Network.Protocol.Driver as Network.Protocol
import qualified Network.Protocol.Handshake.Client as Network.Protocol
import Network.Protocol.Handshake.Types (Handshake)
import Observe.Event.Render.JSON (defaultRenderSelectorJSON)
import Observe.Event.Render.JSON.Handle (simpleJsonStderrBackend)
import qualified Plutus.V1.Ledger.Ada as PV
import qualified Plutus.V1.Ledger.Value as PV
import PlutusCore (defaultCostModelParams)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPrint, hPutStrLn)

-- | Run tests of a Marlowe contract.
runTestSuite
  :: forall era m
   . IsShelleyBasedEra era
  => MonadError CliError m
  => MonadReader (CliEnv era) m
  => MonadIO m
  => ScriptDataSupportedInEra era
  -> TestSuite era FilePath  -- ^ The tests.
  -> m ()                   -- ^ Action for running the tests.
runTestSuite era TestSuite{..} = do
  faucetSigningKey <- readSigningKey (SigningKeyFile tsFaucetSigningKeyFile)
  tests <- do
    let
      step fileName = do
        content <- decodeFileStrict fileName `catchError` \e ->
          throwError $ CliError $ "Failed to decode test case file: " <> fileName <> " " <> show e
        pure (fileName, content)
    mapM step tsTests

  let
    printStats = PrintStats True
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

  let
    env :: TestSuiteRunnerEnv MarlowePlutusVersion era
    env = Env
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

  let
    writeReportFile filePath = do
      let
        json = testSuiteResultToJSON testSuiteResult
      liftIO $ B.writeFile filePath (A.encodePretty json)

  for_ tsReportingStrategy \case
    StreamAndWriteJSONFile filePath -> writeReportFile filePath
    WriteJSONFile filePath -> writeReportFile filePath
    StreamJSON -> pure ()

  let
    failures = do
      let
        testResults = Map.toList $ testSuiteResult ^. tsResult
        failedTestName = \case
          ((name, _), TestFailed {}) -> Just name
          _ -> Nothing
      mapMaybe failedTestName testResults
  if not (null failures)
    then liftIO $ do
      hPutStrLn stderr $ "Some tests failed: " <> intercalate ", " failures
      exitFailure
    else liftIO do
      hPutStrLn stderr "All tests succeeded."
      exitSuccess

