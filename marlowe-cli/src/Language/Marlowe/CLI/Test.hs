-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Testing Marlowe contracts.
--
-----------------------------------------------------------------------------


{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE NamedFieldPuns #-}


module Language.Marlowe.CLI.Test
  ( -- * Testing
    runTests
  ) where


import Cardano.Api
  ( ConsensusModeParams(CardanoModeParams)
  , EpochSlots(..)
  , IsShelleyBasedEra
  , Key(getVerificationKey, verificationKeyHash)
  , LocalNodeConnectInfo(..)
  , ScriptDataSupportedInEra
  )
import qualified Cardano.Api as C
import Cardano.Api.Shelley (protocolParamProtocolVersion)
import Contrib.Control.Concurrent.Async (altIO)
import Control.Error (hush)
import Control.Lens (Bifunctor(bimap))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (MonadError, MonadIO, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT(runReaderT), runReader)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State (execStateT)
import Data.Default (Default(def))
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import Language.Marlowe.CLI.Cardano.Api (toPlutusProtocolVersion)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (IsPlutusScriptLanguage)
import Language.Marlowe.CLI.IO (decodeFileStrict, getDefaultCostModel, queryInEra, readSigningKey)
import Language.Marlowe.CLI.Test.CLI.Types (CLIContracts(CLIContracts))
import Language.Marlowe.CLI.Test.ExecutionMode (ExecutionMode)
import Language.Marlowe.CLI.Test.Interpret (interpret)
import Language.Marlowe.CLI.Test.Log (logLabeledMsg)
import qualified Language.Marlowe.CLI.Test.Runtime.Monitor as Runtime.Monitor
import qualified Language.Marlowe.CLI.Test.Runtime.Types as Runtime
import qualified Language.Marlowe.CLI.Test.Runtime.Types as Runtime.Monitor
import Language.Marlowe.CLI.Test.Types
  (InterpretEnv(..), InterpretState(..), TestCase(..), TestResult(TestFailed, TestSucceeded), TestSuite(..))
import Language.Marlowe.CLI.Test.Wallet.Types (Currencies(Currencies), Wallet)
import qualified Language.Marlowe.CLI.Test.Wallet.Types as Wallet
import Language.Marlowe.CLI.Transaction (querySlotConfig, queryUtxos)
import Language.Marlowe.CLI.Types
  (CliEnv(CliEnv), CliError(..), MarlowePlutusVersion, PrintStats(PrintStats), SigningKeyFile(SigningKeyFile))
import qualified Language.Marlowe.CLI.Types as T
import qualified Language.Marlowe.Runtime.App.Types as Apps
import Observe.Event.Render.JSON (defaultRenderSelectorJSON)
import Observe.Event.Render.JSON.Handle (simpleJsonStderrBackend)
import PlutusCore (defaultCostModelParams)


mkWallet connection address signingKey = do
  utxos <- queryUtxos connection address
  pure $ Wallet.fromUTxO address signingKey utxos

-- | Run tests of a Marlowe contract.
runTests
  :: forall era m
   . IsShelleyBasedEra era
  => MonadError CliError m
  => MonadReader (CliEnv era) m
  => MonadIO m
  => ScriptDataSupportedInEra era
  -> TestSuite era FilePath  -- ^ The tests.
  -> m ()                   -- ^ Action for running the tests.
runTests era TestSuite{..} =
  do
    costModel <- getDefaultCostModel
    let
      runCli action = runReaderT action (CliEnv era)
      connection =
        LocalNodeConnectInfo
        {
          localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
        , localNodeNetworkId       = stNetwork
        , localNodeSocketPath      = stSocketPath
        }
    protocol <- runCli $ queryInEra connection C.QueryProtocolParameters
    faucetSigningKey <- readSigningKey (SigningKeyFile stFaucetSigningKeyFile)
    faucet <- mkWallet connection stFaucetAddress faucetSigningKey
    slotConfig <- runCli $ querySlotConfig connection
    tests' <- mapM decodeFileStrict stTests
    let
      printStats = PrintStats True
      protocolVersion = toPlutusProtocolVersion $ protocolParamProtocolVersion protocol

    for_ tests' \testCase@TestCase { testName } -> do
      liftIO $ putStrLn ""
      liftIO . putStrLn $ "***** Test " <> show testName <> " *****"

      let
        config = def
          { -- chainSeekHost = "127.0.0.1"
            Apps.chainSeekSyncPort = 32778 -- 3715
          , Apps.chainSeekCommandPort = 32776 -- 3720
          , Apps.runtimePort = 32782 -- 3700
          -- , Apps.runtimeHost = "127.0.0.1"
          -- , timeoutSeconds = 900
          -- , buildSeconds = 3
          -- , confirmSeconds = 3
          -- , retrySeconds = 10
          -- , retryLimit = 5
          }
      (runtimeMonitorInput, runtimeMonitorContracts, runtimeMonitor) <- liftIO $ Runtime.Monitor.mkRuntimeMonitor config
      let
        env :: InterpretEnv MarlowePlutusVersion era
        env = InterpretEnv
          { _ieConnection=connection
          , _ieCostModelParams=costModel
          , _ieEra=era
          , _ieProtocolVersion=protocolVersion
          , _ieSlotConfig=slotConfig
          , _ieExecutionMode=stExecutionMode
          , _iePrintStats=printStats
          , _ieRuntimeMonitor = Just (runtimeMonitorInput, runtimeMonitorContracts)
          }
        runMonitor' = TestFailed . CliError . show <$> Runtime.runMonitor runtimeMonitor
      result <- liftIO $ runTest env faucet testCase `altIO` runMonitor'
      case result of
        TestSucceeded -> do
          liftIO $ putStrLn "***** PASSED *****"
        TestFailed err -> do
          liftIO (print err)
          liftIO (putStrLn "***** FAILED *****")

runTest
  :: IsShelleyBasedEra era
  => IsPlutusScriptLanguage lang
  => InterpretEnv lang era
  -> Wallet era
  -> TestCase
  -> IO TestResult
runTest env faucet (TestCase testName testOperations) = do
  let
    interpretState = InterpretState
      {
        _isCLIContracts = CLIContracts mempty
      , _isPublishedScripts = Nothing
      , _isCurrencies = Currencies mempty
      , _isWallets = Wallet.Wallets $ Map.singleton Wallet.faucetNickname faucet
      , _isKnownContracts = mempty
      }
  res <- runExceptT $ flip runReaderT env $ flip execStateT interpretState $ for testOperations \operation -> do
    interpret operation
  case res of
    Left err -> pure $ TestFailed err
    Right _ -> pure TestSucceeded


-- -- | Test a Marlowe contract.
-- scriptTest  :: forall era m
--              . MonadError CliError m
--             => IsShelleyBasedEra era
--             => MonadIO m
--             => ScriptDataSupportedInEra era
--             -> ProtocolVersion
--             -> CostModelParams
--             -> LocalNodeConnectInfo CardanoMode  -- ^ The connection to the local node.
--             -> Wallet era                        -- ^ Wallet which should be used as faucet.
--             -> SlotConfig                        -- ^ The time and slot correspondence.
--             -> ExecutionMode
--             -> ScriptTest                        -- ^ The tests to be run.
--             -> m ()                              -- ^ Action for running the tests.
-- scriptTest era protocolVersion costModel connection faucet slotConfig executionMode ScriptTest{..} =
--   do
--     liftIO $ putStrLn ""
--     liftIO . putStrLn $ "***** Test " <> show stTestName <> " *****"
--     eventBackend <- liftIO $ simpleJsonStderrBackend defaultRenderSelectorJSON
--     let
--       printStats = PrintStats True
--       scriptEnv = ScriptEnv connection costModel era protocolVersion slotConfig executionMode printStats eventBackend
--     runScriptTest scriptEnv (scriptState faucet) stScriptOperations
-- runScriptTest scriptEnv scriptSt stScriptOperations =
--   do
--     let
--       -- We gonna make runtime an optional requirment for the runner
--       -- eventBackend = undefined
--       interpretLoop = for_ stScriptOperations \operation -> do
--         logSoMsg SoName operation ""
--         interpret operation
--
--     void $ catchError
--       (runReaderT (execStateT interpretLoop scriptSt) scriptEnv)
--       $ \e -> do
--         -- TODO: Clean up wallets and instances.
--         liftIO (print e)
--         liftIO (putStrLn "***** FAILED *****")
--         throwError (e :: CliError)
--     liftIO $ putStrLn "***** PASSED *****"

