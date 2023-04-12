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
import Contrib.Monad.Loops (MaxRetries(MaxRetries), RetryCounter(RetryCounter), retryTillJust)
import Control.Concurrent.Async (race)
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
import Language.Marlowe.CLI.Test.Runtime.Types (RuntimeError(RuntimeRollbackError))
import qualified Language.Marlowe.CLI.Test.Runtime.Types as Runtime
import qualified Language.Marlowe.CLI.Test.Runtime.Types as Runtime.Monitor
import Language.Marlowe.CLI.Test.Types
  ( InterpretEnv(..)
  , InterpretState(..)
  , RuntimeConfig(..)
  , TestCase(..)
  , TestResult(TestFailed, TestSucceeded)
  , TestSuite(..)
  )
import Language.Marlowe.CLI.Test.Wallet.Types (Currencies(Currencies), Wallet)
import qualified Language.Marlowe.CLI.Test.Wallet.Types as Wallet
import Language.Marlowe.CLI.Transaction (querySlotConfig, queryUtxos)
import Language.Marlowe.CLI.Types
  (CliEnv(CliEnv), CliError(..), MarlowePlutusVersion, PrintStats(PrintStats), SigningKeyFile(SigningKeyFile))
import qualified Language.Marlowe.CLI.Types as T
import qualified Language.Marlowe.Protocol.Client as Marlowe.Protocol
import qualified Language.Marlowe.Runtime.App.Types as Apps
import qualified Network.Protocol.Connection as Network.Protocol
import qualified Network.Protocol.Driver as Network.Protocol
import qualified Network.Protocol.Handshake.Client as Network.Protocol
import Observe.Event.Render.JSON (defaultRenderSelectorJSON)
import Observe.Event.Render.JSON.Handle (simpleJsonStderrBackend)
import PlutusCore (defaultCostModelParams)


mkWallet
  :: (MonadError CliError m, MonadIO m, MonadReader (CliEnv era) m)
  => LocalNodeConnectInfo C.CardanoMode
  -> C.AddressInEra era
  -> T.SomePaymentSigningKey
  -> m (Wallet era)
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
        RuntimeConfig { .. } = stRuntime
        connector :: Network.Protocol.SomeClientConnector Marlowe.Protocol.MarloweRuntimeClient IO
        connector = Network.Protocol.SomeConnector
          $ Network.Protocol.handshakeClientConnector
          $ Network.Protocol.tcpClient rcRuntimeHost rcRuntimePort Marlowe.Protocol.marloweRuntimeClientPeer
        config = def
          { Apps.chainSeekHost = rcRuntimeHost
          , Apps.runtimePort = rcRuntimePort
          , Apps.chainSeekSyncPort = rcChainSeekSyncPort
          , Apps.chainSeekCommandPort = rcChainSeekCommandPort
          }
      (runtimeMonitorInput, runtimeMonitorContracts, runtimeMonitor) <- liftIO $ Runtime.Monitor.mkRuntimeMonitor config

      let
        state = InterpretState
          {
            _isCLIContracts = CLIContracts mempty
          , _isPublishedScripts = Nothing
          , _isCurrencies = Currencies mempty
          , _isWallets = Wallet.Wallets $ Map.singleton Wallet.faucetNickname faucet
          , _isKnownContracts = mempty
          }

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
          , _ieRuntimeClientConnector = Just connector
          }
        maxRetries = 4

      result <- retryTillJust (MaxRetries maxRetries) \(RetryCounter retry) -> do
        result <- liftIO $ runTest env state testCase `race` Runtime.runMonitor runtimeMonitor
        case result of
          Right (RuntimeRollbackError _) | retry + 1 <= maxRetries -> do
            liftIO $ putStrLn "Re running the test case because the rollback occured."
            pure Nothing
          Right runtimeError -> do
            liftIO $ putStrLn "Runtime monitor execution failed."
            pure $ Just $ TestFailed . CliError . show $ runtimeError
          Left testResult ->
            pure $ Just testResult

      for_ result \case
        TestSucceeded -> do
          liftIO $ putStrLn "***** PASSED *****"
        TestFailed err -> do
          liftIO (print err)
          liftIO (putStrLn "***** FAILED *****")

runTest
  :: IsShelleyBasedEra era
  => IsPlutusScriptLanguage lang
  => InterpretEnv lang era
  -> InterpretState lang era
  -> TestCase
  -> IO TestResult
runTest env state (TestCase _ testOperations) = do
  res <- runExceptT $ flip runReaderT env $ flip execStateT state $ for testOperations \operation -> do
    interpret operation
  case res of
    Left err -> pure $ TestFailed err
    Right _ -> pure TestSucceeded


