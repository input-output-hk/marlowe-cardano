{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Language.Marlowe.CLI.Test.Runner where

import Cardano.Api (
  AddressInEra,
  IsShelleyBasedEra,
  LocalNodeConnectInfo (..),
  Lovelace (Lovelace),
  ScriptDataSupportedInEra,
 )
import Cardano.Api qualified as C
import Cardano.Api.Shelley (protocolParamProtocolVersion)
import Cardano.Api.Shelley qualified as CS
import Contrib.Cardano.Api (lovelaceFromInt, lovelaceToInt)
import Contrib.Control.Exception (liftEitherIO)
import Contrib.Control.Monad.Trans.State.IO (unsafeExecIOStateT)
import Contrib.Data.Foldable (foldMapFlipped)
import Contrib.Monad.Loops (MaxRetries (..), PrevResult (PrevResult), retryTillJust, retryTillRight)
import Contrib.UnliftIO.Async.Pool qualified as UnlifIO
import Contrib.UnliftIO.Control.Concurrent (threadDelayBy)
import Control.Concurrent.STM (TVar, modifyTVar, modifyTVar', newTVarIO, readTVar, readTVarIO, retry, writeTVar)
import Control.Exception (Exception, onException, throwIO)
import Control.Lens (makeLenses, view, views, (^.))
import Control.Monad (unless, void, when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (MonadError, MonadIO, runExceptT)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), withReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON, ToJSON, (.=))
import Data.Aeson qualified as A
import Data.Aeson.OneLine qualified as A
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.IORef (IORef, newIORef, readIORef)
import Data.List.Extra qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as S (singleton)
import Data.Text qualified as Text
import Data.Time.Units (Second)
import Data.Traversable (for)
import GHC.Generics (Generic)
import GHC.IO.Handle.FD (stderr)
import Language.Marlowe.CLI.Cardano.Api (toPlutusProtocolVersion, txOutValueValue)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (IsPlutusScriptLanguage)
import Language.Marlowe.CLI.Cardano.Api.Value (lovelaceToPlutusValue, toPlutusValue)
import Language.Marlowe.CLI.IO (queryInEra)
import Language.Marlowe.CLI.Test.ExecutionMode (ExecutionMode)
import Language.Marlowe.CLI.Test.Interpret (interpret)
import Language.Marlowe.CLI.Test.InterpreterError (InterpreterError (CliOperationFailed, TimeOutReached))
import Language.Marlowe.CLI.Test.Runtime.Monitor qualified as Runtime.Monitor
import Language.Marlowe.CLI.Test.Runtime.Types (RuntimeError (RuntimeRollbackError))
import Language.Marlowe.CLI.Test.Runtime.Types qualified as R
import Language.Marlowe.CLI.Test.Runtime.Types qualified as Runtime
import Language.Marlowe.CLI.Test.TestCase (
  TxCostsUpperBounds (TxCostsUpperBounds),
  testFaucetBudgetUpperBound,
  testTxsFeesUpperBound,
  testsFaucetBudgetUpperBound,
 )
import Language.Marlowe.CLI.Test.Types (
  ConcurrentRunners (ConcurrentRunners),
  FailureError (InterpreterError, RuntimeMonitorError),
  FailureReport (FailureReport),
  FaucetsNumber (FaucetsNumber),
  InterpretEnv (..),
  InterpretState (..),
  RuntimeConfig (..),
  TestCase (..),
  TestName (TestName),
  TestOperation (WalletOperation),
  TestResult (TestFailed, TestSucceeded),
  mkInterpretState,
  plutusValueToJSON,
  someTxBodyToJSON,
  testResultToJSON,
 )
import Language.Marlowe.CLI.Test.Wallet.Types (
  SomeTxBody,
  Wallet (Wallet, _waAddress, _waBalanceCheckBaseline),
  WalletNickname (WalletNickname),
  Wallets (Wallets),
  getFaucet,
  getNonFaucetWallets,
  mkWallets,
  overSomeTxBody',
  txBodyFee,
  waSubmittedTransactions,
 )
import Language.Marlowe.CLI.Test.Wallet.Types qualified as Wallet
import Language.Marlowe.CLI.Transaction (maximumFee, queryUtxos)
import Language.Marlowe.CLI.Types (CliEnv (CliEnv), CliError (..), PrintStats, SomePaymentSigningKey)
import Language.Marlowe.CLI.Types qualified as T
import Language.Marlowe.Protocol.Client qualified as Marlowe.Protocol
import Language.Marlowe.Runtime.App.Types qualified as Apps
import Network.Protocol.Connection qualified as Network.Protocol
import Network.Protocol.Driver qualified as Network.Protocol
import Plutus.V1.Ledger.SlotConfig (SlotConfig)
import Plutus.V2.Ledger.Api qualified as P
import PlutusTx.Monoid qualified as P
import System.IO (hPrint, hPutStrLn)
import UnliftIO (MonadUnliftIO (withRunInIO), atomically, race, writeIORef)

data TestRunnerError
  = TestRunnerError String
  | TestRunnerCliError String
  | -- | We run the test interpreter as a part of the runner
    -- to do wallet provisioning and cleanups.
    TestRunnerInterpreterError InterpreterError
  deriving (Show, Generic, Exception, ToJSON, FromJSON)

fromCLIError :: CliError -> TestRunnerError
fromCLIError (CliError e) = TestRunnerCliError e

-- A helper structure which keeps track of the funded transactions
-- accross test runs.
data TestRunnerFaucet era = TestRunnerFaucet
  { _trAddress :: AddressInEra era
  , _trSigningKey :: SomePaymentSigningKey
  , _trInitialBalance :: P.Value
  , _trSubmittedTransactions :: ![SomeTxBody]
  -- ^ Transactions submitted directly by the test runner (like subfaucets funding).
  , _trFundedTransactions :: ![SomeTxBody]
  -- ^ Transactions submitted non directly (subfaucets and test wallets)
  }

makeLenses 'TestRunnerFaucet

testRunnerFaucetToJSON :: (C.IsCardanoEra era) => TestRunnerFaucet era -> A.Value
testRunnerFaucetToJSON TestRunnerFaucet{..} = do
  A.object
    [ "address" .= _trAddress
    , "initialBalance" .= plutusValueToJSON _trInitialBalance
    , "submittedTransactions" .= (_trSubmittedTransactions <&> someTxBodyToJSON)
    , "fundedTransactions" .= (_trFundedTransactions <&> someTxBodyToJSON)
    ]

newtype MasterFaucet era = MasterFaucet {_mfFaucet :: TestRunnerFaucet era}

makeLenses 'MasterFaucet

type TestsResults lang era = Map.Map (FilePath, TestName) (TestResult lang era)

-- We parametrize over `resource`.
-- Test suite runner allocates faucets funded from master faucet and updates the
-- master faucet at the end.
-- Single test runner allocates a wallet from the pool and updates it at the end.
data Env lang era resource = Env
  { _envEra :: ScriptDataSupportedInEra era
  , _envConnection :: LocalNodeConnectInfo C.CardanoMode
  , _envExecutionMode :: ExecutionMode
  , _envResource :: resource
  , _envRuntimeConfig :: Maybe RuntimeConfig
  , _envPrintStats :: PrintStats
  , _envStreamJSON :: Bool
  , _envSlotConfig :: SlotConfig
  , _envProtocolParams :: CS.ProtocolParameters
  , _envCostModelParams :: P.CostModelParams
  , _envMaxRetries :: Int
  }

makeLenses 'Env

setEnvResource :: Env lang era resource -> resource' -> Env lang era resource'
setEnvResource Env{..} resource = Env{_envResource = resource, ..}

-- We use `ReaderT` directly because we want to use `withReaderT` to hide or replace the resource:

-- * test suite runner bracket has to access "master faucet"

-- * test runner loop has to access "faucets pool"

-- * test interpreter can only access its interpret state reference.
type EnvReader lang era resource a = ReaderT (Env lang era resource) IO a

withReaderEnvResource :: resource' -> EnvReader lang era resource' a -> EnvReader lang era resource a
withReaderEnvResource resource = withReaderT (`setEnvResource` resource)

data FaucetToWalletConversion = IncludeAllTransactions | ExcludeAllTransactions

-- When we run test we use a faucet from our pool and we turn
-- it into a test faucet. At the end of the test we update
-- the faucet with the transactions it submitted and funded.
toTestWallet
  :: (C.IsCardanoEra era)
  => (MonadIO m)
  => (MonadReader (Env lang era resource) m)
  => TestRunnerFaucet era
  -> FaucetToWalletConversion
  -> m (Wallet era)
toTestWallet faucet@TestRunnerFaucet{..} conversion = do
  let expectedBalance =
        _trInitialBalance
          <> P.inv (_trSubmittedTransactions `foldMapFlipped` (lovelaceToPlutusValue . overSomeTxBody' txBodyFee))
          <> P.inv (_trFundedTransactions `foldMapFlipped` (lovelaceToPlutusValue . overSomeTxBody' txBodyFee))
  actualBalance <- toPlutusValue <$> fetchAddressTotalValue _trAddress
  when (actualBalance /= expectedBalance) $ do
    liftIO $ hPutStrLn stderr $ "Faucet: " <> Text.unpack (A.renderValue (testRunnerFaucetToJSON faucet))
    liftIO $ hPutStrLn stderr "has unexpected balance: "
    liftIO $ hPutStrLn stderr $ Text.unpack (A.renderValue (A.toJSON actualBalance))
  pure $ case conversion of
    ExcludeAllTransactions ->
      Wallet
        { _waAddress = _trAddress
        , _waBalanceCheckBaseline = actualBalance
        , _waSigningKey = _trSigningKey
        , _waSubmittedTransactions = mempty
        , _waExternal = False
        }
    IncludeAllTransactions -> do
      let actualBalance' =
            actualBalance
              <> P.inv (_trSubmittedTransactions `foldMapFlipped` (lovelaceToPlutusValue . overSomeTxBody' txBodyFee))
              <> P.inv (_trFundedTransactions `foldMapFlipped` (lovelaceToPlutusValue . overSomeTxBody' txBodyFee))
      Wallet
        { _waAddress = _trAddress
        , _waBalanceCheckBaseline = actualBalance'
        , _waSigningKey = _trSigningKey
        , _waSubmittedTransactions = _trSubmittedTransactions <> _trFundedTransactions
        , _waExternal = False
        }

updateFaucet
  :: TestRunnerFaucet era
  -> InterpretState lang era
  -> TestRunnerFaucet era
updateFaucet TestRunnerFaucet{..} InterpretState{_isWallets = wallets} =
  TestRunnerFaucet
    { _trAddress = _trAddress
    , _trSigningKey = _trSigningKey
    , _trInitialBalance = _trInitialBalance
    , _trSubmittedTransactions =
        _trSubmittedTransactions <> view waSubmittedTransactions (getFaucet wallets)
    , _trFundedTransactions =
        _trFundedTransactions <> concatMap (view waSubmittedTransactions) (getNonFaucetWallets wallets)
    }

newtype TestFaucetBudget = TestFaucetBudget C.Lovelace

type CliAction era a =
  forall m
   . (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => (MonadError CliError m)
  => m a

runCli
  :: (MonadIO m)
  => (MonadReader (Env lang era resource) m)
  => CliAction era a
  -> m a
runCli action = do
  era <- view envEra
  res <- liftIO $ runExceptT $ runReaderT action (CliEnv era)
  case res of
    Left err -> liftIO $ throwIO $ TestRunnerCliError $ show err
    Right a -> pure a

fetchAddressUTxOs
  :: forall era lang m resource
   . (MonadIO m)
  => (MonadReader (Env lang era resource) m)
  => AddressInEra era
  -> m (C.UTxO era)
fetchAddressUTxOs address = do
  connection <- view envConnection
  runCli
    $ queryInEra connection
      . C.QueryUTxO
      . C.QueryUTxOByAddress
      . S.singleton
      . T.toAddressAny'
    $ address

fetchAddressTotalValue
  :: (MonadIO m)
  => (MonadReader (Env lang era resource) m)
  => AddressInEra era
  -> m CS.Value
fetchAddressTotalValue addr = do
  utxos <- fetchAddressUTxOs addr
  pure $ foldMap txOutValueValue . Map.elems . C.unUTxO $ utxos

fetchAddressTotalLovelace
  :: (MonadIO m)
  => (MonadReader (Env lang era resource) m)
  => AddressInEra era
  -> m C.Lovelace
fetchAddressTotalLovelace address = do
  value <- fetchAddressTotalValue address
  pure $ C.selectLovelace value

liftCliEither
  :: (MonadIO m)
  => Either CliError a
  -> m a
liftCliEither = liftIO . liftEitherIO . first fromCLIError

-- Useful helper to use the interpreter as a part of the test runner.
-- We don't run runtime monitor thread here because we don't need it.
execWalletOperations
  :: forall era lang m resource
   . (IsShelleyBasedEra era)
  => (IsPlutusScriptLanguage lang)
  => (MonadIO m)
  => (MonadReader (Env lang era resource) m)
  => InterpretState lang era
  -> [Wallet.WalletOperation]
  -> m (Either (TestRunnerError, InterpretState lang era) (InterpretState lang era))
execWalletOperations interpretState operations = runExceptT do
  (testEnv, _) <- lift setupTestInterpretEnv
  let operations' = operations <&> WalletOperation
  stateRef <- liftIO $ newIORef interpretState
  let loop = unsafeExecIOStateT stateRef $ runExceptT $ flip runReaderT testEnv $ for operations' interpret
  result <- liftIO loop
  interpretState' <- liftIO $ readIORef stateRef
  case result of
    Left err -> throwError (TestRunnerInterpreterError err, interpretState')
    Right _ -> pure interpretState'

setupTestInterpretEnv
  :: (MonadReader (Env lang era resource) m)
  => (MonadIO m)
  => m (InterpretEnv lang era, Maybe R.RuntimeMonitor)
setupTestInterpretEnv = do
  connection <- view envConnection
  era <- view envEra
  executionMode <- view envExecutionMode
  printStats <- view envPrintStats

  possibleRuntimeConfig <- view envRuntimeConfig
  runtimeSetup <- for possibleRuntimeConfig \RuntimeConfig{..} -> do
    let connector :: (Network.Protocol.Connector Marlowe.Protocol.MarloweRuntimeClient IO)
        connector = Network.Protocol.tcpClient rcRuntimeHost rcRuntimePort Marlowe.Protocol.marloweRuntimeClientPeer
        config =
          Apps.Config
            { Apps.chainSeekHost = rcRuntimeHost
            , Apps.runtimeHost = rcRuntimeHost
            , Apps.runtimePort = rcRuntimePort
            , Apps.chainSeekSyncPort = rcChainSeekSyncPort
            , Apps.chainSeekCommandPort = rcChainSeekCommandPort
            , Apps.timeoutSeconds = 900
            , Apps.buildSeconds = 3
            , Apps.confirmSeconds = 3
            , Apps.retrySeconds = 10
            , Apps.retryLimit = 5
            }

    (connector,) <$> liftIO (Runtime.Monitor.mkRuntimeMonitor config)

  protocolParams <- view envProtocolParams
  slotConfig <- view envSlotConfig
  costModelParams <- view envCostModelParams
  let protocolVersion = toPlutusProtocolVersion $ protocolParamProtocolVersion protocolParams
      env =
        InterpretEnv
          { _ieConnection = connection
          , _ieCostModelParams = costModelParams
          , _ieEra = era
          , _ieProtocolVersion = protocolVersion
          , _ieSlotConfig = slotConfig
          , _ieExecutionMode = executionMode
          , _iePrintStats = printStats
          , _ieRuntimeMonitor = do
              (_, (runtimeMonitorContracts, runtimeMonitorInput, _)) <- runtimeSetup
              pure (runtimeMonitorContracts, runtimeMonitorInput)
          , _ieRuntimeClientConnector = fst <$> runtimeSetup
          }
  pure $ (env,) do
    (_, (_, _, runtimeMonitor)) <- runtimeSetup
    pure runtimeMonitor

-- This `IORef` implies that the test operations are executed sequentially and not in parallel.
-- Internally we call `unsafeExecIOStateT` but we don't leak this reference anywhere outside this context.
-- We use it to reference the interpreter state even in a case of a exception or a rollback.
type TestInterpreterM lang era a = EnvReader lang era (TestRunId, IORef (InterpretState lang era)) a

-- The resulting `Either a a` is the way to express
-- retries with short circuiting (by using `retryTillRight` somewhere below).
-- Result semantics is as follows:
--  * `Right` - close the loop imidiatelly. It is a definite result.
--  * `Left` - possibly try again. the result is not definite and retry could change it.
interpretTest
  :: forall lang era
   . (IsShelleyBasedEra era)
  => (IsPlutusScriptLanguage lang)
  => [TestOperation]
  -> PrevResult (TestResult lang era) (TestResult lang era)
  -> TestInterpreterM lang era (Either (TestResult lang era) (TestResult lang era))
interpretTest testOperations (PrevResult possiblePrevResult) = do
  stateRef <- views envResource snd
  (testEnv, possibleRuntimeMonitor) <- setupTestInterpretEnv
  let runInterpretLoop = do
        liftIO $ unsafeExecIOStateT stateRef $ runExceptT $ flip runReaderT testEnv $ for testOperations \operation ->
          interpret operation
  res <- case possibleRuntimeMonitor of
    Just runtimeMonitor -> runInterpretLoop `race` liftIO (Runtime.runMonitor runtimeMonitor)
    Nothing -> Left <$> runInterpretLoop

  let prevFailures = case either id id <$> possiblePrevResult of
        Just TestSucceeded{} -> []
        Just (TestFailed failure retries) -> failure : retries
        Nothing -> []

      testFailed failure = TestFailed failure prevFailures

  state <- liftIO $ readIORef stateRef
  -- We perform retries only when meaningful rollback occures
  -- throughout the excution (indicated by the runtime monitor) or
  -- upon `CliOperationFailed` or operation timeout.
  pure $ case res of
    Left testInterpretResult -> case testInterpretResult of
      Left err@CliOperationFailed{} -> Left $ testFailed (FailureReport (InterpreterError err) state)
      Left err@TimeOutReached{} -> Left $ testFailed (FailureReport (InterpreterError err) state)
      Left err -> Right $ testFailed (FailureReport (InterpreterError err) state)
      Right _ -> Right $ TestSucceeded state
    Right runtimeMonitorError -> case runtimeMonitorError of
      runtimeError@(RuntimeRollbackError _) ->
        Left $ testFailed (FailureReport (RuntimeMonitorError runtimeError) state)
      runtimeError ->
        Right $ testFailed (FailureReport (RuntimeMonitorError runtimeError) state)

-- Shared counter which we can use to possibly mark stderr debug logs etc.
newtype TestRunId = TestRunId Int
  deriving (Eq, Ord, Show)
  deriving newtype (Enum)

-- Test runner modifies the results and some faucets (used during testing and retries).
data TestRunnerEnvResource lang era = TestRunnerEnvResource
  { _trcFaucets :: TVar [TestRunnerFaucet era]
  , _trcRunId :: TVar TestRunId
  , _trcResults :: TVar (TestsResults lang era)
  }

makeLenses ''TestRunnerEnvResource

type TestRunnerEnv lang era = Env lang era (TestRunnerEnvResource lang era)

acquireFaucet
  :: (MonadIO m)
  => (MonadReader (TestRunnerEnv lang era) m)
  => m (TestRunnerFaucet era)
acquireFaucet = do
  faucetsRef <- view (envResource . trcFaucets)
  atomically $ do
    faucets <- readTVar faucetsRef
    case faucets of
      [] -> retry
      faucet : faucets' -> do
        writeTVar faucetsRef faucets'
        pure faucet

releaseFaucet
  :: (MonadIO m)
  => (MonadReader (TestRunnerEnv lang era) m)
  => TestRunnerFaucet era
  -> m ()
releaseFaucet faucet = do
  faucetsRef <- view (envResource . trcFaucets)
  atomically $ modifyTVar faucetsRef (++ [faucet])

-- Resource which we acquire during the test execution is a faucet. We also
-- track all the wallets and transactions which were submitted throught the
-- test execution.
-- The actuall interpreting function has an access to the interpreter state
-- only. The acquired faucet is used later on during the cleanup phase.
type TestInterpretContext lang era = (IORef (InterpretState lang era), TestRunnerFaucet era)

acquireTestInterpretContext
  :: (C.IsCardanoEra era)
  => (MonadIO m)
  => (MonadReader (TestRunnerEnv lang era) m)
  => m (TestInterpretContext lang era)
acquireTestInterpretContext = do
  faucet <- acquireFaucet
  stateRef <- do
    faucetWallet <- toTestWallet faucet ExcludeAllTransactions
    liftIO $ newIORef (mkInterpretState $ mkWallets faucetWallet)
  pure (stateRef, faucet)

releaseTestInterpretContext
  :: (MonadIO m)
  => (IsShelleyBasedEra era)
  => (IsPlutusScriptLanguage lang)
  => (MonadReader (TestRunnerEnv lang era) m)
  => TestInterpretContext lang era
  -> m ()
releaseTestInterpretContext (stateRef, faucet) = do
  let -- In the last phase we execute two cleanup operations
      -- using existing test interpreter state so we extend the
      -- tx set with these two and perform a full faucet update
      -- at the end.
      operations =
        [ Wallet.BurnAll mempty
        , Wallet.ReturnFunds
        ]

  void $ retryTillJust (MaxRetries 5) \_ -> do
    interpretState <- liftIO $ readIORef stateRef
    execWalletOperations interpretState operations >>= \case
      Left (err, interpretState') -> do
        liftIO $ hPutStrLn stderr $ "Faucet cleanup failure: " <> show err
        writeIORef stateRef interpretState'
        threadDelayBy (10 :: Second)
        pure Nothing
      Right interpretState' -> do
        writeIORef stateRef interpretState'
        pure $ Just ()
  interpretState <- liftIO $ readIORef stateRef
  let faucet' = updateFaucet faucet interpretState
  releaseFaucet faucet'

type TestRunnerM lang era a = ReaderT (TestRunnerEnv lang era) IO a

runTest
  :: forall era lang
   . (IsPlutusScriptLanguage lang)
  => (IsShelleyBasedEra era)
  => (FilePath, TestCase)
  -> TestRunnerM lang era ()
runTest (testFile, TestCase{testName, operations = testOperations}) = do
  liftIO $ hPutStrLn stderr ""
  liftIO $ hPutStrLn stderr $ "***** Test " <> coerce testName <> " *****"
  maxRetries <- view envMaxRetries
  result <-
    either id id <$> retryTillRight (MaxRetries maxRetries) \_ prevResult ->
      unmaskedReleaseBracket' acquireTestInterpretContext releaseTestInterpretContext \(stateRef, _) -> do
        testRunIdRef <- view (envResource . trcRunId)
        testRunId <- atomically $ do
          testRunId <- readTVar testRunIdRef
          writeTVar testRunIdRef (succ testRunId)
          pure testRunId
        withReaderEnvResource (testRunId, stateRef) (interpretTest testOperations prevResult)

  resultsRef <- view (envResource . trcResults)
  atomically $
    modifyTVar' resultsRef (Map.insert (testFile, testName) result)

  whenM (view envStreamJSON) do
    resultJson <- testResultToJSON testFile testName result
    liftIO $ putStrLn $ Text.unpack $ A.renderValue resultJson

  let printResultMsg msg = liftIO $ hPutStrLn stderr $ "***** " <> coerce testName <> ": " <> msg <> " *****"
  case result of
    TestSucceeded{} -> printResultMsg "SUCCEEDED"
    TestFailed{} -> printResultMsg "FAILED"

-- Internally we pass a reference to a master faucet through `bracket`
-- so we can record all the txs.
type TestSuiteRunnerInternalEnv lang era = Env lang era (TVar (MasterFaucet era))

acquireFaucets
  :: forall era lang m
   . (IsShelleyBasedEra era)
  => (MonadIO m)
  => (IsPlutusScriptLanguage lang)
  => (MonadReader (TestSuiteRunnerInternalEnv lang era) m)
  => TestFaucetBudget
  -> FaucetsNumber
  -> m (TVar [TestRunnerFaucet era])
acquireFaucets (TestFaucetBudget testFaucetBudget) (FaucetsNumber faucetNumber) = do
  masterFaucetRef <- view envResource
  MasterFaucet masterFaucet <- liftIO $ readTVarIO masterFaucetRef

  wallets <- do
    let faucetNicknames = [1 .. faucetNumber] <&> \i -> WalletNickname ("Faucet-" <> show i)
        operations = do
          let createWallets =
                faucetNicknames <&> \walletNickname -> Wallet.CreateWallet walletNickname Nothing
          List.snoc createWallets (Wallet.Fund faucetNicknames [testFaucetBudget])
    faucetWallet <- toTestWallet masterFaucet ExcludeAllTransactions
    let updateMasterFaucet interpretState' =
          atomically $ modifyTVar' masterFaucetRef $ \(MasterFaucet masterFaucet') ->
            MasterFaucet (updateFaucet masterFaucet' interpretState')

        wallets = mkWallets faucetWallet
        interpretState = mkInterpretState wallets
    execWalletOperations interpretState operations >>= \case
      Left (err, interpretState') -> do
        updateMasterFaucet interpretState'
        liftIO $ throwIO $ TestRunnerError $ "Failed to create faucet wallets: " <> show err
      Right interpretState'@InterpretState{_isWallets = ws} -> do
        updateMasterFaucet interpretState'
        pure ws

  let subFaucetsWallets = getNonFaucetWallets wallets

      fromFreshWallet Wallet{..} = do
        unless (null _waSubmittedTransactions) $
          liftIO $
            throwIO $
              TestRunnerError "TestRunnerFaucet should be initialized from a fresh wallet"
        initialBalance <- toPlutusValue <$> fetchAddressTotalValue _waAddress
        pure $
          TestRunnerFaucet
            { _trAddress = _waAddress
            , _trSigningKey = _waSigningKey
            , _trInitialBalance = initialBalance
            , _trSubmittedTransactions = mempty
            , _trFundedTransactions = mempty
            }
  subfaucets <- Foldable.toList <$> for subFaucetsWallets fromFreshWallet
  liftIO $ newTVarIO subfaucets

releaseFaucets
  :: forall era lang m
   . (IsShelleyBasedEra era)
  => (MonadIO m)
  => (IsPlutusScriptLanguage lang)
  => (MonadReader (TestSuiteRunnerInternalEnv lang era) m)
  => TVar [TestRunnerFaucet era]
  -> m ()
releaseFaucets subfaucetsRef = do
  masterFaucetRef <- view envResource
  MasterFaucet masterFaucet <- liftIO $ readTVarIO masterFaucetRef

  masterFaucetWallet <- toTestWallet masterFaucet ExcludeAllTransactions
  subfaucets <- liftIO $ readTVarIO subfaucetsRef
  subfaucetsWallets <- fmap Map.fromList $ for (zip [(1 :: Integer) ..] subfaucets) \(i, subfaucet) -> do
    -- We include all the transactions because we want to update the master wallet using the state.
    subfaucetWallet <- toTestWallet subfaucet IncludeAllTransactions
    pure (WalletNickname ("Faucet-" <> show i), subfaucetWallet)

  let wallets = Wallets subfaucetsWallets masterFaucetWallet
      interpretState = mkInterpretState wallets
      operations = [Wallet.ReturnFunds]

  -- FIXME: This loop should be part of the `RetrunFund` interpreter
  interpretState' <- fmap (either id id) $ retryTillRight (MaxRetries 5) \_ (PrevResult res) -> do
    let -- We pass the interpret state from the previous iteration so we keep track of all the txs.
        interpretState' = maybe interpretState (either id id) res
    execWalletOperations interpretState' operations >>= \case
      Left (err, interpretState'') -> do
        liftIO $ hPutStrLn stderr "Failed to return funds to the master wallet"
        liftIO $ hPrint stderr err
        -- Sould we throw here or only add to the test report this failure?
        -- liftIO $ throwIO $ TestRunnerError $ "Failed to return funds to the master wallet: " <> show err
        threadDelayBy (10 :: Second)
        pure $ Left interpretState''
      Right interpretState'' -> do
        -- Let's update the master faucet transactions set with the funding one.
        pure $ Right interpretState''

  atomically $ modifyTVar' masterFaucetRef $ \(MasterFaucet masterFaucet') ->
    MasterFaucet (updateFaucet masterFaucet' interpretState')

-- | Types which are part of public facing API
type TestSuiteRunnerEnv lang era = Env lang era (AddressInEra era, SomePaymentSigningKey)

type TestSuiteRunnerM lang era a = ReaderT (TestSuiteRunnerEnv lang era) IO a

fetchBalance :: (MonadReader (Env lang era resource) m) => (MonadIO m) => AddressInEra era -> m P.Value
fetchBalance address = do
  connection <- view envConnection
  C.UTxO utxo <- runCli $ queryUtxos connection address
  pure $ foldMap (toPlutusValue . txOutValueValue) (Map.elems utxo)

mkMasterFaucet
  :: (MonadReader (TestSuiteRunnerEnv lang era) m)
  => (MonadIO m)
  => m (MasterFaucet era)
mkMasterFaucet = do
  (address, signingKey) <- view envResource
  total <- fetchBalance address
  pure $
    MasterFaucet $
      TestRunnerFaucet
        { _trAddress = address
        , _trSigningKey = signingKey
        , _trInitialBalance = total
        , _trSubmittedTransactions = []
        , _trFundedTransactions = []
        }

data MasterFaucetInfo = MasterFaucetInfo
  { _mfiInitialBalance :: P.Value
  , _mfiCurrentBalance :: P.Value
  , _mfiSubmittedTransactions :: [SomeTxBody]
  , _mfiFundedTransactions :: [SomeTxBody]
  }

masterFauceInfoToJSON :: MasterFaucetInfo -> A.Value
masterFauceInfoToJSON MasterFaucetInfo{..} =
  A.object
    [ "initialBalance" .= plutusValueToJSON _mfiInitialBalance
    , "currentBalance" .= plutusValueToJSON _mfiCurrentBalance
    , "totalKnownFees" .= do
        foldMap (overSomeTxBody' txBodyFee) _mfiSubmittedTransactions
          <> foldMap (overSomeTxBody' txBodyFee) _mfiFundedTransactions
    , "submittedTxs" .= (_mfiSubmittedTransactions <&> someTxBodyToJSON)
    , "fundedTxs" .= (_mfiFundedTransactions <&> someTxBodyToJSON)
    ]

data TestSuiteResult lang era = TestSuiteResult
  { _tsResult :: TestsResults lang era
  , _tsMasterFaucetInfo :: MasterFaucetInfo
  }

testSuiteResultToJSON :: (C.IsCardanoEra era) => (MonadIO m) => TestSuiteResult lang era -> m A.Value
testSuiteResultToJSON TestSuiteResult{..} = do
  resultsJSON <- for (Map.toList _tsResult) \((testFile, testName), result) -> testResultToJSON testFile testName result
  pure $
    A.object
      [ "results" .= resultsJSON
      , "masterFaucetInfo" A..= masterFauceInfoToJSON _tsMasterFaucetInfo
      ]

-- It is a bit surprising but the standard bracketing failes (hangs)
-- when we have to perform node queries during the release action.
unmaskedReleaseBracket
  :: IO a
  -> (a -> IO b)
  -> (a -> IO c)
  -> IO c
unmaskedReleaseBracket before after thing = do
  a <- before
  r <- thing a `onException` after a
  _ <- after a
  return r

unmaskedReleaseBracket'
  :: (MonadUnliftIO m)
  => m a
  -> (a -> m b)
  -> (a -> m c)
  -> m c
unmaskedReleaseBracket' before after thing = withRunInIO \run -> do
  unmaskedReleaseBracket (run before) (run . after) (run . thing)

makeLenses ''TestSuiteResult

runTests
  :: forall era lang
   . (IsShelleyBasedEra era)
  => (IsPlutusScriptLanguage lang)
  => [(FilePath, TestCase)]
  -> ConcurrentRunners
  -> TestSuiteRunnerM lang era (TestSuiteResult lang era)
runTests tests (ConcurrentRunners maxConcurrentRunners) = do
  protocolParams <- view envProtocolParams
  let concurrentRunners = min maxConcurrentRunners (length tests)
      -- Our coin selection algorithm bounds the tx fees using this 2x factor.
      txCosts = TxCostsUpperBounds (2 * maximumFee protocolParams) (Lovelace 75_000_000)
      subfaucetBudget = testsFaucetBudgetUpperBound txCosts (map snd tests)
      requiredFunds = lovelaceFromInt (lovelaceToInt subfaucetBudget * concurrentRunners)
      totalTxCost = sum (testTxsFeesUpperBound txCosts <$> map snd tests)
      acquireFaucets' = acquireFaucets (TestFaucetBudget subfaucetBudget) (FaucetsNumber concurrentRunners)

  liftIO do
    hPutStrLn stderr $ "Estimated required funds: " <> show (toInteger requiredFunds `div` 1_000_000) <> " ADA"
    hPutStrLn stderr $ "Estimated subfaucet budget: " <> show (toInteger subfaucetBudget `div` 1_000_000) <> " ADA"
    hPutStrLn stderr $ "Estimated test total tx fees: " <> show (toInteger totalTxCost `div` 1_000_000) <> " ADA"
    hPutStrLn stderr $ "Concurrent runners: " <> show concurrentRunners

    for_ tests $ \(testFile, operations) -> do
      let budgetUpperBound = testFaucetBudgetUpperBound txCosts operations
          txsFeesUpperBound = testTxsFeesUpperBound txCosts operations

      hPutStrLn stderr $ testFile <> ":"
      hPutStrLn stderr $ "Estimated test budget: " <> show (toInteger budgetUpperBound `div` 1_000_000) <> " ADA"
      hPutStrLn stderr $ "Estimated test tx fees: " <> show (toInteger txsFeesUpperBound `div` 1_000_000) <> " ADA"

  masterFaucet <- mkMasterFaucet
  masterFaucetRef <- liftIO $ newTVarIO masterFaucet
  resultsRef <- liftIO $ newTVarIO mempty

  void $ withReaderEnvResource masterFaucetRef $ unmaskedReleaseBracket' acquireFaucets' releaseFaucets \subfaucetsRef -> do
    runCounterRef <- liftIO $ newTVarIO (TestRunId 1)
    let testRunnerEnvResource =
          TestRunnerEnvResource
            { _trcFaucets = subfaucetsRef
            , _trcRunId = runCounterRef
            , _trcResults = resultsRef
            }
    withReaderEnvResource testRunnerEnvResource $ UnlifIO.withTaskGroup concurrentRunners \taskGroup -> do
      UnlifIO.mapTasksE taskGroup $ fmap runTest tests

  masterFaucet' <- liftIO $ readTVarIO masterFaucetRef
  currentBalance <- fetchBalance (masterFaucet' ^. (mfFaucet . trAddress))
  let masterFaucetInfo =
        MasterFaucetInfo
          { _mfiInitialBalance = masterFaucet' ^. (mfFaucet . trInitialBalance)
          , _mfiCurrentBalance = currentBalance
          , _mfiSubmittedTransactions = masterFaucet' ^. (mfFaucet . trSubmittedTransactions)
          , _mfiFundedTransactions = masterFaucet' ^. (mfFaucet . trFundedTransactions)
          }
  result <- liftIO $ readTVarIO resultsRef
  pure $ TestSuiteResult result masterFaucetInfo
