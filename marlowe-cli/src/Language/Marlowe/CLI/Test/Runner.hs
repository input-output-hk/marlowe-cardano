{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Language.Marlowe.CLI.Test.Runner
  where

import Cardano.Api
  ( AddressInEra
  , ConsensusModeParams(CardanoModeParams)
  , EpochSlots(..)
  , IsShelleyBasedEra
  , Key(getVerificationKey, verificationKeyHash)
  , LocalNodeConnectInfo(..)
  , Lovelace(Lovelace)
  , ScriptDataSupportedInEra
  , lovelaceToValue
  )
import qualified Cardano.Api as C
import Cardano.Api.Shelley (protocolParamProtocolVersion)
import Contrib.Cardano.Api (lovelaceFromInt, lovelaceToInt)
import Contrib.Control.Concurrent.Async (altIO)
import Contrib.Control.Monad.Trans.State.IO (IOStateT, unsafeExecIOStateT)
import Contrib.Data.Foldable (foldMapFlipped, foldMapM, foldMapMFlipped)
import Contrib.Monad.Loops
  (MaxRetries(MaxRetries), PrevResult(PrevResult), RetryCounter(RetryCounter), retryTillJust, retryTillRight)
import Contrib.UnliftIO.Control.Concurrent (threadDelayBy)
-- import Control.Concurrent.Async.Pool (mapTasks, mapTasks_, withTaskGroup)
import qualified Cardano.Api.Shelley as CS
import qualified Cardano.Api.Shelley as S
import Contrib.Control.Exception (liftEitherIO, noteIO)
import qualified Contrib.UnliftIO.Async.Pool as UnlifIO
import Control.Concurrent.STM
  (STM, TVar, modifyTVar, modifyTVar', newTVar, newTVarIO, readTVar, readTVarIO, retry, writeTVar)
import Control.Error (ExceptT, hush, note)
import Control.Exception (Exception, SomeException, mask, onException, throwIO)
import Control.Lens
  (At(at), Bifunctor(bimap), Each(each), _2, _Just, coerced, makeLenses, non, over, traversed, view, views, (^.), (^..))
import qualified Control.Lens as L
import qualified Control.Lens as Lens
import Control.Monad (replicateM, unless, void, when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (MonadError, MonadIO, catchError, liftEither, runExceptT, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader(ask), ReaderT(runReaderT), runReader, withReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State (execStateT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON, ToJSON, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson.OneLine as A
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as B
import Data.Coerce (coerce)
import Data.Default (Default(def))
import Data.Foldable (Foldable(fold), for_, toList)
import qualified Data.Foldable as Foldable
import Data.Functor ((<&>))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as S (singleton)
import qualified Data.Text as Text
import Data.Time.Units (Second, TimeUnit(toMicroseconds))
import Data.Traversable (for)
import GHC.Generics (Generic)
import GHC.IO.Handle.FD (stderr)
import Language.Marlowe.CLI.Cardano.Api (toPlutusProtocolVersion, txOutValueValue)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (IsPlutusScriptLanguage)
import Language.Marlowe.CLI.Cardano.Api.Value (lovelaceToPlutusValue, toPlutusValue)
import qualified Language.Marlowe.CLI.Cardano.Api.Value as CV
import Language.Marlowe.CLI.IO (decodeFileStrict, getDefaultCostModel, queryInEra, readSigningKey)
import Language.Marlowe.CLI.Test.CLI.Types
  (CLIContractInfo(CLIContractInfo), CLIContracts(CLIContracts), CLITxInfo, ciPlan, ciThread, unCLIContracts)
import Language.Marlowe.CLI.Test.ExecutionMode (ExecutionMode(OnChainMode), toSubmitMode)
import Language.Marlowe.CLI.Test.Interpret (interpret)
import Language.Marlowe.CLI.Test.InterpreterError (InterpreterError(CliOperationFailed))
import Language.Marlowe.CLI.Test.Log (logLabeledMsg)
import qualified Language.Marlowe.CLI.Test.Runtime.Monitor as Runtime.Monitor
import Language.Marlowe.CLI.Test.Runtime.Types (RuntimeError(RuntimeRollbackError), RuntimeMonitor)
import qualified Language.Marlowe.CLI.Test.Runtime.Types as R
import qualified Language.Marlowe.CLI.Test.Runtime.Types as Runtime
import qualified Language.Marlowe.CLI.Test.Runtime.Types as Runtime.Monitor
import Language.Marlowe.CLI.Test.TestCase
  (TransactionCostUpperBound(TransactionCostUpperBound), testTxsFeesUpperBound, testsFaucetBudgetUpperBound)
import qualified Language.Marlowe.CLI.Test.TestCase as TestCase
import Language.Marlowe.CLI.Test.Types
  ( ConcurrentRunners(ConcurrentRunners)
  , FailureError(InterpreterError, RuntimeMonitorError)
  , FailureReport(FailureReport)
  , FaucetsNumber(FaucetsNumber)
  , InterpretEnv(..)
  , InterpretState(..)
  , ReportingStrategy(..)
  , RuntimeConfig(..)
  , TestCase(..)
  , TestName(TestName)
  , TestOperation(WalletOperation)
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
  , mkInterpretState
  , plutusValueToJSON
  , someTxBodyToJSON
  , testResultToJSON
  , walletToJSON
  )
import Language.Marlowe.CLI.Test.Wallet.Interpret (createWallet)
import Language.Marlowe.CLI.Test.Wallet.Types
  ( Currencies(Currencies)
  , Currency(Currency)
  , CurrencyNickname(CurrencyNickname)
  , SomeTxBody(SomeTxBody)
  , Wallet(Wallet, _waAddress, _waBalanceCheckBaseline)
  , WalletNickname(WalletNickname)
  , Wallets(Wallets)
  , faucetNickname
  , getFaucet
  , getNonFaucetWallets
  , mkWallets
  , overSomeTxBody
  , overSomeTxBody'
  , txBodyFee
  , waSubmittedTransactions
  , walletTxFees
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
import qualified Language.Marlowe.Runtime.App.Types as Apps
import qualified Network.Protocol.Connection as Network.Protocol
import qualified Network.Protocol.Driver as Network.Protocol
import qualified Network.Protocol.Handshake.Client as Network.Protocol
import Observe.Event.Render.JSON (defaultRenderSelectorJSON)
import Observe.Event.Render.JSON.Handle (simpleJsonStderrBackend)
import qualified Plutus.V1.Ledger.Ada as PV
import Plutus.V1.Ledger.SlotConfig (SlotConfig)
import qualified Plutus.V1.Ledger.Value as PV
import qualified Plutus.V2.Ledger.Api as P
import PlutusCore (defaultCostModelParams)
import qualified PlutusTx.Monoid as P
import System.IO (hPrint, hPutStrLn)
import UnliftIO (IOException, MonadUnliftIO(withRunInIO), atomically, bracket, race, writeIORef)
import qualified UnliftIO as UnlifIO

data TestRunnerError
  = TestRunnerError String
  | TestRunnerCliError String
  | TestRunnerInterpreterError InterpreterError
  -- ^ We run the test interpreter as a part of the runner
  -- to do wallet provisioning and cleanups.
  deriving (Show, Generic, Exception, ToJSON, FromJSON)

fromCLIError :: CliError -> TestRunnerError
fromCLIError (CliError e) = TestRunnerCliError e

-- A helper structure which keeps track of the funded transactions
-- accross test runs.
data TestRunnerFaucet era = TestRunnerFaucet
  { _trAddress :: AddressInEra era
  , _trSigningKey :: SomePaymentSigningKey
  , _trInitialBalance :: P.Value
  , _trSubmittedTransactions :: [SomeTxBody]
  , _trFundedTransactions :: [SomeTxBody]
  }

makeLenses 'TestRunnerFaucet

testRunnerFaucetToJSON :: C.IsCardanoEra era => TestRunnerFaucet era -> A.Value
testRunnerFaucetToJSON TestRunnerFaucet {..} = do
  let
    getTxIds = overSomeTxBody (const C.getTxId)
  A.object
    [ "address" .= _trAddress
    , "initialBalance" .= plutusValueToJSON _trInitialBalance
    , "submittedTransactions" .= (_trSubmittedTransactions <&> someTxBodyToJSON)
    , "fundedTransactions" .= (_trFundedTransactions <&> someTxBodyToJSON)
    ]

newtype MasterFaucet era = MasterFaucet { _mfFaucet :: TestRunnerFaucet era }

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
  }

makeLenses 'Env

setEnvResource :: Env lang era resource -> resource' -> Env lang era resource'
setEnvResource Env{..} resource = Env{_envResource=resource,..}

-- We use `ReaderT` directly because we want to use `withReaderT` to hide or replace the resource:
-- * test suite runner bracket has to access "master faucet"
-- * test runner loop has to access "faucets pool"
-- * test interpreter can only access its interpret state reference.
type EnvReader lang era resource a = ReaderT (Env lang era resource) IO a

withReaderEnvResource resource = withReaderT (`setEnvResource` resource)

data FaucetToWalletConversion = IncludeAllTransactions | ExcludeAllTransactions

-- When we run test we use a faucet from our pool and we turn
-- it into a test faucet. At the end of the test we update
-- the faucet with the transactions it submitted and funded.
-- toTestWallet :: TestRunnerFaucet era -> FaucetToWalletConversion -> Wallet era
toTestWallet
  :: C.IsCardanoEra era
  => MonadIO m
  => MonadReader (Env lang era resource) m
  => TestRunnerFaucet era
  -> FaucetToWalletConversion
  -> m (Wallet era)
toTestWallet faucet@TestRunnerFaucet{..} conversion = do
  let
    expectedBalance =
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
        }
    IncludeAllTransactions -> do
      let
        actualBalance' =
          actualBalance
          <> P.inv (_trSubmittedTransactions `foldMapFlipped` (lovelaceToPlutusValue . overSomeTxBody' txBodyFee))
          <> P.inv (_trFundedTransactions `foldMapFlipped` (lovelaceToPlutusValue . overSomeTxBody' txBodyFee))
      Wallet
        { _waAddress = _trAddress
        , _waBalanceCheckBaseline = actualBalance'
        , _waSigningKey = _trSigningKey
        , _waSubmittedTransactions = _trSubmittedTransactions <> _trFundedTransactions
        }

updateFaucet
  :: TestRunnerFaucet era
  -> InterpretState lang era
  -> TestRunnerFaucet era
updateFaucet TestRunnerFaucet{..} InterpretState { _isWallets=wallets } =
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

type CliAction era a
  = forall m
   . MonadIO m
  => MonadReader (CliEnv era) m
  => MonadError CliError m
  => m a

runCli
  :: MonadIO m
  => MonadReader (Env lang era resource) m
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
   . MonadIO m
  => MonadReader (Env lang era resource) m
  => AddressInEra era
  -> m (C.UTxO era)
fetchAddressUTxOs address = do
  connection <- view envConnection
  runCli $ queryInEra connection
    . C.QueryUTxO
    . C.QueryUTxOByAddress
    . S.singleton
    . T.toAddressAny'
    $ address

fetchAddressTotalValue
  :: MonadIO m
  => MonadReader (Env lang era resource) m
  => AddressInEra era
  -> m CS.Value
fetchAddressTotalValue addr = do
  utxos <- fetchAddressUTxOs addr
  pure $ foldMap txOutValueValue . Map.elems . C.unUTxO $ utxos

fetchAddressTotalLovelace
  :: MonadIO m
  => MonadReader (Env lang era resource) m
  => AddressInEra era
  -> m C.Lovelace
fetchAddressTotalLovelace address = do
  value <- fetchAddressTotalValue address
  pure $ C.selectLovelace value

liftCliEither
  :: MonadIO m
  => Either CliError a
  -> m a
liftCliEither = liftIO . liftEitherIO . first fromCLIError

-- Useful helper to use the interpreter as a part of the test runner.
-- We don't run runtime monitor thread here because we don't need it.
execWalletOperations
  :: forall era lang m resource
   . IsShelleyBasedEra era
  => IsPlutusScriptLanguage lang
  => MonadIO m
  => MonadReader (Env lang era resource) m
  => InterpretState lang era
  -> [Wallet.WalletOperation]
  -> m (Either (TestRunnerError, InterpretState lang era) (InterpretState lang era))
execWalletOperations interpretState operations = runExceptT do
  (testEnv, _) <- lift setupTestInterpretEnv
  let
    operations' = operations <&> WalletOperation
  stateRef <- liftIO $ newIORef interpretState
  let
    loop = unsafeExecIOStateT stateRef $ runExceptT $ flip runReaderT testEnv $ for operations' interpret
  result <- liftIO loop
  interpretState' <- liftIO $ readIORef stateRef
  case result of
    Left err -> throwError (TestRunnerInterpreterError err, interpretState')
    Right _ -> pure interpretState'

setupTestInterpretEnv
  :: MonadReader (Env lang era resource) m
  => MonadIO m
  => m (InterpretEnv lang era, Maybe R.RuntimeMonitor)
setupTestInterpretEnv = do
  connection <- view envConnection
  era <- view envEra
  executionMode <- view envExecutionMode
  printStats <- view envPrintStats

  possibleRuntimeConfig <- view envRuntimeConfig
  runtimeSetup <- for possibleRuntimeConfig \RuntimeConfig {..} -> do
    let
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
    (connector,) <$> liftIO (Runtime.Monitor.mkRuntimeMonitor config)

  protocolParams <- view envProtocolParams
  slotConfig <- view envSlotConfig
  costModelParams <- view envCostModelParams
  let
    protocolVersion = toPlutusProtocolVersion $ protocolParamProtocolVersion protocolParams
    env = InterpretEnv
      { _ieConnection=connection
      , _ieCostModelParams=costModelParams
      , _ieEra=era
      , _ieProtocolVersion=protocolVersion
      , _ieSlotConfig=slotConfig
      , _ieExecutionMode=executionMode
      , _iePrintStats=printStats
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
type TestInterpreterM lang era a = EnvReader lang era (IORef (InterpretState lang era)) a

-- The resulting `Either a a` is the way to express
-- retries with short circuiting (by using `retryTillRight` somewhere below).
-- Result semantics is as follows:
--  * `Right` - close the loop imidiatelly. It is a definite result.
--  * `Left` - possibly try again. the result is not definite and retry could change it.
interpretTest
  :: forall lang era
   . IsShelleyBasedEra era
  => IsPlutusScriptLanguage lang
  => [TestOperation]
  -> PrevResult (TestResult lang era) (TestResult lang era)
  -> TestInterpreterM lang era (Either (TestResult lang era) (TestResult lang era))
interpretTest testOperations (PrevResult possiblePrevResult) = do
  stateRef <- view envResource
  (testEnv, possibleRuntimeMonitor) <- setupTestInterpretEnv
  let
    runInterpretLoop = do
      liftIO $ unsafeExecIOStateT stateRef $ runExceptT $ flip runReaderT testEnv $ for testOperations \operation ->
        interpret operation
  res <- case possibleRuntimeMonitor of
    Just runtimeMonitor -> runInterpretLoop `race` liftIO (Runtime.runMonitor runtimeMonitor)
    Nothing -> Left <$> runInterpretLoop

  -- data TestResult lang era
  --   = TestSucceeded (InterpretState lang era) -- { retries :: RetryCounter, time :: Second }
  --   | TestFailed
  --     { result :: FailureReport lang era
  --     , retries :: [FailureReport lang era]
  --     }
  let
    prevFailures = case either id id <$> possiblePrevResult of
      Just TestSucceeded {} -> []
      Just (TestFailed failure retries) -> failure:retries
      Nothing -> []

    testFailed failure = TestFailed failure prevFailures

  state <- liftIO $ readIORef stateRef
  -- We perform retries only when meaningful rollback occures
  -- throughout the excution (indicated by the runtime monitor) or
  -- ulon `CliOperationFailed`.
  pure $ case res of
    Left testInterpretResult -> case testInterpretResult of
      Left err@CliOperationFailed {} -> Left $ testFailed (FailureReport (InterpreterError err) state)
      Left err -> Right $ testFailed (FailureReport (InterpreterError err) state)
      Right _ -> Right $ TestSucceeded state
    Right runtimeMonitorError -> case runtimeMonitorError of
      runtimeError@(RuntimeRollbackError _) ->
        Left $ testFailed (FailureReport (RuntimeMonitorError runtimeError) state)
      runtimeError ->
        Right $ testFailed (FailureReport (RuntimeMonitorError runtimeError) state)

-- Test runner modifies the results and some faucets (used during testing and retries).
type TestRunnerEnv lang era = Env lang era (TVar [TestRunnerFaucet era], TVar (TestsResults lang era))

acquireFaucet
  :: MonadIO m
  => MonadReader (TestRunnerEnv lang era) m
  => m (TestRunnerFaucet era)
acquireFaucet = do
  faucetsRef <- views envResource fst
  atomically $ do
    faucets <- readTVar faucetsRef
    case faucets of
      [] -> retry
      faucet : faucets' -> do
        writeTVar faucetsRef faucets'
        pure faucet

-- releaseFaucet :: TVar [TestRunnerFaucet era] -> TestRunnerFaucet era -> STM ()
releaseFaucet faucet = do
  faucetsRef <- views envResource fst
  atomically $ modifyTVar faucetsRef (++ [faucet])

-- Resource which we acquire during the test execution is a faucet. We also
-- track all the wallets and transactions which were submitted throught the
-- test execution.
-- The actuall interpreting function has an access to the interpreter state
-- only. The acquired faucet is used later on during the cleanup phase.
type TestInterpretContext lang era = (IORef (InterpretState lang era), TestRunnerFaucet era)

acquireTestInterpretContext
  :: C.IsCardanoEra era
  => MonadIO m
  => MonadReader (TestRunnerEnv lang era) m
  => m (TestInterpretContext lang era)
acquireTestInterpretContext = do
  faucet <- acquireFaucet
  stateRef <- do
    faucetWallet <- toTestWallet faucet ExcludeAllTransactions
    liftIO $ newIORef (mkInterpretState $ mkWallets faucetWallet)
  pure (stateRef, faucet)

releaseTestInterpretContext
  :: MonadIO m
  => IsShelleyBasedEra era
  => IsPlutusScriptLanguage lang
  => MonadReader (TestRunnerEnv lang era) m
  => TestInterpretContext lang era
  -> m ()
releaseTestInterpretContext (stateRef, faucet) = do
  let
    -- In the last phase we execute two cleanup operations
    -- using the test interpreter state so we extend the
    -- tx set with these two.
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
  let
    faucet' = updateFaucet faucet interpretState
  releaseFaucet faucet'

type TestRunnerM lang era a = ReaderT (TestRunnerEnv lang era) IO a

runTest
  :: forall era lang m
   . IsPlutusScriptLanguage lang
  => IsShelleyBasedEra era
  => (FilePath, TestCase)
  -> TestRunnerM lang era ()
runTest (testFile, TestCase { testName, operations=testOperations }) = do
  let
    maxRollbackRetries = MaxRetries 4
  liftIO $ hPutStrLn stderr ""
  liftIO $ hPutStrLn stderr $ "***** Test " <> coerce testName <> " *****"
  result <- either id id <$> retryTillRight maxRollbackRetries \_ prevResult ->
    unmaskedReleaseBracket' acquireTestInterpretContext releaseTestInterpretContext \(stateRef, _) ->
      withReaderEnvResource stateRef (interpretTest testOperations prevResult)

  resultsRef <- views envResource snd
  atomically $
    modifyTVar' resultsRef (Map.insert (testFile, testName) result)

  streamJson <- view envStreamJSON
  when streamJson $ do
    let
      resultJson = testResultToJSON testFile testName result
    liftIO $ putStrLn $ Text.unpack $ A.renderValue resultJson

-- Internally we pass a reference to a master faucet through `bracket`
-- so we can record all the txs.
type TestSuiteRunnerInternalEnv lang era = Env lang era (TVar (MasterFaucet era))

acquireFaucets
  :: forall era lang m
   . IsShelleyBasedEra era
  => MonadIO m
  => IsPlutusScriptLanguage lang
  => MonadReader (TestSuiteRunnerInternalEnv lang era) m
  => TestFaucetBudget
  -> FaucetsNumber
  -> m (TVar [TestRunnerFaucet era])
acquireFaucets (TestFaucetBudget testFaucetBudget) (FaucetsNumber faucetNumber) = do
  masterFaucetRef <- view envResource
  MasterFaucet masterFaucet <- liftIO $ readTVarIO masterFaucetRef

  wallets <- do
    let
      faucetNicknames = [1..faucetNumber] <&> \i -> WalletNickname ("Faucet-" <> show i)
      createWallets = faucetNicknames <&> \walletNickname -> Wallet.CreateWallet walletNickname
      operations =
        createWallets
        <> [Wallet.FundWallets faucetNicknames [testFaucetBudget] (Just False)]
    faucetWallet <- toTestWallet masterFaucet ExcludeAllTransactions
    let
      updateMasterFaucet interpretState' =
        atomically $ modifyTVar' masterFaucetRef $ \(MasterFaucet masterFaucet') ->
          MasterFaucet (updateFaucet masterFaucet' interpretState')

      wallets = mkWallets faucetWallet
      interpretState = mkInterpretState wallets
    execWalletOperations interpretState operations >>= \case
      Left (err, interpretState') -> do
        updateMasterFaucet interpretState'
        liftIO $ throwIO $ TestRunnerError $ "Failed to create faucet wallets: " <> show err
      Right interpretState'@InterpretState { _isWallets=wallets } -> do
        updateMasterFaucet interpretState'
        pure wallets

  let
    subFaucetsWallets = getNonFaucetWallets wallets

    fromFreshWallet wallet@Wallet{..} = do
      unless (null _waSubmittedTransactions) $
        liftIO $ throwIO $ TestRunnerError "TestRunnerFaucet should be initialized from a fresh wallet"
      initialBalance <- toPlutusValue <$> fetchAddressTotalValue _waAddress
      pure $ TestRunnerFaucet
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
   . IsShelleyBasedEra era
  => MonadIO m
  => IsPlutusScriptLanguage lang
  => MonadReader (TestSuiteRunnerInternalEnv lang era) m
  => TVar [TestRunnerFaucet era]
  -> m ()
releaseFaucets subfaucetsRef = do
  masterFaucetRef <- view envResource
  MasterFaucet masterFaucet <- liftIO $ readTVarIO masterFaucetRef

  masterFaucetWallet <- toTestWallet masterFaucet ExcludeAllTransactions
  subfaucets <- liftIO $ readTVarIO subfaucetsRef
  subfaucetsWallets <- fmap Map.fromList $ for (zip [1..] subfaucets) \(i, subfaucet) -> do
    -- We include all the transactions because we want to update the master wallet using the state.
    subfaucetWallet <- toTestWallet subfaucet IncludeAllTransactions
    pure (WalletNickname ("Faucet-" <> show i), subfaucetWallet)

  let
    wallets = Wallets subfaucetsWallets masterFaucetWallet
    interpretState = mkInterpretState wallets
    operations = [ Wallet.ReturnFunds ]

  -- FIXME: This loop should be part of the `RetrunFund` interpreter
  interpretState' <- fmap (either id id) $ retryTillRight (MaxRetries 5) \_ (PrevResult res) -> do
    let
      -- We pass the interpret state from the previous iteration so we keep track of all the txs.
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

fetchBalance address = do
  connection <- view envConnection
  C.UTxO utxo <- runCli $ queryUtxos connection address
  pure $ foldMap (toPlutusValue . txOutValueValue) (Map.elems utxo)

mkMasterFaucet
  :: MonadReader (TestSuiteRunnerEnv lang era) m
  => MonadIO m
  => m (MasterFaucet era)
mkMasterFaucet = do
  (address, signingKey) <- view envResource
  total <- fetchBalance address
  pure $ MasterFaucet $ TestRunnerFaucet
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
masterFauceInfoToJSON MasterFaucetInfo {..} = A.object
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

testSuiteResultToJSON :: C.IsCardanoEra era => TestSuiteResult lang era -> A.Value
testSuiteResultToJSON TestSuiteResult {..} = A.object
  [ "results" .= do
      let
        step ((testFile, testName), result) = testResultToJSON testFile testName result
      map step $ Map.toList _tsResult

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

unmaskedReleaseBracket' before after thing = withRunInIO \run -> do
  unmaskedReleaseBracket (run before) (run . after) (run . thing)

runTests
  :: forall era lang
   . IsShelleyBasedEra era
  => IsPlutusScriptLanguage lang
  => [(FilePath, TestCase)]
  -> ConcurrentRunners
  -> TestSuiteRunnerM lang era (TestSuiteResult lang era)
runTests tests (ConcurrentRunners concurrentRunners) = do
  let
    txCosts = TransactionCostUpperBound (Lovelace 3_000_000) (Lovelace 70_000_000)
    subfaucetBudget = testsFaucetBudgetUpperBound txCosts (map snd tests)
    requiredFunds = lovelaceFromInt (lovelaceToInt subfaucetBudget * concurrentRunners)
    totalTxCost = sum (testTxsFeesUpperBound txCosts <$> map snd tests)
    acquireFaucets' = acquireFaucets (TestFaucetBudget subfaucetBudget) (FaucetsNumber concurrentRunners)

  liftIO do
    hPutStrLn stderr $ "Estimated required funds: " <> show (toInteger requiredFunds `div` 1_000_000) <> " ADA"
    hPutStrLn stderr $ "Estimated subfaucet budget: " <> show (toInteger subfaucetBudget `div` 1_000_000) <> " ADA"
    hPutStrLn stderr $ "Estimated test total tx fees: " <> show (toInteger totalTxCost `div` 1_000_000) <> " ADA"
    hPutStrLn stderr $ "Concurrent runners: " <> show concurrentRunners

  masterFaucet <- mkMasterFaucet
  masterFaucetRef <- liftIO $ newTVarIO masterFaucet
  resultsRef <- liftIO $ newTVarIO mempty

  withReaderEnvResource masterFaucetRef $ unmaskedReleaseBracket' acquireFaucets' releaseFaucets \subfaucetsRef -> do
    withReaderEnvResource (subfaucetsRef, resultsRef) $ UnlifIO.withTaskGroup concurrentRunners \taskGroup -> do
      UnlifIO.mapTasksE taskGroup $ fmap runTest tests

  masterFaucet' <- liftIO $ readTVarIO masterFaucetRef
  currentBalance <- fetchBalance (masterFaucet' ^. (mfFaucet . trAddress))
  let
    masterFaucetInfo = MasterFaucetInfo
      { _mfiInitialBalance = masterFaucet' ^. (mfFaucet . trInitialBalance)
      , _mfiCurrentBalance = currentBalance
      , _mfiSubmittedTransactions = masterFaucet' ^. (mfFaucet . trSubmittedTransactions)
      , _mfiFundedTransactions = masterFaucet' ^. (mfFaucet . trFundedTransactions)
      }
  result <- liftIO $ readTVarIO resultsRef
  pure $ TestSuiteResult result masterFaucetInfo


