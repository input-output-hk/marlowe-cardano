{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}


module Language.Marlowe.CLI.Test.Types
  where

import Cardano.Api (AddressInEra, CardanoMode, IsCardanoEra, LocalNodeConnectInfo, NetworkId, ScriptDataSupportedInEra)
import Control.Lens (_1, _2, _Just, makeLenses, (^.))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import Data.Map (Map)
import GHC.Generics (Generic)
import Language.Marlowe.CLI.Types (MarlowePlutusVersion, MarloweScriptsRefs, PrintStats)
import Ledger.Orphans ()
import Plutus.ApiCommon (ProtocolVersion)
import Plutus.V1.Ledger.Api (CostModelParams)
import Plutus.V1.Ledger.SlotConfig (SlotConfig)

import qualified Cardano.Api as C
import Control.Category ((<<<))
import Control.Monad.State.Class (MonadState)
import qualified Data.Aeson.Key as A.Key
import qualified Data.Aeson.KeyMap as A.KeyMap
import Data.Bifunctor (Bifunctor(bimap))
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Units (Second, TimeUnit(fromMicroseconds, toMicroseconds))
import Language.Marlowe.CLI.Test.CLI.Types (CLIContracts(CLIContracts), CLIOperation)
import qualified Language.Marlowe.CLI.Test.CLI.Types as CLI
import Language.Marlowe.CLI.Test.Contract (ContractNickname)
import Language.Marlowe.CLI.Test.ExecutionMode (ExecutionMode)
import Language.Marlowe.CLI.Test.InterpreterError (InterpreterError, ieMessage)
import Language.Marlowe.CLI.Test.Log (Logs)
import qualified Language.Marlowe.CLI.Test.Log as Log
import qualified Language.Marlowe.CLI.Test.Operation.Aeson as Operation
import Language.Marlowe.CLI.Test.Runtime.Types (RuntimeMonitorInput, RuntimeMonitorState, RuntimeOperation)
import qualified Language.Marlowe.CLI.Test.Runtime.Types as Runtime
import Language.Marlowe.CLI.Test.Wallet.Types as Wallet
import qualified Language.Marlowe.Protocol.Client as Marlowe.Protocol
import qualified Language.Marlowe.Protocol.Types as Marlowe.Protocol
import qualified Network.Protocol.Connection as Network.Protocol
import Network.Protocol.Handshake.Types (Handshake)
import Network.Socket (PortNumber)
import qualified Plutus.V1.Ledger.Value as P.Value
import qualified Plutus.V2.Ledger.Api as P
import qualified PlutusTx.AssocMap as P.AssocMap

data RuntimeConfig = RuntimeConfig
  { rcRuntimeHost :: String
  , rcRuntimePort :: PortNumber
  , rcChainSeekSyncPort :: PortNumber
  , rcChainSeekCommandPort :: PortNumber
  }
  deriving stock (Eq, Generic, Show)

newtype ConcurrentRunners = ConcurrentRunners Int
  deriving stock (Eq, Generic, Show)

data ReportingStrategy
  = StreamJSON
  | WriteJSONFile FilePath
  | StreamAndWriteJSONFile FilePath
  deriving stock (Eq, Generic, Show)

data TestSuite era a =
    TestSuite
    {
      tsNetwork :: NetworkId
    -- ^ The network ID, if any.
    , tsSocketPath :: FilePath
    -- ^ The path to the node socket.
    , tsFaucetSigningKeyFile :: FilePath
    -- ^ The file containing the faucet's signing key.
    , tsFaucetAddress :: AddressInEra era
    -- ^ The faucet address.
    , tsExecutionMode :: ExecutionMode
    , tsTests :: [a]
    -- ^ Input for the tests.
    , tsRuntime :: RuntimeConfig
    , tsConcurrentRunners :: ConcurrentRunners
    , tsReportingStrategy :: Maybe ReportingStrategy
    , tsMaxRetries :: Int
    }
    deriving stock (Eq, Generic, Show)

newtype TestName = TestName String
  deriving stock (Eq, Ord, Generic, Show)

instance FromJSON TestName where
  parseJSON json = TestName <$> A.parseJSON json

instance ToJSON TestName where
  toJSON (TestName name) = A.toJSON name

-- | An on-chain test of the Marlowe contract and payout validators.
data TestCase =
  TestCase
  {
    testName :: TestName -- ^ The name of the test.
  , operations :: [TestOperation] -- ^ The sequence of test operations.
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON) --, ToJSON)

data FailureError
  = InterpreterError InterpreterError
  | RuntimeMonitorError Runtime.RuntimeError
  deriving stock (Eq, Generic, Show)

instance A.FromJSON FailureError where
  parseJSON = Operation.parseSingleFieldConstructorBasedJSON

instance A.ToJSON FailureError where
  toJSON = Operation.toSingleFieldConstructorBasedJSON

data FailureReport lang era = FailureReport
  { _frErr :: FailureError
  , _frInterpretState :: InterpretState lang era
  }

data TestResult lang era
  = TestSucceeded
    (InterpretState lang era) -- { retries :: RetryCounter, time :: Second }
  | TestFailed
    { result :: FailureReport lang era
    , retries :: [FailureReport lang era]
    }

data TestOperation =
    CLIOperation CLIOperation
  | RuntimeOperation RuntimeOperation
  | WalletOperation WalletOperation
  | Fail
    {
      soFailureMessage :: String
    }
  | Sleep
    {
      soDelay :: Second
    }
  deriving stock (Eq, Generic, Show)

-- | FIXME: Constructor names could be generically derived from the types.
cliConstructors :: [Text]
cliConstructors =
  [ "AutoRun"
  , "Initialize"
  , "Prepare"
  , "Publish"
  , "Withdraw"
  ]

runtimeConstructors :: [Text]
runtimeConstructors =
  [ "RuntimeAwaitClosed"
  , "RuntimeCreateContract"
  , "RuntimeApplyInputs"
  , "RuntimeWithdraw"
  ]

walletConstructors :: [Text]
walletConstructors =
  [ "BurnAll"
  , "CreateWallet"
  , "CheckBalance"
  , "FundWallets"
  , "Mint"
  , "SplitWallet"
  , "ReturnFunds"
  ]

divideEightBy :: Float -> Float
divideEightBy x = 8.0 / x

instance FromJSON TestOperation where
  parseJSON json = do
    let
      parseSubobject _ (A.Object (A.KeyMap.toList -> [(_, v)])) = do
        parseJSON v
      parseSubobject name _ = fail $ "Expecting object with a single key for an operation: " <> name
      parseTaggedJson tag = case (tag, json) of
        _ | tag `List.elem` cliConstructors -> CLIOperation <$> parseJSON json
        _ | tag `List.elem` runtimeConstructors -> RuntimeOperation <$> parseJSON json
        _ | tag `List.elem` walletConstructors -> WalletOperation <$> parseJSON json
        _ | tag == "Fail" -> do
          obj <- parseSubobject "Fail" json
          Fail <$> obj .: "failureMessage"
        _ | tag == "Sleep" -> do
          obj <- parseSubobject "Sleep" json
          Sleep <$> do
            (seconds :: Integer) <- obj .: "seconds"
            pure $ fromMicroseconds (seconds * 1_000_000)
        _ -> fail $ "Unknown tag: " <> T.unpack tag
    case json of
      (A.Object (A.KeyMap.toList -> [(k, _)])) -> do
        let
          tag = A.Key.toText k
        parseTaggedJson tag
      (A.String tag) -> do
        parseTaggedJson tag
      _ -> fail $ "Expected a tagged object or string, got: " <> show json

instance ToJSON TestOperation where
  toJSON (CLIOperation op) = toJSON op
  toJSON (RuntimeOperation op) = toJSON op
  toJSON (WalletOperation op) = toJSON op
  toJSON (Fail msg) = A.object [("Fail", toJSON msg)]
  toJSON (Sleep seconds) = A.object [("Sleep", toJSON $ toMicroseconds seconds `div` 1_000_000)]

-- Let's serialize it to object of objects:
plutusValueToJSON :: P.Value -> A.Value
plutusValueToJSON (P.Value value) = do
  let
    -- `AssocMap` has not `Functor` instance
    -- so we have to convert to list it and then map
    -- over it.
    value' = fmap (fmap P.AssocMap.toList) $ P.AssocMap.toList value
  A.object $ value' <&> \(cs, inner) -> do
    let
      inner' = A.object $ bimap (A.Key.fromString . P.Value.toString) A.toJSON <$> inner
    (A.Key.fromString . show $ cs, inner')

data InterpretState lang era = InterpretState
  {
    -- We should flatten these two contracInfo representations.
    _isCLIContracts :: CLIContracts lang era
  , _isKnownContracts :: Map ContractNickname Runtime.ContractInfo
  , _isPublishedScripts :: Maybe (MarloweScriptsRefs MarlowePlutusVersion era)
  , _isCurrencies :: Currencies
  , _isWallets :: Wallets era
  , _isLogs :: Logs
  }

mkInterpretState :: Wallets era -> InterpretState lang era
mkInterpretState wallets = InterpretState
  {
    _isCLIContracts = CLIContracts mempty
  , _isPublishedScripts = Nothing
  , _isCurrencies = Currencies mempty
  , _isWallets = wallets
  , _isKnownContracts = mempty
  , _isLogs = mempty
  }

data InterpretEnv lang era = InterpretEnv
  {
    _ieRuntimeMonitor :: Maybe (RuntimeMonitorInput, RuntimeMonitorState)
  , _ieRuntimeClientConnector :: Maybe (Network.Protocol.SomeClientConnector Marlowe.Protocol.MarloweRuntimeClient IO)
  , _ieExecutionMode :: ExecutionMode
  , _ieConnection :: LocalNodeConnectInfo CardanoMode
  , _ieEra :: ScriptDataSupportedInEra era
  , _iePrintStats :: PrintStats
  , _ieSlotConfig :: SlotConfig
  , _ieCostModelParams :: CostModelParams
  , _ieProtocolVersion :: ProtocolVersion
  }

type InterpretMonad m lang era =
  ( MonadState (InterpretState lang era) m
  , MonadReader (InterpretEnv lang era) m
  , MonadError InterpreterError m
  , MonadIO m
  )

makeLenses 'InterpretEnv
makeLenses 'InterpretState

instance Log.HasLogStore (InterpretState lang era) where
  logStoreL = isLogs

instance Wallet.HasInterpretEnv (InterpretEnv lang era) era where
  connectionL = ieConnection
  eraL = ieEra
  printStatsL = iePrintStats
  executionModeL = ieExecutionMode

instance Wallet.HasInterpretState (InterpretState lang era) era where
  walletsL = isWallets
  currenciesL = isCurrencies

instance CLI.HasInterpretEnv (InterpretEnv lang era) lang era where
  connectionL = ieConnection
  eraL = ieEra
  printStatsL = iePrintStats
  executionModeL = ieExecutionMode
  slotConfigL = ieSlotConfig
  costModelParamsL = ieCostModelParams
  protocolVersionL = ieProtocolVersion

instance CLI.HasInterpretState (InterpretState lang era) lang era where
  walletsL = isWallets
  currenciesL = isCurrencies
  contractsL = isCLIContracts
  publishedScriptsL = isPublishedScripts

instance Runtime.HasInterpretState (InterpretState lang era) era where
  knownContractsL = isKnownContracts
  walletsL = isWallets
  currenciesL = isCurrencies

instance Runtime.HasInterpretEnv (InterpretEnv lang era) era where
  runtimeMonitorStateT = ieRuntimeMonitor <<< _Just <<< _2
  runtimeMonitorInputT = ieRuntimeMonitor <<< _Just <<< _1
  runtimeClientConnectorT = ieRuntimeClientConnector . _Just
  executionModeL = ieExecutionMode
  connectionT = ieConnection
  eraL = ieEra

-- txId = overSomeTxBody (const C.getTxId)
someTxBodyToJSON :: SomeTxBody -> A.Value
someTxBodyToJSON someTxBody = A.object
  [ "txId" .= overSomeTxBody' C.getTxId someTxBody
  , "txFee" .= overSomeTxBody' txBodyFee someTxBody
  ]

walletToJSON :: IsCardanoEra era => Wallet era -> A.Value
walletToJSON Wallet {..} = A.object
  [ "address" .= _waAddress
  , "balanceCheckBaseline" .= plutusValueToJSON _waBalanceCheckBaseline
  , "txs" .= (_waSubmittedTransactions <&> someTxBodyToJSON)
  ]

currencyToJSON :: Currency -> A.Value
currencyToJSON Currency {..} = A.object
  [ "issuer" .= ccIssuer
  , "policyId" .= ccPolicyId
  ]

interpretStateToJSONPairs
  :: forall a era lang
   . IsCardanoEra era
  => Aeson.KeyValue a
  => InterpretState lang era
  -> [a]
interpretStateToJSONPairs InterpretState{..} =
  [ "wallets" .= do
      let
        step n wallet = [(n, walletToJSON wallet)]
      Map.foldMapWithKey step $ getAllWallets _isWallets
  , "currencies" .= do
      let
        step n curr = [(n, currencyToJSON curr)]
      Map.foldMapWithKey step $ unCurrencies _isCurrencies
  ]

failureReportToJSON
  :: forall era lang
   . IsCardanoEra era
  => FilePath
  -> TestName
  -> FailureReport lang era
  -> [FailureReport lang era]
  -> Aeson.Value
failureReportToJSON testFile (TestName name) failure@FailureReport{ _frInterpretState = interpretState@InterpretState{..} } retries = do
  let
    err = _frErr failure
    -- It is a bit easier to read the log when we have the error message itself at the end.
    logs = case err of
      InterpreterError err' -> reverse (("Error", err' ^. ieMessage, []) : _isLogs)
      _ -> reverse _isLogs
  A.object $
    [ "name" .= name
    , "testSource" .= testFile
    , "result" A..= ("failed" :: String)
    , "error" .= err
    , "retries" .= fmap _frErr retries
    , "logs" .= logs
    ]
    <> interpretStateToJSONPairs interpretState

testResultToJSON
  :: IsCardanoEra era
  => FilePath
  -> TestName
  -> TestResult lang era
  -> Aeson.Value
testResultToJSON testFile testName@(TestName name) = \case
  TestSucceeded interpretState -> A.object $
    [ "testName" A..= name
    , "testSource" A..= testFile
    , "result" A..= ("passed" :: String)
    ]
    <> interpretStateToJSONPairs interpretState
  TestFailed err retries -> failureReportToJSON testFile testName err retries

newtype FaucetsNumber = FaucetsNumber Int

