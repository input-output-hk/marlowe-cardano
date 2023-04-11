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


module Language.Marlowe.CLI.Test.Types
  where

import Cardano.Api
  (AddressInEra, CardanoMode, LocalNodeConnectInfo, Lovelace, NetworkId, PolicyId, ScriptDataSupportedInEra, TxBody)
import qualified Cardano.Api as C
import Control.Lens (Lens', Prism', _1, _2, _Just, makeLenses, prism)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as A
import qualified Data.Fixed as F
import qualified Data.Fixed as Fixed
import Data.Foldable (fold)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString(fromString))
import qualified Data.Text as T
import Data.Traversable (for)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import GHC.Num (Natural)
import Language.Marlowe.CLI.Cardano.Api.Value (toPlutusValue, txOutValueValue)
import Language.Marlowe.CLI.Transaction (queryUtxos)
import Language.Marlowe.CLI.Types
  ( CliEnv
  , CliError
  , MarlowePlutusVersion
  , MarloweScriptsRefs
  , MarloweTransaction(MarloweTransaction, mtInputs)
  , PrintStats
  , Seconds
  , SomePaymentSigningKey
  , SomeTimeout
  )
import qualified Language.Marlowe.Core.V1.Semantics.Types as M
import qualified Language.Marlowe.Extended.V1 as E
import qualified Language.Marlowe.Runtime.Cardano.Api as Runtime.Cardano.Api
import qualified Language.Marlowe.Runtime.Core.Api as Runtime.Core.Api
import Ledger.Orphans ()
import Plutus.ApiCommon (ProtocolVersion)
import Plutus.V1.Ledger.Api (CostModelParams, CurrencySymbol, TokenName)
import Plutus.V1.Ledger.SlotConfig (SlotConfig)
import qualified Plutus.V1.Ledger.Value as P
import Text.Read (readMaybe)

import Control.Applicative (Alternative((<|>)))
import Control.Category ((<<<))
import Control.Concurrent.STM (TChan, TVar)
import Control.Lens.Lens (lens)
import Control.Monad.State.Class (MonadState)
import Data.Set (Set)
import Language.Marlowe.CLI.Test.CLI.Types
  (AnyCLIMarloweThread, CLIContracts(CLIContracts), CLIOperation, cliContractsIds)
import qualified Language.Marlowe.CLI.Test.CLI.Types as CLI
import Language.Marlowe.CLI.Test.Contract (ContractNickname(ContractNickname))
import Language.Marlowe.CLI.Test.ExecutionMode (ExecutionMode)
import Language.Marlowe.CLI.Test.Runtime.Types
  (RuntimeMonitorInput(RuntimeMonitorInput), RuntimeMonitorState(RuntimeMonitorState), RuntimeOperation)
import qualified Language.Marlowe.CLI.Test.Runtime.Types as Runtime
import Language.Marlowe.CLI.Test.Wallet.Types
  (Currencies(Currencies), CurrencyNickname, WalletOperation, Wallets(Wallets))
import Language.Marlowe.CLI.Test.Wallet.Types as Wallet
import qualified Language.Marlowe.CLI.Test.Wallet.Types as Wallet
import qualified Language.Marlowe.Protocol.Client as Marlowe.Protocol
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient)
import qualified Language.Marlowe.Runtime.App.Stream as Runtime.App
import Language.Marlowe.Runtime.App.Types (Client)
import Language.Marlowe.Runtime.ChainSync.Api (SlotNo)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag(V1))
import qualified Network.Protocol.Connection as Network.Protocol
import qualified Network.Protocol.Driver as Network.Protocol
import qualified Network.Protocol.Handshake.Client as Network.Protocol
import Network.Socket (PortNumber)
import Observe.Event.Backend (EventBackend)
import Observe.Event.Dynamic (DynamicEventSelector)
import Observe.Event.Render.JSON.Handle (JSONRef)

data RuntimeConfig = RuntimeConfig
  { rcRuntimeHost :: String
  , rcRuntimePort :: PortNumber
  , rcChainSeekSyncPort :: PortNumber
  , rcChainSeekCommandPort :: PortNumber
  }
    deriving stock (Eq, Generic, Show)

data TestSuite era a =
    TestSuite
    {
      stNetwork :: NetworkId              -- ^ The network ID, if any.
    , stSocketPath :: FilePath            -- ^ The path to the node socket.
    , stFaucetSigningKeyFile :: FilePath  -- ^ The file containing the faucet's signing key.
    , stFaucetAddress :: AddressInEra era -- ^ The faucet address.
    , stExecutionMode :: ExecutionMode
    , stTests :: [a]                      -- ^ Input for the tests.
    , stRuntime :: RuntimeConfig
    }
    deriving stock (Eq, Generic, Show)

-- | An on-chain test of the Marlowe contract and payout validators.
data TestCase =
  TestCase
  {
    testName :: String  -- ^ The name of the test.
  , operations :: [TestOperation] -- ^ The sequence of test operations.
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON) --, ToJSON)

-- FIXME: `CliError` was adpoted for an errror reporting as a quick
-- migration path. We should improve error reporting here.
-- We should also enhance success reporting.
data TestResult = TestSucceeded | TestFailed CliError
  deriving stock (Eq, Generic, Show)

testResultFromEither :: Either CliError a -> TestResult
testResultFromEither = either TestFailed (const TestSucceeded)

-- | On-chain test operations for the Marlowe contract and payout validators.
data TestOperation =
    CLIOperation CLIOperation
  | RuntimeOperation RuntimeOperation
  | WalletOperation WalletOperation
  | Fail
    {
      soFailureMessage :: String
    }
  deriving stock (Eq, Generic, Show)

-- | FIXME: Constructor names could be generically derived from the types.
cliConstructors :: [String]
cliConstructors =
  [ "AutoRun"
  , "Initialize"
  , "Prepare"
  , "Publish"
  , "Withdraw"
  ]

runtimeConstructors :: [String]
runtimeConstructors =
  [ "RuntimeAwaitCreated"
  , "RuntimeAwaitInputsApplied"
  , "RuntimeAwaitClosed"
  , "RuntimeCreateContract"
  , "RuntimeApplyInputs"
  ]

walletConstructors :: [String]
walletConstructors =
  [ "BurnAll"
  , "CreateWallet"
  , "CheckBalance"
  , "FundWallets"
  , "Mint"
  , "SplitWallet"
  , "ReturnFunds"
  ]

instance FromJSON TestOperation where
  parseJSON json = do
    obj <- Aeson.parseJSON json
    (tag :: String) <- obj .: "tag"
    case tag of
      tag | tag `List.elem` cliConstructors -> CLIOperation <$> parseJSON json
      tag | tag `List.elem` runtimeConstructors -> RuntimeOperation <$> parseJSON json
      tag | tag `List.elem` walletConstructors -> WalletOperation <$> parseJSON json
      tag | tag == "Fail" -> Fail <$> obj .: "failureMessage"
      _ -> fail $ "Unknown tag: " <> tag

instance ToJSON TestOperation where
  toJSON (CLIOperation op) = toJSON op
  toJSON (RuntimeOperation op) = toJSON op
  toJSON (WalletOperation op) = toJSON op
  toJSON (Fail msg) = A.object [("failureMessage", toJSON msg)]

data InterpretState lang era = InterpretState
  {
    _isCLIContracts :: CLIContracts lang era
  , _isPublishedScripts :: Maybe (MarloweScriptsRefs MarlowePlutusVersion era)
  , _isCurrencies :: Currencies
  , _isWallets :: Wallets era
  , _isKnownContracts :: Map ContractNickname ContractId
  }

data InterpretEnv lang era = InterpretEnv
  {
    _ieRuntimeMonitor :: Maybe (RuntimeMonitorInput, RuntimeMonitorState lang era)
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
  , MonadError CliError m
  , MonadIO m
  )

makeLenses 'InterpretEnv
makeLenses 'InterpretState

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

instance Runtime.HasInterpretState (InterpretState lang era) lang era where
  knownContractsL = isKnownContracts
  walletsL = isWallets
  currenciesL = isCurrencies

instance Runtime.HasInterpretEnv (InterpretEnv lang era) lang era where
  runtimeMonitorStateT = ieRuntimeMonitor <<< _Just <<< _2
  runtimeMonitorInputT = ieRuntimeMonitor <<< _Just <<< _1
  runtimeClientConnectorT = ieRuntimeClientConnector . _Just
  executionModeL = ieExecutionMode
  connectionT = ieConnection
  eraL = ieEra
