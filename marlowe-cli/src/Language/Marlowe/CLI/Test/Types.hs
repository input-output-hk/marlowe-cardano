-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Types for testing Marlowe contracts.
--
-----------------------------------------------------------------------------


{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}


module Language.Marlowe.CLI.Test.Types
  where
--   ( MarloweTests(..)
--   , ScriptTest(..)
--   , TokenName(..)
--   -- , ssRuntimeContractStreams
--   -- , ssRuntimeKnownContracts
--   ) where

import Cardano.Api
  (AddressInEra, CardanoMode, LocalNodeConnectInfo, Lovelace, NetworkId, PolicyId, ScriptDataSupportedInEra, TxBody)
import qualified Cardano.Api as C
import Control.Lens (Lens', makeLenses)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON(..), ToJSON(..), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as A
import qualified Data.Fixed as F
import qualified Data.Fixed as Fixed
import Data.Foldable (fold)
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
import Control.Concurrent.STM (TChan, TVar)
import Control.Lens.Lens (lens)
import Control.Monad.State.Class (MonadState)
import Data.Set (Set)
import Language.Marlowe.CLI.Test.CLI.Types (AnyCLIMarloweThread, CLIContracts(CLIContracts), CLIOperation)
import qualified Language.Marlowe.CLI.Test.CLI.Types as CLI
import Language.Marlowe.CLI.Test.Contract (ContractNickname(ContractNickname))
import Language.Marlowe.CLI.Test.ExecutionMode (ExecutionMode)
import Language.Marlowe.CLI.Test.Runtime.Types
  (RuntimeMonitorInput(RuntimeMonitorInput), RuntimeMonitorState(RuntimeMonitorState), RuntimeOperation)
import qualified Language.Marlowe.CLI.Test.Runtime.Types as Runtime
import Language.Marlowe.CLI.Test.Wallet.Types
  (Currencies(Currencies), CurrencyNickname, WalletOperation, Wallets(Wallets))
import qualified Language.Marlowe.CLI.Test.Wallet.Types as Wallet
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient)
import qualified Language.Marlowe.Runtime.App.Stream as Runtime.App
import Language.Marlowe.Runtime.App.Types (Client)
import Language.Marlowe.Runtime.ChainSync.Api (SlotNo)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag(V1))
import Observe.Event.Backend (EventBackend)
import Observe.Event.Dynamic (DynamicEventSelector)
import Observe.Event.Render.JSON.Handle (JSONRef)


-- | Configuration for a set of Marlowe tests.
data MarloweTests era a =
    -- | Test contracts on-chain.
    ScriptTests
    {
      stNetwork :: NetworkId   -- ^ The network ID, if any.
    , stSocketPath :: FilePath    -- ^ The path to the node socket.
    , stFaucetSigningKeyFile :: FilePath    -- ^ The file containing the faucet's signing key.
    , stFaucetAddress :: AddressInEra era  -- ^ The faucet address.
    , stExecutionMode :: ExecutionMode
    , stTests :: [a]         -- ^ Input for the tests.
    }
    deriving stock (Eq, Generic, Show)

-- | An on-chain test of the Marlowe contract and payout validators.
data ScriptTest =
  ScriptTest
  {
    stTestName         :: String             -- ^ The name of the test.
  , stScriptOperations :: [TestOperation]  -- ^ The sequence of test operations.
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

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

-- FIXME:
-- * We should improve the error reporting here by manually
-- checking the `tag` and then parsing appropriate sub-operation.
-- * Improve `Fail` parsing / printing.
instance FromJSON TestOperation where
  parseJSON json =
    CLIOperation <$> parseJSON json
    <|> RuntimeOperation <$> parseJSON json
    <|> WalletOperation <$> parseJSON json
    <|> Fail <$> parseJSON json

instance ToJSON TestOperation where
  toJSON (CLIOperation op) = toJSON op
  toJSON (RuntimeOperation op) = toJSON op
  toJSON (WalletOperation op) = toJSON op
  toJSON (Fail msg) = toJSON msg

data InterpretState lang era = InterpretState
  {
    _ssContracts :: CLIContracts lang era
  , _ssReferenceScripts :: Maybe (MarloweScriptsRefs MarlowePlutusVersion era)
  , _ssCurrencies :: Currencies
  , _ssWallets :: Wallets era
  , _isKnownContracts :: Map ContractNickname ContractId
  }

data InterpretEnv lang era = InterpretEnv
  {
    _ieRuntimeMonitorState :: RuntimeMonitorState lang era
  , _ieRuntimeMonitorInput :: RuntimeMonitorInput
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

toCLIInterpretEnv :: InterpretEnv lang era -> CLI.InterpretEnv lang era
toCLIInterpretEnv (InterpretEnv _ _ executionMode connection era printStats slotConfig costModelParams protocolVersion) =
  CLI.InterpretEnv connection era printStats executionMode slotConfig costModelParams protocolVersion

toRuntimeInterpretEnv :: InterpretEnv lang era -> Runtime.InterpretEnv lang era
toRuntimeInterpretEnv (InterpretEnv runtimeMonitorState runtimeMonitorInput executionMode connection era printStats slotConfig costModelParams protocolVersion) =
  Runtime.InterpretEnv runtimeMonitorState runtimeMonitorInput executionMode

toWalletInterpretEnv :: InterpretEnv lang era -> Wallet.InterpretEnv era
toWalletInterpretEnv (InterpretEnv _ _ executionMode connection era printStats _ _ _) =
  Wallet.InterpretEnv connection era printStats executionMode

cliInpterpretStateL :: Lens' (InterpretState lang era) (CLI.InterpretState lang era)
cliInpterpretStateL = lens toCLIInterpretState fromCLIInterpretState
  where
    toCLIInterpretState (InterpretState contracts referenceScripts currencies wallets knownContracts) =
      CLI.InterpretState wallets currencies contracts referenceScripts knownContracts
    fromCLIInterpretState
      InterpretState {}
      (CLI.InterpretState wallets currencies contracts referenceScripts knownContracts) =
      InterpretState contracts referenceScripts currencies wallets knownContracts

runtimeInpterpretStateL :: Lens' (InterpretState lang era) Runtime.InterpretState
runtimeInpterpretStateL = lens toRuntimeInterpretState fromRuntimeInterpretState
  where
    toRuntimeInterpretState (InterpretState contracts referenceScripts currencies wallets knownContracts) =
      Runtime.InterpretState knownContracts
    fromRuntimeInterpretState
      (InterpretState contracts referenceScripts currencies wallets _)
      (Runtime.InterpretState knownContracts) =
      InterpretState contracts referenceScripts currencies wallets knownContracts

walletInpterpretStateL :: Lens' (InterpretState lang era) (Wallet.InterpretState era)
walletInpterpretStateL = lens toWalletInterpretState fromWalletInterpretState
  where
    toWalletInterpretState (InterpretState contracts referenceScripts currencies wallets knownContracts) =
      Wallet.InterpretState wallets currencies
    fromWalletInterpretState
      (InterpretState contracts referenceScripts _ _ knownContracts)
      (Wallet.InterpretState wallets currencies) =
      InterpretState contracts referenceScripts currencies wallets knownContracts

makeLenses 'InterpretEnv
makeLenses 'InterpretState

