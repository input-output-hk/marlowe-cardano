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
import Control.Lens (makeLenses)
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

import Control.Concurrent.STM (TChan, TVar)
import Data.Set (Set)
import Language.Marlowe.CLI.Test.CLI.Types (AnyCLIMarloweThread, CLIOperation)
import Language.Marlowe.CLI.Test.ExecutionMode (ExecutionMode)
import Language.Marlowe.CLI.Test.Runtime.Types (RuntimeOperation)
import Language.Marlowe.CLI.Test.Wallet.Types (CurrencyNickname, WalletOperation)
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
      stNetwork              :: NetworkId   -- ^ The network ID, if any.
    , stSocketPath           :: FilePath    -- ^ The path to the node socket.
    , stFaucetSigningKeyFile :: FilePath    -- ^ The file containing the faucet's signing key.
    , stFaucetAddress        :: AddressInEra era  -- ^ The faucet address.
    , stExecutionMode        :: ExecutionMode
    , stTests                :: [a]         -- ^ Input for the tests.
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


--data UseMarloweScripts
--  = RuntimeScripts
--  | CompiledScripts
--
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
    deriving anyclass (FromJSON, ToJSON)


-- data ScriptState lang era = ScriptState
--   {
--     _ssContracts              :: Map ContractNickname (MarloweContract lang era)
--   , _ssCurrencies             :: Map CurrencyNickname Currency
--   , _ssReferenceScripts       :: Maybe (MarloweScriptsRefs lang era)
--   , _ssWallets                :: Map WalletNickname (Wallet era)                    -- ^ Faucet wallet should be included here.
--   , _ssRuntime                :: Maybe (RuntimeMonitorInput lang era, RuntimeState lang era)
--   }
--
--
-- faucetNickname :: WalletNickname
-- faucetNickname = "Faucet"
--
--
-- scriptState :: Wallet era -> ScriptState lang era
-- scriptState faucet = do
--   let
--     wallets = Map.singleton faucetNickname faucet
--   ScriptState mempty mempty Nothing wallets Nothing
--
--
-- data ScriptEnv era = ScriptEnv
--   { _seConnection         :: LocalNodeConnectInfo CardanoMode
--   , _seCostModelParams    :: CostModelParams
--   , _seEra                :: ScriptDataSupportedInEra era
--   , _seProtocolVersion    :: ProtocolVersion
--   , _seSlotConfig         :: SlotConfig
--   , _seExecutionMode      :: ExecutionMode
--   , _sePrintStats         :: PrintStats
--   , _seEventBackend       :: EventBackend IO JSONRef DynamicEventSelector
--   }
--
--
-- makeLenses 'ScriptEnv
-- makeLenses 'ScriptState
--
