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
{-# LANGUAGE ViewPatterns #-}


module Language.Marlowe.CLI.Test.Runtime.Types where

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
  , SomePaymentSigningKey
  , SomeTimeout, Seconds
  )
import qualified Language.Marlowe.Core.V1.Semantics.Types as M
import qualified Language.Marlowe.Extended.V1 as E
import Ledger.Orphans ()
import Plutus.ApiCommon (ProtocolVersion)
import Plutus.V1.Ledger.Api (CostModelParams, CurrencySymbol, TokenName)
import Plutus.V1.Ledger.SlotConfig (SlotConfig)
import qualified Plutus.V1.Ledger.Value as P
import Text.Read (readMaybe)
import qualified Language.Marlowe.Runtime.Core.Api as Runtime.Core.Api
import qualified Language.Marlowe.Runtime.Cardano.Api as Runtime.Cardano.Api

import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient)
import Language.Marlowe.Runtime.App.Types (Client)
import Observe.Event.Dynamic (DynamicEventSelector)
import Observe.Event.Backend (EventBackend)
import Observe.Event.Render.JSON.Handle (JSONRef)
import Control.Concurrent.STM (TChan, TVar)
import qualified Language.Marlowe.Runtime.App.Stream as Runtime.App
import Language.Marlowe.Runtime.Core.Api (MarloweVersionTag(V1), ContractId)
import Data.Set (Set)
import Language.Marlowe.Runtime.ChainSync.Api (SlotNo)

data RuntimeError
  = RuntimeConnectionError
  | RuntimeExecutionFailure String
  | RuntimeContractNotFound ContractId
  | RuntimeRollbackError ContractId

newtype RuntimeContractInfo era lang =
  RuntimeContractInfo (AnyRuntimeMarloweThread era lang)

newtype RuntimeMonitorInput era lang = RuntimeMonitorInput (TChan (ContractNickname, ContractId))

newtype RuntimeState era lang = RuntimeState (TVar (Map ContractId (RuntimeContractInfo era lang)))

newtype RuntimeMonitor = RuntimeMonitor (IO RuntimeError)

