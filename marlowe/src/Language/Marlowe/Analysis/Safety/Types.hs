-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Types for safety analysis for Marlowe contracts.
module Language.Marlowe.Analysis.Safety.Types (
  -- * Types
  SafetyError (..),
  SafetyReport (..),
  Transaction (..),
) where

import Data.Aeson (ToJSON (..), object, (.=))
import Language.Marlowe.Core.V1.Semantics (TransactionInput, TransactionOutput)
import Language.Marlowe.Core.V1.Semantics.Types (AccountId, ChoiceId, Contract, State, Token, ValueId)
import Language.Marlowe.Core.V1.Semantics.Types.Address (Network)
import Numeric.Natural (Natural)
import Plutus.V2.Ledger.Api (CurrencySymbol, DatumHash, ExBudget, TokenName)

import qualified Language.Marlowe.Core.V1.Semantics as V1 (TransactionWarning)
import qualified Plutus.V2.Ledger.Api as Ledger (Address)

-- | Information on the safety of a Marlowe contract and state.
data SafetyReport = SafetyReport
  { safetyErrors :: [SafetyError]
  -- ^ Safety-related errors in the contract.
  , boundOnMinimumUtxo :: Maybe Integer
  -- ^ A bound on the minimum-UTxO value, over all execution paths.
  , boundOnDatumSize :: Natural
  -- ^ A bound (in bytes) on the size of the datum, over all execution paths.
  , boundOnRedeemerSize :: Natural
  -- ^ A bound (in bytes) on the size of the redeemer, over all execution paths.
  , networks :: [Network]
  -- ^ Which network the contract must use.
  }
  deriving (Show)

-- | An unsafe aspect of a Marlowe contract.
data SafetyError
  = -- | Roles are present but there is no roles currency.
    MissingRolesCurrency
  | -- | No roles are present but there is a roles currency.
    ContractHasNoRoles
  | -- | A required role is not minted.
    MissingRoleToken TokenName
  | -- | A role is minted but not required.
    ExtraRoleToken TokenName
  | -- | A role name is longer than the 32 bytes allowed by the ledger.
    RoleNameTooLong TokenName
  | -- | The currency symbol for a native asset is not 28 bytes long.
    InvalidCurrencySymbol CurrencySymbol
  | -- | A token name is longer than the 32 bytes allowed by the ledger.
    TokenNameTooLong TokenName
  | -- | A token name is associated with the ada symbol.
    InvalidToken Token
  | -- | Initial account balance is not positive.
    NonPositiveBalance AccountId Token
  | -- | Duplicate account in state.
    DuplicateAccount AccountId Token
  | -- | Duplicate choice in state.
    DuplicateChoice ChoiceId
  | -- | Duplicate bound value in state.
    DuplicateBoundValue ValueId
  | -- | Too many tokens might be stored at some point in the contract.
    MaximumValueMayExceedProtocol Natural
  | -- | The transaction size (in bytes) might be too large.
    TransactionSizeMayExceedProtocol Transaction Natural
  | -- | The transaction's execution cost might be too high.
    TransactionCostMayExceedProtocol Transaction ExBudget
  | -- | The transaction does not validate.
    TransactionValidationError Transaction String
  | -- | The transacttion has warnings.
    TransactionWarning V1.TransactionWarning
  | -- | The contract is missing a continuation not present in its continuation map.
    MissingContinuation DatumHash
  | -- | The contract contains both mainnet and testnet addresses.
    InconsistentNetworks
  | -- | The contract contains invalid addresses for the network.
    WrongNetwork
  | -- | The contract contains an illegal ledger address.
    IllegalAddress Ledger.Address
  deriving (Eq, Show)

-- | A Marlowe transaction.
data Transaction = Transaction
  { txState :: State
  , txContract :: Contract
  , txInput :: TransactionInput
  , txOutput :: TransactionOutput
  }
  deriving (Eq, Show)

instance ToJSON Transaction where
  toJSON Transaction{..} =
    object
      [ "state" .= txState
      , "contract" .= txContract
      , "input" .= txInput
      , "output" .= txOutput
      ]
