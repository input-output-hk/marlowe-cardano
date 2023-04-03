-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Types for safety analysis for Marlowe contracts.
--
-----------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.Analysis.Safety.Types
  ( -- * Types
    SafetyError(..)
  , SafetyReport(..)
  , Transaction(..)
  ) where


import Data.Aeson (ToJSON(..), object, (.=))
import Language.Marlowe.Core.V1.Semantics (TransactionInput, TransactionOutput)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, State, Token)
import Plutus.V2.Ledger.Api (CurrencySymbol, ExBudget, TokenName)


-- | Information on the safety of a Marlowe contract and state.
data SafetyReport =
  SafetyReport
  {
    safetyErrors :: [SafetyError]  -- ^ Safety-related errors in the contract.
  , boundOnMinimumUtxo :: Int  -- ^ A bound on the minimum-UTxO value, over all execution paths.
  , boundOnDatumSize :: Int  -- ^ A bound (in bytes) on the size of the datum, over all execution paths.
  , boundOnRedeemerSize :: Int  -- ^ A bound (in bytes) on the size of the redeemer, over all execution paths.
  }
    deriving Show


-- | An unsafe aspect of a Marlowe contract.
data SafetyError =
    -- | Roles are present but there is no roles currency.
    MissingRolesCurrency
    -- | A role name is longer than the 32 bytes allowed by the ledger.
  | RoleNameTooLong TokenName
    -- | The currency symbol for a native asset is not 28 bytes long.
  | InvalidCurrencySymbol CurrencySymbol
    -- | A token name is longer than the 32 bytes allowed by the ledger.
  | TokenNameTooLong TokenName
    -- | A token name is associated with the ada symbol.
  | InvalidToken Token
    -- | Too many tokens might be stored at some point in the contract.
  | MaximumValueMayExceedProtocol Int
    -- | The transaction size (in bytes) might be too large.
  | TransactionSizeMayExceedProtocol Transaction Int
    -- | The transaction's execution cost might be too high.
  | TransactionCostMayExceedProtocol Transaction ExBudget
    deriving Show


-- | A Marlowe transaction.
data Transaction =
  Transaction
  {
    txState :: State
  , txContract :: Contract
  , txInput :: TransactionInput
  , txOutput :: TransactionOutput
  }
    deriving (Show)

instance ToJSON Transaction where
  toJSON Transaction{..} =
    object
      [
        "state"    .= txState
      , "contract" .= txContract
      , "input"    .= txInput
      , "output"   .= txOutput
      ]
