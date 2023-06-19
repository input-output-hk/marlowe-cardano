-- editorconfig-checker-disable-file

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


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.Analysis.Safety.Types
  ( -- * Types
    SafetyError(..)
  , SafetyReport(..)
  , Transaction(..)
  ) where


import Data.Aeson (ToJSON(..), Value(String), object, (.=))
import Data.ByteString.Base16.Aeson (EncodeBase16(EncodeBase16))
import GHC.Generics (Generic)
import Language.Marlowe.Core.V1.Semantics (TransactionInput, TransactionOutput)
import Language.Marlowe.Core.V1.Semantics.Types (AccountId, ChoiceId, Contract, State, Token, ValueId)
import Language.Marlowe.Core.V1.Semantics.Types.Address (Network)
import Numeric.Natural (Natural)
import Plutus.V2.Ledger.Api (CurrencySymbol(..), DatumHash(..), ExBudget, TokenName(..), fromBuiltin)

import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Language.Marlowe.Core.V1.Semantics as V1 (TransactionWarning)
import qualified Plutus.V2.Ledger.Api as Ledger (Address)


-- | Information on the safety of a Marlowe contract and state.
data SafetyReport =
  SafetyReport
  {
    safetyErrors :: [SafetyError]  -- ^ Safety-related errors in the contract.
  , boundOnMinimumUtxo :: Maybe Integer  -- ^ A bound on the minimum-UTxO value, over all execution paths.
  , boundOnDatumSize :: Natural  -- ^ A bound (in bytes) on the size of the datum, over all execution paths.
  , boundOnRedeemerSize :: Natural  -- ^ A bound (in bytes) on the size of the redeemer, over all execution paths.
  , networks :: [Network]  -- ^ Which network the contract must use.
  }
    deriving Show


-- | An unsafe aspect of a Marlowe contract.
data SafetyError =
    -- | Roles are present but there is no roles currency.
    MissingRolesCurrency
    -- | No roles are present but there is a roles currency.
  | ContractHasNoRoles
    -- | A required role is not minted.
  | MissingRoleToken TokenName
    -- | A role is minted but not required.
  | ExtraRoleToken TokenName
    -- | A role name is longer than the 32 bytes allowed by the ledger.
  | RoleNameTooLong TokenName
    -- | The currency symbol for a native asset is not 28 bytes long.
  | InvalidCurrencySymbol CurrencySymbol
    -- | A token name is longer than the 32 bytes allowed by the ledger.
  | TokenNameTooLong TokenName
    -- | A token name is associated with the ada symbol.
  | InvalidToken Token
    -- | Initial account balance is not positive.
  | NonPositiveBalance AccountId Token
    -- | Duplicate account in state.
  | DuplicateAccount AccountId Token
    -- | Duplicate choice in state.
  | DuplicateChoice ChoiceId
    -- | Duplicate bound value in state.
  | DuplicateBoundValue ValueId
    -- | Too many tokens might be stored at some point in the contract.
  | MaximumValueMayExceedProtocol Natural
    -- | The transaction size (in bytes) might be too large.
  | TransactionSizeMayExceedProtocol Transaction Natural
    -- | The transaction's execution cost might be too high.
  | TransactionCostMayExceedProtocol Transaction ExBudget
    -- | The transaction does not validate.
  | TransactionValidationError Transaction String
    -- | The transaction has warnings.
  | TransactionWarning Transaction V1.TransactionWarning
    -- | The contract is missing a continuation not present in its continuation map.
  | MissingContinuation DatumHash
    -- | The contract contains both mainnet and testnet addresses.
  | InconsistentNetworks
    -- | The contract contains invalid addresses for the network.
  | WrongNetwork
    -- | The contract contains an illegal ledger address.
  | IllegalAddress Ledger.Address
    deriving (Eq, Generic, Show)


instance ToJSON SafetyError where
  toJSON MissingRolesCurrency =
    object
      [ "error" .= ("MissingRolesCurrency" :: String)
      , "detail" .= ("Roles are present in the contract, but no roles currency was specified." :: String)
      , "fatal" .= True
      ]
  toJSON ContractHasNoRoles =
    object
      [ "error" .= ("ContractHasNoRoles" :: String)
      , "detail" .= ("No roles are present in the contract, but a roles currency was specified." :: String)
      , "fatal" .= False
      ]
  toJSON (MissingRoleToken (TokenName tokenName)) =
    object
      [ "error" .= ("MissingRoleToken" :: String)
      , "detail" .= ("This role name is present in the contract, but that role token was not specified for minting." :: String)
      , "role-name" .= String (T.decodeUtf8 $ fromBuiltin tokenName)
      , "fatal" .= True
      ]
  toJSON (ExtraRoleToken (TokenName tokenName)) =
    object
      [ "error" .= ("ExtraRoleToken" :: String)
      , "detail" .= ("This role token was specified for minting, but that role is not present in the contract." :: String)
      , "role-name" .= String (T.decodeUtf8 $ fromBuiltin tokenName)
      , "fatal" .= False
      ]
  toJSON (RoleNameTooLong (TokenName tokenName)) =
    object
      [ "error" .= ("RoleNameTooLong" :: String)
      , "detail" .= ("This role name is longer than the 32 bytes allowed by the ledger rules." :: String)
      , "role-name" .= String (T.decodeUtf8 $ fromBuiltin tokenName)
      , "fatal" .= True
      ]
  toJSON (InvalidCurrencySymbol (CurrencySymbol currencySymbol)) =
    object
      [ "error" .= ("InvalidCurrencySymbol" :: String)
      , "detail" .= ("This currency symbol is not the 28-bytes required by the ledger rules." :: String)
      , "currency-symbol" .= toJSON (EncodeBase16 $ fromBuiltin currencySymbol)
      , "fatal" .= True
      ]
  toJSON (TokenNameTooLong (TokenName tokenName)) =
    object
      [ "error" .= ("TokenNameTooLong" :: String)
      , "detail" .= ("This token name is longer than the 32 bytes allowed by the ledger rules." :: String)
      , "token-name" .= String (T.decodeUtf8 $ fromBuiltin tokenName)
      , "fatal" .= True
      ]
  toJSON (InvalidToken token) =
    object
      [ "error" .= ("InvalidToken" :: String)
      , "detail" .= ("This token associates a name with the ada currency symbol." :: String)
      , "token" .= token
      , "fatal" .= True
      ]
  toJSON (NonPositiveBalance accountId token) =
    object
      [ "error" .= ("NonPositiveBalance" :: String)
      , "detail" .= ("In the initial state of the contract, ths account has a non-positive balance of this token." :: String)
      , "account-id" .= accountId
      , "token" .= token
      , "fatal" .= True
      ]
  toJSON (DuplicateAccount accountId token) =
    object
      [ "error" .= ("DuplicateAccount" :: String)
      , "detail" .= ("In the initial state of the contract, there are duplicate entries for this account with this token." :: String)
      , "account-id" .= accountId
      , "token" .= token
      , "fatal" .= True
      ]
  toJSON (DuplicateChoice choiceId) =
    object
      [ "error" .= ("DuplicateChoice" :: String)
      , "detail" .= ("In the initial state of the contract, there are duplicate entries for this choice." :: String)
      , "choice-id" .= choiceId
      , "fatal" .= True
      ]
  toJSON (DuplicateBoundValue valueId) =
    object
      [ "error" .= ("DuplicateBoundValue" :: String)
      , "detail" .= ("In the initial state of the contract, there are duplicate entries for this bound value." :: String)
      , "value-id" .= valueId
      , "fatal" .= True
      ]
  toJSON (MaximumValueMayExceedProtocol natural) =
    object
      [ "error" .= ("MaximumValueMayExceedProtocol" :: String)
      , "detail" .= ("At some point in during its executation, the contract may hold more native tokens than permitted by the ledger rules." :: String)
      , "bytes" .= natural
      , "fatal" .= False
      ]
  toJSON (TransactionSizeMayExceedProtocol transaction natural) =
    object
      [ "error" .= ("TransactionSizeMayExceedProtocol" :: String)
      , "detail" .= ("This transaction's size may exceed the limit permitted by the ledger rules." :: String)
      , "transaction" .= transaction
      , "byte" .= natural
      , "fatal" .= False
      ]
  toJSON (TransactionCostMayExceedProtocol transaction exBudget) =
    object
      [ "error" .= ("TransactionCostMayExceedProtocol" :: String)
      , "detail" .= ("This transaction's Plutus execution cost may exceed the limit permitted by the ledger rules." :: String)
      , "transaction" .= transaction
      , "cost" .= exBudget
      , "fatal" .= False
      ]
  toJSON (TransactionValidationError transaction message) =
    object
      [ "error" .= ("TransactionValidationError" :: String)
      , "detail" .= ("This transaction fails to validate on the ledger." :: String)
      , "transaction" .= transaction
      , "message" .= message
      , "fatal" .= True
      ]
  toJSON (TransactionWarning transaction warning) =
    object
      [ "error" .= ("TransactionWarning" :: String)
      , "detail" .= ("A Marlowe semantics warning is reported for this transaction." :: String)
      , "transaction" .= transaction
      , "warning" .= warning
      , "fatal" .= False
      ]
  toJSON (MissingContinuation (DatumHash datumHash)) =
    object
      [ "error" .= ("MissingContinuation" :: String)
      , "detail" .= ("The contract is missing a continuation that is not present in its map of continuations." :: String)
      , "hash" .= toJSON (EncodeBase16 $ fromBuiltin datumHash)
      , "fatal" .= False
      ]
  toJSON InconsistentNetworks =
    object
      [ "error" .= ("InconsistentNetworks" :: String)
      , "detail" .= ("The contract contains both mainnet and testnet addresses." :: String)
      , "fatal" .= True
      ]
  toJSON WrongNetwork =
     object
       [ "error" .= ("WrongNetwork" :: String)
       , "detail" .= ("The contract contains addresses that are do not match the network on which it will be executed." :: String)
       , "fatal" .= True
       ]
  toJSON (IllegalAddress address) =
    object
       [ "error" .= ("IllegalAddress" :: String)
       , "detail" .= ("The contract contains this address that is invalid for the network on which it will be executed." :: String)
       , "address" .= show address
       , "fatal" .= True
       ]


-- | A Marlowe transaction.
data Transaction =
  Transaction
  {
    txState :: State
  , txContract :: Contract
  , txInput :: TransactionInput
  , txOutput :: TransactionOutput
  }
    deriving (Eq, Generic, Show)

instance ToJSON Transaction where
  toJSON Transaction{..} =
    object
      [
        "state"    .= txState
      , "contract" .= txContract
      , "input"    .= txInput
      , "output"   .= txOutput
      ]
