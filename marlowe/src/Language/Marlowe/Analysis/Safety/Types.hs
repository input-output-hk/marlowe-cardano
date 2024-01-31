{-# LANGUAGE DerivingStrategies #-}
-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Types for safety analysis for Marlowe contracts.
module Language.Marlowe.Analysis.Safety.Types (
  -- * Types
  SafetyError (..),
  SafetyReport (..),
  Transaction (..),
  stripAnnotation,
) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.ByteString.Base16.Aeson (EncodeBase16 (EncodeBase16))
import Data.String (fromString)
import GHC.Generics (Generic)
import Language.Marlowe.Core.V1.Semantics (TransactionInput, TransactionOutput)
import Language.Marlowe.Core.V1.Semantics.Types (AccountId, ChoiceId, Contract, State, Token, ValueId)
import Language.Marlowe.Core.V1.Semantics.Types.Address (Network)
import Numeric.Natural (Natural)
import PlutusLedgerApi.V2 (
  Credential (..),
  CurrencySymbol (..),
  DatumHash (..),
  ExBudget,
  StakingCredential (..),
  TokenName (..),
  fromBuiltin,
  toBuiltin,
 )

import qualified Data.Text.Encoding as T (decodeUtf8, encodeUtf8)
import qualified Language.Marlowe.Core.V1.Semantics as V1 (TransactionWarning)
import qualified PlutusLedgerApi.V2 as Ledger (Address (..))

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
    TransactionSizeMayExceedProtocol TransactionWithoutAnnotation Natural
  | -- | The transaction's execution cost might be too high.
    TransactionCostMayExceedProtocol TransactionWithoutAnnotation ExBudget
  | -- | The transaction does not validate.
    TransactionValidationError TransactionWithoutAnnotation String
  | -- | The transaction has warnings.
    TransactionWarning TransactionWithoutAnnotation V1.TransactionWarning
  | -- | The contract is missing a continuation not present in its continuation map.
    MissingContinuation DatumHash
  | -- | The contract contains both mainnet and testnet addresses.
    InconsistentNetworks
  | -- | The contract contains invalid addresses for the network.
    WrongNetwork
  | -- | The contract contains an illegal ledger address.
    IllegalAddress Ledger.Address
  | -- | The safety analysis exceeded the allotted time.
    SafetyAnalysisTimeout
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
      , "detail"
          .= ("In the initial state of the contract, there are duplicate entries for this account with this token." :: String)
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
      , "detail"
          .= ( "At some point in during its executation, the contract may hold more native tokens than permitted by the ledger rules."
                :: String
             )
      , "bytes" .= natural
      , "fatal" .= False
      ]
  toJSON (TransactionSizeMayExceedProtocol transaction natural) =
    object
      [ "error" .= ("TransactionSizeMayExceedProtocol" :: String)
      , "detail" .= ("This transaction's size may exceed the limit permitted by the ledger rules." :: String)
      , "transaction" .= transaction
      , "bytes" .= natural
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
      , "detail"
          .= ("The contract contains addresses that are do not match the network on which it will be executed." :: String)
      , "fatal" .= True
      ]
  toJSON (IllegalAddress Ledger.Address{..}) =
    object
      [ "error" .= ("IllegalAddress" :: String)
      , "detail"
          .= ("The contract contains this address that is invalid for the network on which it will be executed." :: String)
      , "address"
          .= let credentialToJSON (PubKeyCredential hash) =
                  object
                    [ "pubKeyCredential" .= show hash
                    ]
                 credentialToJSON (ScriptCredential hash) =
                  object
                    [ "scriptCredential" .= show hash
                    ]
                 stakingCredentialToJSON (StakingHash credential) =
                  object
                    [ "stakingHash" .= credentialToJSON credential
                    ]
                 stakingCredentialToJSON (StakingPtr x y z) =
                  object
                    [ "stakingPtr" .= (x, y, z)
                    ]
              in object
                  [ "addressCredential" .= credentialToJSON addressCredential
                  , "addressStakingCredential" .= fmap stakingCredentialToJSON addressStakingCredential
                  ]
      , "fatal" .= True
      ]
  toJSON SafetyAnalysisTimeout =
    object
      [ "error" .= ("SafetyAnalysisTimeout" :: String)
      , "detail" .= ("The safety analysis exceeded the allotted time." :: String)
      , "fatal" .= False
      ]

instance FromJSON SafetyError where
  parseJSON =
    withObject "SafetyError" $
      \o ->
        (o .: "error" :: Parser String)
          >>= \case
            "MissingRolesCurrency" -> pure MissingRolesCurrency
            "ContractHasNoRoles" -> pure ContractHasNoRoles
            "MissingRoleToken" -> MissingRoleToken . TokenName . toBuiltin . T.encodeUtf8 <$> o .: "role-name"
            "ExtraRoleToken" -> ExtraRoleToken . TokenName . toBuiltin . T.encodeUtf8 <$> o .: "role-name"
            "RoleNameTooLong" -> RoleNameTooLong . TokenName . toBuiltin . T.encodeUtf8 <$> o .: "role-name"
            "InvalidCurrencySymbol" ->
              do
                EncodeBase16 bs <- parseJSON =<< o .: "currency-symbol"
                pure . InvalidCurrencySymbol . CurrencySymbol $ toBuiltin bs
            "TokenNameTooLong" -> TokenNameTooLong . TokenName . toBuiltin . T.encodeUtf8 <$> o .: "token-name"
            "InvalidToken" -> InvalidToken <$> o .: "token"
            "NonPositiveBalance" -> NonPositiveBalance <$> o .: "account-id" <*> o .: "token"
            "DuplicateAccount" -> DuplicateAccount <$> o .: "account-id" <*> o .: "token"
            "DuplicateChoice" -> DuplicateChoice <$> o .: "choice-id"
            "DuplicateBoundValue" -> DuplicateBoundValue <$> o .: "value-id"
            "MaximumValueMayExceedProtocol" -> MaximumValueMayExceedProtocol <$> o .: "bytes"
            "TransactionSizeMayExceedProtocol" -> TransactionSizeMayExceedProtocol <$> o .: "transaction" <*> o .: "bytes"
            "TransactionCostMayExceedProtocol" -> TransactionCostMayExceedProtocol <$> o .: "transaction" <*> o .: "cost"
            "TransactionValidationError" -> TransactionValidationError <$> o .: "transaction" <*> o .: "message"
            "TransactionWarning" -> TransactionWarning <$> o .: "transaction" <*> o .: "warning"
            "MissingContinuation" ->
              do
                EncodeBase16 bs <- parseJSON =<< o .: "hash"
                pure . MissingContinuation . DatumHash $ toBuiltin bs
            "InconsistentNetworks" -> pure InconsistentNetworks
            "WrongNetwork" -> pure WrongNetwork
            "IllegalAddress" ->
              do
                let pubKeyCredentialFromJSON =
                      withObject "PubKeyCredential" $
                        \o' -> PubKeyCredential . fromString <$> o' .: "pubKeyCredential"
                    scriptCredentialFromJSON =
                      withObject "ScriptCredential" $
                        \o' -> ScriptCredential . fromString <$> o' .: "scriptCredential"
                    credentialFromJSON o' = pubKeyCredentialFromJSON o' <|> scriptCredentialFromJSON o'
                    stakingHashFromJSON =
                      withObject "StakingHash" $
                        \o' -> StakingHash <$> (credentialFromJSON =<< o' .: "stakingHash")
                    stakingPtrFromJSON =
                      withObject "StakingPtr" $
                        \o' ->
                          do
                            (x, y, z) <- o' .: "stakingPtr"
                            pure $ StakingPtr x y z
                    stakingCredentialFromJSON o' = stakingHashFromJSON o' <|> stakingPtrFromJSON o'
                o' <- o .: "address"
                addressCredential <- credentialFromJSON =<< o' .: "addressCredential"
                addressStakingCredential <-
                  (fmap Just $ stakingCredentialFromJSON =<< o' .: "stakingCredential")
                    <|> pure Nothing
                pure . IllegalAddress $ Ledger.Address{..}
            "SafetyAnalysisTimeout" -> pure SafetyAnalysisTimeout
            _ -> fail "Invalid safety error."

-- | A Marlowe transaction.
data Transaction a = Transaction
  { txState :: State
  , txContract :: Contract
  , txInput :: TransactionInput
  , txOutput :: TransactionOutput
  , txAnnotation :: a
  }
  deriving (Eq, Generic, Show)

instance {-# OVERLAPPING #-} ToJSON (Transaction ()) where
  toJSON Transaction{..} =
    object
      [ "state" .= txState
      , "contract" .= txContract
      , "input" .= txInput
      , "output" .= txOutput
      ]

instance (ToJSON a) => ToJSON (Transaction a) where
  toJSON Transaction{..} =
    object
      [ "state" .= txState
      , "contract" .= txContract
      , "input" .= txInput
      , "output" .= txOutput
      , "annotation" .= txAnnotation
      ]

instance {-# OVERLAPPING #-} FromJSON (Transaction ()) where
  parseJSON =
    withObject "Transaction" $
      \o ->
        do
          txState <- o .: "state"
          txContract <- o .: "contract"
          txInput <- o .: "input"
          txOutput <- o .: "output"
          let txAnnotation = ()
          pure Transaction{..}

instance (FromJSON a) => FromJSON (Transaction a) where
  parseJSON =
    withObject "Transaction" $
      \o ->
        do
          txState <- o .: "state"
          txContract <- o .: "contract"
          txInput <- o .: "input"
          txOutput <- o .: "output"
          txAnnotation <- o .: "annotation"
          pure Transaction{..}

type TransactionWithoutAnnotation = Transaction ()

stripAnnotation
  :: Transaction a
  -> Transaction ()
stripAnnotation Transaction{txState, txContract, txInput, txOutput} =
  let txAnnotation = ()
   in Transaction{..}
