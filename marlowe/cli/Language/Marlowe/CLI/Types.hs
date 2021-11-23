-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Types for Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}


module Language.Marlowe.CLI.Types (
-- * Marlowe Transactions
  MarloweInfo(..)
, ValidatorInfo(..)
, DatumInfo(..)
, RedeemerInfo(..)
-- * Exceptions
, CliError(..)
, liftCli
-- * Marlowe CLI Commands
, Command(..)
) where


import           Cardano.Api                  (AddressInEra, IsCardanoEra, NetworkId, PlutusScriptV1, Script, SlotNo,
                                               StakeAddressReference, serialiseAddress, serialiseToTextEnvelope)
import           Control.Monad.Except         (MonadError, liftEither)
import           Data.Aeson                   (ToJSON (..), Value, object, (.=))
import           Data.Bifunctor               (first)
import           Data.ByteString.Short        (ShortByteString)
import           Data.String                  (IsString)
import           GHC.Generics                 (Generic)
import           Language.Marlowe.CLI.Orphans ()
import           Language.Marlowe.Semantics   (MarloweData (..))
import           Ledger.Typed.Scripts         (TypedValidator)
import           Plutus.V1.Ledger.Api         (CurrencySymbol, Datum, DatumHash, ExBudget, Redeemer, ValidatorHash)


-- | Exception for Marlowe CLI.
newtype CliError = CliError {unCliError :: String}
  deriving (Eq, IsString, Ord, Read, Show)


-- | Lift an "Either" result into the CLI.
liftCli :: MonadError CliError m
        => Show e
        => Either e a  -- ^ The result.
        -> m a         -- ^ The lifted result.
liftCli = liftEither . first (CliError . show)


-- | Comprehensive information about a Marlowe transaction.
data MarloweInfo era =
  MarloweInfo
  {
    validatorInfo :: ValidatorInfo era  -- ^ Validator information.
  , datumInfo     :: DatumInfo          -- ^ Datum information.
  , redeemerInfo  :: RedeemerInfo       -- ^ Redeemer information.
  }
    deriving (Eq, Generic, Show)

instance IsCardanoEra era => ToJSON (MarloweInfo era) where
  toJSON MarloweInfo{..} =
    object
      [
        "validator" .= toJSON validatorInfo
      , "datum"     .= toJSON datumInfo
      , "redeemer"  .= toJSON redeemerInfo
      ]


-- | Information about Marlowe validator.
data ValidatorInfo era =
  ValidatorInfo
  {
    viValidator :: TypedValidator MarloweData  -- ^ The validator.
  , viScript    :: Script PlutusScriptV1       -- ^ The Plutus script.
  , viBytes     :: ShortByteString             -- ^ The serialisation of the validator.
  , viHash      :: ValidatorHash               -- ^ The validator hash.
  , viAddress   :: AddressInEra era            -- ^ The script address.
  , viSize      :: Int                         -- ^ The script size, in bytes.
  , viCost      :: ExBudget                    -- ^ The execution budget for the script.
  }
    deriving (Eq, Generic, Show)

instance IsCardanoEra era => ToJSON (ValidatorInfo era) where
  toJSON ValidatorInfo{..} =
    object
      [
        "address" .= serialiseAddress viAddress
      , "hash"    .= toJSON viHash
      , "script"  .= toJSON (serialiseToTextEnvelope Nothing viScript)
      , "size"    .= toJSON viSize
      , "cost"    .= toJSON viCost
      ]


-- | Information about Marlowe datum.
data DatumInfo =
  DatumInfo
  {
    diDatum :: Datum            -- ^ The datum.
  , diBytes :: ShortByteString  -- ^ The serialisation of the datum.
  , diJson  :: Value            -- ^ The JSON representation of the datum.
  , diHash  :: DatumHash        -- ^ The hash of the datum.
  , diSize  :: Int              -- ^ The size of the datum, in bytes.
  }
    deriving (Eq, Generic, Show)

instance ToJSON DatumInfo where
  toJSON DatumInfo{..} =
    object
      [
        "hash"    .= toJSON diHash
      , "cborHex" .= toJSON diBytes
      , "json"    .=        diJson
      , "size"    .= toJSON diSize
      ]


-- | Information about Marlowe redeemer.
data RedeemerInfo =
  RedeemerInfo
  {
    riRedeemer :: Redeemer         -- ^ The redeemer.
  , riBytes    :: ShortByteString  -- ^ The serialisation of the redeemer.
  , riJson     :: Value            -- ^ The JSON representation of the redeemer.
  , riSize     :: Int              -- ^ The size of the redeemer, in bytes.
  }
    deriving (Eq, Generic, Show)

instance ToJSON RedeemerInfo where
  toJSON RedeemerInfo{..} =
    object
      [
        "cboxHex" .= toJSON riBytes
      , "json"    .=        riJson
      , "size"    .= toJSON riSize
      ]


-- | Marlowe CLI commands and options.
data Command =
    -- | Export comprehensive Marlowe contrac and transactiont information.
    Export
    {
      network       :: Maybe NetworkId              -- ^ The network ID, if any.
    , stake         :: Maybe StakeAddressReference  -- ^ The stake address, if any.
    , rolesCurrency :: Maybe CurrencySymbol         -- ^ The role currency symbols, if any.
    , contractFile  :: FilePath                     -- ^ The JSON file containing the contract.
    , stateFile     :: FilePath                     -- ^ The JSON file containing the contract's state.
    , inputsFile    :: Maybe FilePath               -- ^ The JSON file containing the contract's input, if any.
    , minimumSlot   :: SlotNo                       -- ^ The first valid slot for the transaction.
    , maximumSlot   :: SlotNo                       -- ^ The last valid slot for the tranasction.
    , outputFile    :: FilePath                     -- ^ The output JSON file for Marlowe contract information.
    , printStats    :: Bool                         -- ^ Whether to print statistics about the contract and transaction.
    }
    -- | Export the address for a Marlowe contract.
  | ExportAddress
    {
      network       :: Maybe NetworkId              -- ^ The network ID, if any.
    , stake         :: Maybe StakeAddressReference  -- ^ The stake address, if any.
    , rolesCurrency :: Maybe CurrencySymbol         -- ^ The role currency symbols, if any.
    }
    -- | Export the validator for a Marlowe contract.
  | ExportValidator
    {
      network       :: Maybe NetworkId              -- ^ The network ID, if any.
    , stake         :: Maybe StakeAddressReference  -- ^ The stake address, if any.
    , rolesCurrency :: Maybe CurrencySymbol         -- ^ The role currency symbols, if any.
    , validatorFile :: FilePath                     -- ^ The output JSON file for the validator information.
    , printHash     :: Bool                         -- ^ Whether to print the validator hash.
    , printStats    :: Bool                         -- ^ Whether to print statistics about the contract.
    }
    -- | Export the datum for a Marlowe contract transaction.
  | ExportDatum
    {
      contractFile :: FilePath  -- ^ The JSON file containing the contract.
    , stateFile    :: FilePath  -- ^ The JSON file containing the contract's state.
    , datumFile    :: FilePath  -- ^ The output JSON file for the datum.
    , printStats   :: Bool      -- ^ Whether to print statistics about the datum.
    }
    -- | Export the redeemer for a Marlowe contract transaction.
  | ExportRedeemer
    {
      inputsFile   :: Maybe FilePath
    , minimumSlot  :: SlotNo          -- ^ The first valid slot for the transaction.
    , maximumSlot  :: SlotNo          -- ^ The last valid slot for the tranasction.
    , redeemerFile :: FilePath        -- ^ The output JSON file for the redeemer.
    , printStats   :: Bool            -- ^ Whether to print statistics about the redeemer.
    }
    -- | Ad-hoc example.
  | Example
    deriving (Eq, Generic, Show)
