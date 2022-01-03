-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Types for the Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}


module Language.Marlowe.CLI.Types (
-- * Marlowe Transactions
  MarloweTransaction(..)
, MarloweInfo(..)
, ValidatorInfo(..)
, DatumInfo(..)
, RedeemerInfo(..)
-- * eUTxOs
, PayFromScript(..)
, PayToScript(..)
-- * Keys
, SomePaymentVerificationKey
, SomePaymentSigningKey
-- * Exceptions
, CliError(..)
) where


import           Cardano.Api                     (AddressInEra, AlonzoEra, AsType (..), Hash, IsCardanoEra,
                                                  PaymentExtendedKey, PaymentKey, PlutusScript, PlutusScriptV1,
                                                  PlutusScriptVersion (..), Script (..), ScriptData, SigningKey, SlotNo,
                                                  TxIn, VerificationKey, deserialiseAddress,
                                                  deserialiseFromTextEnvelope, serialiseAddress,
                                                  serialiseToTextEnvelope)
import           Cardano.Api.Shelley             (PlutusScript (..))
import           Codec.Serialise                 (deserialise)
import           Data.Aeson                      (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.=))
import           Data.ByteString.Short           (ShortByteString)
import           Data.String                     (IsString)
import           GHC.Generics                    (Generic)
import           Language.Marlowe.CLI.Orphans    ()
import           Language.Marlowe.Semantics      (Payment)
import           Language.Marlowe.SemanticsTypes (Contract, Input, State)
import           Plutus.V1.Ledger.Api            (CurrencySymbol, Datum, DatumHash, ExBudget, Redeemer, ValidatorHash)

import qualified Cardano.Api                     as Api (Value)
import qualified Data.ByteString.Lazy            as LBS (fromStrict)
import qualified Data.ByteString.Short           as SBS (fromShort)


-- | Exception for Marlowe CLI.
newtype CliError = CliError {unCliError :: String}
  deriving (Eq, IsString, Ord, Read, Show)


-- | A payment key.
type SomePaymentVerificationKey = Either (VerificationKey PaymentKey) (VerificationKey PaymentExtendedKey)


-- | A payment signing key.
type SomePaymentSigningKey = Either (SigningKey PaymentKey) (SigningKey PaymentExtendedKey)


-- | Complete description of a Marlowe transaction.
data MarloweTransaction era =
  MarloweTransaction
  {
    mtValidator     :: ValidatorInfo era       -- ^ The Marlowe validator.
  , mtRoleValidator :: ValidatorInfo era       -- ^ The roles validator.
  , mtRoles         :: CurrencySymbol          -- ^ The roles currency.
  , mtState         :: State                   -- ^ The Marlowe state after the transaction.
  , mtContract      :: Contract                -- ^ The Marlowe contract after the transaction.
  , mtRange         :: Maybe (SlotNo, SlotNo)  -- ^ The slot range for the transaction, if any.
  , mtInputs        :: [Input]                 -- ^ The inputs to the transaction.
  , mtPayments      :: [Payment]               -- ^ The payments from the transaction.
  }
    deriving (Generic, Show)

instance IsCardanoEra era => ToJSON (MarloweTransaction era) where
  toJSON MarloweTransaction{..} =
    object
      [
        "marloweValidator" .= toJSON mtValidator
      , "rolesValidator"   .= toJSON mtRoleValidator
      , "roles"            .= toJSON mtRoles
      , "state"            .= toJSON mtState
      , "contract"         .= toJSON mtContract
      , "range"            .= toJSON mtRange
      , "inputs"           .= toJSON mtInputs
      , "payments"         .= toJSON mtPayments
      ]

instance FromJSON (MarloweTransaction AlonzoEra) where  -- FIXME: Generalize eras.
  parseJSON =
    withObject "MarloweTransaction"
      $ \o ->
        do
          mtValidator     <- o .: "marloweValidator"
          mtRoleValidator <- o .: "rolesValidator"
          mtRoles         <- o .: "roles"
          mtState         <- o .: "state"
          mtContract      <- o .: "contract"
          mtRange         <- o .: "range"
          mtInputs        <- o .: "inputs"
          mtPayments      <- o .: "payments"
          pure MarloweTransaction{..}


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

instance FromJSON (MarloweInfo AlonzoEra) where  -- FIXME: Generalize eras.
  parseJSON =
    withObject "MarloweInfo"
      $ \o ->
        do
          validatorInfo <- o .: "validator"
          datumInfo     <- o .: "datum"
          redeemerInfo  <- o .: "redeemer"
          pure MarloweInfo{..}


-- | Information about Marlowe validator.
data ValidatorInfo era =
  ValidatorInfo
  {
    viScript  :: Script PlutusScriptV1       -- ^ The Plutus script.
  , viBytes   :: ShortByteString             -- ^ The serialisation of the validator.
  , viHash    :: ValidatorHash               -- ^ The validator hash.
  , viAddress :: AddressInEra era            -- ^ The script address.
  , viSize    :: Int                         -- ^ The script size, in bytes.
  , viCost    :: ExBudget                    -- ^ The execution budget for the script.
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

instance FromJSON (ValidatorInfo AlonzoEra) where  -- FIXME: Generalize eras.
  parseJSON =
    withObject "ValidatorInfo"
      $ \o ->
        do
          address   <- o .: "address"
          viHash    <- o .: "hash"
          script    <- o .: "script"
          viSize    <- o .: "size"
          viCost    <- o .: "cost"
          viAddress <- case deserialiseAddress (AsAddressInEra AsAlonzoEra) address of
                         Just address' -> pure address'
                         Nothing       -> fail "Failed deserialising address."
          viScript <- case deserialiseFromTextEnvelope (AsScript AsPlutusScriptV1) script of
                         Right script' -> pure script'
                         Left message  -> fail $ show message
          let
            PlutusScript PlutusScriptV1 (PlutusScriptSerialised viBytes) = viScript
          pure ValidatorInfo{..}


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

instance FromJSON DatumInfo where
  parseJSON =
    withObject "DatumInfo"
      $ \o ->
        do
          diHash  <- o .: "hash"
          diBytes <- o .: "cboxHex"
          diJson  <- o .: "json"
          diSize  <- o .: "size"
          let
            diDatum = deserialise . LBS.fromStrict $ SBS.fromShort diBytes
          pure DatumInfo{..}


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

instance FromJSON RedeemerInfo where
  parseJSON =
    withObject "RedeemerInfo"
      $ \o ->
        do
          riBytes <- o .: "cboxHex"
          riJson  <- o .: "json"
          riSize  <- o .: "size"
          let
            riRedeemer = deserialise . LBS.fromStrict $ SBS.fromShort riBytes
          pure RedeemerInfo{..}


-- | Information required to spend from a script.
data PayFromScript =
  PayFromScript
  {
    txIn     :: TxIn                         -- ^ The eUTxO to be spent.
  , script   :: PlutusScript PlutusScriptV1  -- ^ The script.
  , datum    :: Datum                        -- ^ The datum.
  , redeemer :: Redeemer                     -- ^ The redeemer.
  }
    deriving (Eq, Generic, Show)


-- | Information required to pay to a script.
data PayToScript era =
  PayToScript
  {
    address   :: AddressInEra era  -- ^ The script address.
  , value     :: Api.Value         -- ^ The value to be paid.
  , datumOut  :: ScriptData        -- ^ The datum.
  , datumHash :: Hash ScriptData   -- ^ The datum hash.
  }
    deriving (Eq, Generic, Show)
