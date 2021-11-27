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
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}


module Language.Marlowe.CLI.Types (
-- * Marlowe Transactions
  MarloweInfo(..)
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
, liftCli
, liftCliIO
-- * Marlowe CLI Commands
, Command(..)
) where


import           Cardano.Api                  (AddressAny, AddressInEra, AlonzoEra, AsType (..), Hash, IsCardanoEra,
                                               NetworkId, PaymentExtendedKey, PaymentKey, PlutusScript, PlutusScriptV1,
                                               PlutusScriptVersion (..), Script (..), ScriptData, SigningKey, SlotNo,
                                               StakeAddressReference, TxIn, VerificationKey, deserialiseAddress,
                                               deserialiseFromTextEnvelope, serialiseAddress, serialiseToTextEnvelope)
import           Cardano.Api.Shelley          (PlutusScript (..))
import           Codec.Serialise              (deserialise)
import           Control.Monad                ((<=<))
import           Control.Monad.Except         (MonadError, MonadIO, liftEither, liftIO)
import           Data.Aeson                   (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.=))
import           Data.Bifunctor               (first)
import           Data.ByteString.Short        (ShortByteString)
import           Data.String                  (IsString)
import           GHC.Generics                 (Generic)
import           Language.Marlowe.CLI.Orphans ()
import           Plutus.V1.Ledger.Api         (CurrencySymbol, Datum, DatumHash, ExBudget, Redeemer, ValidatorHash)

import qualified Cardano.Api                  as Api (Value)
import qualified Data.ByteString.Lazy         as LBS (fromStrict)
import qualified Data.ByteString.Short        as SBS (fromShort)


-- | Exception for Marlowe CLI.
newtype CliError = CliError {unCliError :: String}
  deriving (Eq, IsString, Ord, Read, Show)


-- | Lift an 'Either' result into the CLI.
liftCli :: MonadError CliError m
        => Show e
        => Either e a  -- ^ The result.
        -> m a         -- ^ The lifted result.
liftCli = liftEither . first (CliError . show)


-- | Lift an 'IO' 'Either' result into the CLI.
liftCliIO :: MonadError CliError m
          => MonadIO m
          => Show e
          => IO (Either e a)  -- ^ Action for the result.
          -> m a              -- ^ The lifted result.
liftCliIO = liftCli <=< liftIO


-- | A payment key.
type SomePaymentVerificationKey = Either (VerificationKey PaymentKey) (VerificationKey PaymentExtendedKey)


-- | A payment signing key.
type SomePaymentSigningKey = Either (SigningKey PaymentKey) (SigningKey PaymentExtendedKey)


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
  , datumHash :: Hash ScriptData   -- ^ The datum hash.
  }
    deriving (Eq, Generic, Show)


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
      inputsFile   :: Maybe FilePath  -- ^ The JSON file containing the contract's input, if any.
    , redeemerFile :: FilePath        -- ^ The output JSON file for the redeemer.
    , printStats   :: Bool            -- ^ Whether to print statistics about the redeemer.
    }
    -- | Build a non-Marlowe transaction.
  | BuildTransact
    {
      network    :: Maybe NetworkId            -- ^ The network ID, if any.
    , socketPath :: FilePath                   -- ^ The path to the node socket.
    , inputs     :: [TxIn]                     -- ^ The transaction inputs.
    , outputs    :: [(AddressAny, Api.Value)]  -- ^ The transaction outputs.
    , change     :: AddressAny                 -- ^ The change address.
    , bodyFile   :: FilePath                   -- ^ The output JSON file for the transaction body.
    }
    -- | Build a transaction paying into a Marlowe contract.
  | BuildCreate
    {
      network         :: Maybe NetworkId            -- ^ The network ID, if any.
    , socketPath      :: FilePath                   -- ^ The path to the node socket.
    , scriptAddress   :: AddressAny                 -- ^ The script address.
    , outputDatumFile :: FilePath                   -- ^ The file containing the datum for the payment to the script.
    , outputValue     :: Api.Value                  -- ^ The value to be paid to the script.
    , inputs          :: [TxIn]                     -- ^ The transaction inputs.
    , outputs         :: [(AddressAny, Api.Value)]  -- ^ The transaction outputs.
    , change          :: AddressAny                 -- ^ The change address.
    , bodyFile        :: FilePath                   -- ^ The output JSON file for the transaction body.
    }
    -- | Build a transaction that spends from and pays to a Marlowe contract.
  | BuildAdvance
    {
      network         :: Maybe NetworkId            -- ^ The network ID, if any.
    , socketPath      :: FilePath                   -- ^ The path to the node socket.
    , scriptAddress   :: AddressAny                 -- ^ The script address.
    , validatorFile   :: FilePath                   -- ^ The file containing the script validator.
    , redeemerFile    :: FilePath                   -- ^ The file containing the redeemer.
    , inputDatumFile  :: FilePath                   -- ^ The file containing the datum for spending from the script.
    , signingKeyFiles :: [FilePath]                 -- ^ The files containing the required signing keys.
    , inputTxIn       :: TxIn                       -- ^ The script eUTxO to be spent.
    , outputDatumFile :: FilePath                   -- ^ The file containing the datum for the payment to the script.
    , outputValue     :: Api.Value                  -- ^ The value to be paid to the script.
    , inputs          :: [TxIn]                     -- ^ The transaction inputs.
    , outputs         :: [(AddressAny, Api.Value)]  -- ^ The transaction outputs.
    , collateral      :: TxIn                       -- ^ The collateral.
    , change          :: AddressAny                 -- ^ The change address.
    , minimumSlot     :: SlotNo                     -- ^ The first valid slot for the transaction.
    , maximumSlot     :: SlotNo                     -- ^ The last valid slot for the tranasction.
    , bodyFile        :: FilePath                   -- ^ The output JSON file for the transaction body.
    }
    -- | Build a transaction spending from a Marlowe contract.
  | BuildClose
    {
      network         :: Maybe NetworkId            -- ^ The network ID, if any.
    , socketPath      :: FilePath                   -- ^ The path to the node socket.
    , validatorFile   :: FilePath                   -- ^ The file containing the script validator.
    , redeemerFile    :: FilePath                   -- ^ The file containing the redeemer.
    , inputDatumFile  :: FilePath                   -- ^ The file containing the datum for spending from the script.
    , signingKeyFiles :: [FilePath]                 -- ^ The files containing the required signing keys.
    , inputTxIn       :: TxIn                       -- ^ The script eUTxO to be spent.
    , inputs          :: [TxIn]                     -- ^ The transaction inputs.
    , outputs         :: [(AddressAny, Api.Value)]  -- ^ The transaction outputs.
    , collateral      :: TxIn                       -- ^ The collateral.
    , change          :: AddressAny                 -- ^ The change address.
    , minimumSlot     :: SlotNo                     -- ^ The first valid slot for the transaction.
    , maximumSlot     :: SlotNo                     -- ^ The last valid slot for the tranasction.
    , bodyFile        :: FilePath                   -- ^ The output JSON file for the transaction body.
    }
    -- | Submit a transaction.
  | Submit
    {
      network         :: Maybe NetworkId  -- ^ The network ID, if any.
    , socketPath      :: FilePath         -- ^ The path to the node socket.
    , bodyFile        :: FilePath         -- ^ The JSON file containing the transaction body.
    , signingKeyFiles :: [FilePath]       -- ^ The signing key files.
    }
    -- | Ad-hoc example.
  | Example
    {
      writeFiles :: Bool  -- ^ Whether to serialise states, contracts and inputs to JSON.
    }
    deriving (Eq, Generic, Show)
