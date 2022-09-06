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


{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}


module Language.Marlowe.CLI.Types (
-- * Marlowe Transactions
  MarloweTransaction(..)
, MarlowePlutusVersion
, MarloweInfo(..)
, ValidatorInfo(..)
, DatumInfo(..)
, RedeemerInfo(..)
, SomeMarloweTransaction(..)
, CliEnv(..)
-- * eUTxOs
, PayFromScript(..)
, PayToScript(..)
-- * Keys
, SomePaymentVerificationKey
, SomePaymentSigningKey
-- * Exceptions
, CliError(..)
-- * Queries
, OutputQuery(..)
, OutputQueryResult(..)
-- * Merklization
, Continuations
, Publisher(..)
, PublishScript(..)
-- * Newtype wrappers
, PrintStats(..)
, PublishingStrategy(..)
, TxBodyFile(..)
, SigningKeyFile(..)
-- * pattern matching boilerplate
, withCardanoEra
, withShelleyBasedEra
, toAsType
, toCardanoEra
, toEraInMode
, toShelleyBasedEra
, toPlutusScriptV1LanguageInEra
, toPlutusScriptV2LanguageInEra
, toSimpleScriptV2LanguageInEra
, toTxMetadataSupportedInEra
, toMultiAssetSupportedInEra
, toTxScriptValiditySupportedInEra
, toCollateralSupportedInEra
, toTxFeesExplicitInEra
, toValidityLowerBoundSupportedInEra
, toValidityUpperBoundSupportedInEra
, toValidityNoUpperBoundSupportedInEra
, toExtraKeyWitnessesSupportedInEra
, askEra
, asksEra
, doWithCardanoEra
, doWithShelleyBasedEra
, publisherAddress
, toAddressAny'
, toPaymentVerificationKey
, getVerificationKey
, AnyTimeout(..)
, toTimeout
) where


import Cardano.Api (AddressAny, AddressInEra (AddressInEra), AsType (..), AssetId, CardanoEra (AlonzoEra, BabbageEra),
                    CardanoMode, CollateralSupportedInEra (..), EraInMode (..), HasTypeProxy (proxyToAsType), Hash,
                    IsCardanoEra, IsScriptLanguage, IsShelleyBasedEra, Lovelace, PaymentExtendedKey, PaymentKey,
                    PlutusScript, PlutusScriptV1, PlutusScriptV2, PlutusScriptVersion (PlutusScriptV1, PlutusScriptV2),
                    Script (PlutusScript), ScriptData, ScriptDataSupportedInEra (..), ScriptLanguageInEra (..),
                    ShelleyBasedEra (..), SigningKey, SimpleScriptV2, SlotNo, TxExtraKeyWitnessesSupportedInEra (..),
                    TxFeesExplicitInEra (..), TxIn, TxMetadataSupportedInEra (..), TxScriptValiditySupportedInEra (..),
                    ValidityLowerBoundSupportedInEra (..), ValidityNoUpperBoundSupportedInEra (..),
                    ValidityUpperBoundSupportedInEra (..), VerificationKey, castVerificationKey, deserialiseAddress,
                    deserialiseFromTextEnvelope, serialiseAddress, serialiseToTextEnvelope, toAddressAny)
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (deserialise)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.=))
import Data.ByteString.Short (ShortByteString)
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import GHC.Generics (Generic)
import Language.Marlowe.CLI.Orphans ()
import Language.Marlowe.Core.V1.Semantics (Payment)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Input, State)
import Ledger.Orphans ()
import Plutus.V1.Ledger.Api (CurrencySymbol, Datum, DatumHash, ExBudget, Redeemer, ValidatorHash)
import Plutus.V1.Ledger.SlotConfig (SlotConfig)

import qualified Cardano.Api as Api (Value)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (MonadReader (..), asks)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS (fromStrict)
import qualified Data.ByteString.Short as SBS (fromShort)
import qualified Data.Map.Strict as M (Map)
import Data.Proxy (Proxy (Proxy))
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Exts (IsString (fromString))
import Language.Marlowe.CLI.Cardano.Api (toMultiAssetSupportedInEra, withShelleyBasedEra)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (IsPlutusScriptLanguage, plutusScriptVersion,
                                                      withPlutusScriptVersion)
import Language.Marlowe.Extended.V1 (Timeout (POSIXTime))
import qualified Language.Marlowe.Extended.V1 as E


-- | Exception for Marlowe CLI.
newtype CliError = CliError {unCliError :: String}
  deriving (Eq, Ord, Read, Show)

instance IsString CliError where
  fromString = CliError

-- | A payment key.
type SomePaymentVerificationKey = Either (VerificationKey PaymentKey) (VerificationKey PaymentExtendedKey)

toPaymentVerificationKey :: SomePaymentVerificationKey -> VerificationKey PaymentKey
toPaymentVerificationKey (Left vkey)  = vkey
toPaymentVerificationKey (Right vkey) = castVerificationKey vkey


-- | A payment signing key.
type SomePaymentSigningKey = Either (SigningKey PaymentKey) (SigningKey PaymentExtendedKey)

getVerificationKey :: SomePaymentSigningKey -> SomePaymentVerificationKey
getVerificationKey (Left skey)  = Left $ C.getVerificationKey skey
getVerificationKey (Right skey) = Right $ C.getVerificationKey skey

-- | Continuations for contracts.
type Continuations = M.Map DatumHash Contract


-- | A marlowe transaction in an existentially quantified era
data SomeMarloweTransaction = forall era lang. IsPlutusScriptLanguage lang => SomeMarloweTransaction
  (PlutusScriptVersion lang)
  (ScriptDataSupportedInEra era)
  (MarloweTransaction lang era)


-- | Plutus version which we use in the current Marlowe script.
type MarlowePlutusVersion = PlutusScriptV2


doWithCardanoEra :: forall era m a. MonadReader (CliEnv era) m => (IsCardanoEra era => m a) -> m a
doWithCardanoEra m = askEra >>= \era -> withCardanoEra era m

doWithShelleyBasedEra :: forall era m a. MonadReader (CliEnv era) m => (IsShelleyBasedEra era => m a) -> m a
doWithShelleyBasedEra m = askEra >>= \era -> withShelleyBasedEra era m

-- TODO: Move this set of functions to `Marlowe.CLI.Cardano.Api`
withCardanoEra :: forall era a. ScriptDataSupportedInEra era -> (IsCardanoEra era => a) -> a
withCardanoEra = \case
  ScriptDataInAlonzoEra  -> id
  ScriptDataInBabbageEra -> id

toAsType :: ScriptDataSupportedInEra era -> AsType era
toAsType = \case
  ScriptDataInAlonzoEra  -> AsAlonzoEra
  ScriptDataInBabbageEra -> AsBabbageEra

toCardanoEra :: ScriptDataSupportedInEra era -> CardanoEra era
toCardanoEra = \case
  ScriptDataInAlonzoEra  -> AlonzoEra
  ScriptDataInBabbageEra -> BabbageEra

toEraInMode :: ScriptDataSupportedInEra era -> EraInMode era CardanoMode
toEraInMode = \case
  ScriptDataInAlonzoEra  -> AlonzoEraInCardanoMode
  ScriptDataInBabbageEra -> BabbageEraInCardanoMode

toShelleyBasedEra :: ScriptDataSupportedInEra era -> ShelleyBasedEra era
toShelleyBasedEra = \case
  ScriptDataInAlonzoEra  -> ShelleyBasedEraAlonzo
  ScriptDataInBabbageEra -> ShelleyBasedEraBabbage

toPlutusScriptV1LanguageInEra :: ScriptDataSupportedInEra era -> ScriptLanguageInEra PlutusScriptV1 era
toPlutusScriptV1LanguageInEra = \case
  ScriptDataInAlonzoEra  -> PlutusScriptV1InAlonzo
  ScriptDataInBabbageEra -> PlutusScriptV1InBabbage

toPlutusScriptV2LanguageInEra :: ScriptDataSupportedInEra era -> Maybe (ScriptLanguageInEra PlutusScriptV2 era)
toPlutusScriptV2LanguageInEra = \case
  ScriptDataInAlonzoEra  -> Nothing
  ScriptDataInBabbageEra -> Just PlutusScriptV2InBabbage

toSimpleScriptV2LanguageInEra :: ScriptDataSupportedInEra era -> ScriptLanguageInEra SimpleScriptV2 era
toSimpleScriptV2LanguageInEra = \case
  ScriptDataInAlonzoEra  -> SimpleScriptV2InAlonzo
  ScriptDataInBabbageEra -> SimpleScriptV2InBabbage

toTxMetadataSupportedInEra :: ScriptDataSupportedInEra era -> TxMetadataSupportedInEra era
toTxMetadataSupportedInEra = \case
  ScriptDataInAlonzoEra  -> TxMetadataInAlonzoEra
  ScriptDataInBabbageEra -> TxMetadataInBabbageEra

toTxScriptValiditySupportedInEra :: ScriptDataSupportedInEra era -> TxScriptValiditySupportedInEra era
toTxScriptValiditySupportedInEra = \case
  ScriptDataInAlonzoEra  -> TxScriptValiditySupportedInAlonzoEra
  ScriptDataInBabbageEra -> TxScriptValiditySupportedInBabbageEra

toCollateralSupportedInEra :: ScriptDataSupportedInEra era -> CollateralSupportedInEra era
toCollateralSupportedInEra = \case
  ScriptDataInAlonzoEra  -> CollateralInAlonzoEra
  ScriptDataInBabbageEra -> CollateralInBabbageEra

toTxFeesExplicitInEra :: ScriptDataSupportedInEra era -> TxFeesExplicitInEra era
toTxFeesExplicitInEra = \case
  ScriptDataInAlonzoEra  -> TxFeesExplicitInAlonzoEra
  ScriptDataInBabbageEra -> TxFeesExplicitInBabbageEra

toValidityLowerBoundSupportedInEra :: ScriptDataSupportedInEra era -> ValidityLowerBoundSupportedInEra era
toValidityLowerBoundSupportedInEra = \case
  ScriptDataInAlonzoEra  -> ValidityLowerBoundInAlonzoEra
  ScriptDataInBabbageEra -> ValidityLowerBoundInBabbageEra

toValidityUpperBoundSupportedInEra :: ScriptDataSupportedInEra era -> ValidityUpperBoundSupportedInEra era
toValidityUpperBoundSupportedInEra = \case
  ScriptDataInAlonzoEra  -> ValidityUpperBoundInAlonzoEra
  ScriptDataInBabbageEra -> ValidityUpperBoundInBabbageEra

toValidityNoUpperBoundSupportedInEra :: ScriptDataSupportedInEra era -> ValidityNoUpperBoundSupportedInEra era
toValidityNoUpperBoundSupportedInEra = \case
  ScriptDataInAlonzoEra  -> ValidityNoUpperBoundInAlonzoEra
  ScriptDataInBabbageEra -> ValidityNoUpperBoundInBabbageEra

toExtraKeyWitnessesSupportedInEra :: ScriptDataSupportedInEra era -> TxExtraKeyWitnessesSupportedInEra era
toExtraKeyWitnessesSupportedInEra = \case
  ScriptDataInAlonzoEra  -> ExtraKeyWitnessesInAlonzoEra
  ScriptDataInBabbageEra -> ExtraKeyWitnessesInBabbageEra

toAddressAny' :: AddressInEra era -> AddressAny
toAddressAny' (AddressInEra _ addr) = toAddressAny addr

newtype CliEnv era = CliEnv { era :: ScriptDataSupportedInEra era }

askEra :: MonadReader (CliEnv era) m => m (ScriptDataSupportedInEra era)
askEra = asks era

asksEra :: MonadReader (CliEnv era) m => (ScriptDataSupportedInEra era -> a) -> m a
asksEra f = f <$> askEra

instance ToJSON SomeMarloweTransaction where
  toJSON (SomeMarloweTransaction plutusVersion era tx) = withShelleyBasedEra era $ object
    let
      eraStr :: String
      eraStr = case era of
        ScriptDataInAlonzoEra  -> "alonzo"
        ScriptDataInBabbageEra -> "babbage"
      plutusVersionStr :: String
      plutusVersionStr = case plutusVersion of
        PlutusScriptV1 -> "PlutusScriptV1"
        PlutusScriptV2 -> "PlutusScriptV2"
    in
      [ "era" .= eraStr
      , "plutusVersion" .= plutusVersionStr
      , "tx" .= withPlutusScriptVersion plutusVersion (toJSON tx)
      ]

instance FromJSON SomeMarloweTransaction where
  parseJSON = withObject "SomeTransaction" $ \obj -> do
    eraStr :: String <- obj .: "era"
    plutusVersionStr :: String <- obj .: "plutusVersion"
    case (eraStr, plutusVersionStr) of
      ("alonzo", "PlutusScriptV1")  -> SomeMarloweTransaction PlutusScriptV1 ScriptDataInAlonzoEra <$> obj .: "tx"
      ("alonzo", "PlutusScriptV2")  -> SomeMarloweTransaction PlutusScriptV2 ScriptDataInAlonzoEra <$> obj .: "tx"
      ("babbage", "PlutusScriptV1") -> SomeMarloweTransaction PlutusScriptV1 ScriptDataInBabbageEra <$> obj .: "tx"
      ("babbage", "PlutusScriptV2") -> SomeMarloweTransaction PlutusScriptV2 ScriptDataInBabbageEra <$> obj .: "tx"
      _                             -> fail $ "Unsupported era " <> show eraStr


-- | Complete description of a Marlowe transaction.
data MarloweTransaction lang era =
  MarloweTransaction
  {
    mtValidator     :: ValidatorInfo lang era       -- ^ The Marlowe validator.
  , mtRoleValidator :: ValidatorInfo lang era       -- ^ The roles validator.
  , mtRolesCurrency :: CurrencySymbol               -- ^ The roles currency.
  , mtState         :: State                        -- ^ The Marlowe state after the transaction.
  , mtContract      :: Contract                     -- ^ The Marlowe contract after the transaction.
  , mtContinuations :: Continuations                -- ^ The merkleized continuations for the contract.
  , mtRange         :: Maybe (SlotNo, SlotNo)       -- ^ The slot range for the transaction, if any.
  , mtInputs        :: [Input]                      -- ^ The inputs to the transaction.
  , mtPayments      :: [Payment]                    -- ^ The payments from the transaction.
  , mtSlotConfig    :: SlotConfig                   -- ^ The POSIXTime-to-Slot configuration.
  }
    deriving (Eq, Generic, Show)

instance (IsPlutusScriptLanguage lang, IsShelleyBasedEra era) => ToJSON (MarloweTransaction lang era) where
  toJSON MarloweTransaction{..} =
    object
      [
        "marloweValidator" .= toJSON mtValidator
      , "rolesValidator"   .= toJSON mtRoleValidator
      , "roles"            .= toJSON mtRolesCurrency
      , "state"            .= toJSON mtState
      , "contract"         .= toJSON mtContract
      , "continuations"    .= toJSON mtContinuations
      , "range"            .= toJSON mtRange
      , "inputs"           .= toJSON mtInputs
      , "payments"         .= toJSON mtPayments
      , "slotConfig"       .= toJSON mtSlotConfig
      ]

instance (IsScriptLanguage lang, IsShelleyBasedEra era) => FromJSON (MarloweTransaction lang era) where
  parseJSON =
    withObject "MarloweTransaction"
      $ \o ->
        do
          mtValidator     <- o .: "marloweValidator"
          mtRoleValidator <- o .: "rolesValidator"
          mtRolesCurrency <- o .: "roles"
          mtState         <- o .: "state"
          mtContract      <- o .: "contract"
          mtContinuations <- fromMaybe mempty <$> (o .:? "continuations")
          mtRange         <- o .: "range"
          mtInputs        <- o .: "inputs"
          mtPayments      <- o .: "payments"
          mtSlotConfig    <- o .: "slotConfig"
          pure MarloweTransaction{..}


-- | Comprehensive information about a Marlowe transaction.
data MarloweInfo lang era =
  MarloweInfo
  {
    validatorInfo :: ValidatorInfo lang era  -- ^ Validator information.
  , datumInfo     :: DatumInfo               -- ^ Datum information.
  , redeemerInfo  :: RedeemerInfo            -- ^ Redeemer information.
  }
    deriving (Eq, Generic, Show)

instance (IsPlutusScriptLanguage lang, IsShelleyBasedEra era) => ToJSON (MarloweInfo lang era) where
  toJSON MarloweInfo{..} =
    object
      [
        "validator" .= toJSON validatorInfo
      , "datum"     .= toJSON datumInfo
      , "redeemer"  .= toJSON redeemerInfo
      ]

instance (IsScriptLanguage lang, IsShelleyBasedEra era) => FromJSON (MarloweInfo lang era) where
  parseJSON =
    withObject "MarloweInfo"
      $ \o ->
        do
          validatorInfo <- o .: "validator"
          datumInfo     <- o .: "datum"
          redeemerInfo  <- o .: "redeemer"
          pure MarloweInfo{..}


-- | Information about Marlowe validator.
data ValidatorInfo lang era =
  ValidatorInfo
  {
    -- TODO: We probably want to introduce `AnyPlutusScript` (`Cardano.Api` provides only `ScriptInAnyLang`) so
    -- we can hide the `lang` behind it.
    viScript  :: PlutusScript lang           -- ^ The Plutus script.
  , viBytes   :: ShortByteString             -- ^ The serialisation of the validator.
  , viHash    :: ValidatorHash               -- ^ The validator hash.
  , viAddress :: AddressInEra era            -- ^ The script address.
  , viSize    :: Int                         -- ^ The script size, in bytes.
  , viCost    :: ExBudget                    -- ^ The execution budget for the script.
  }
    deriving (Eq, Generic, Show)

instance (IsPlutusScriptLanguage lang, IsShelleyBasedEra era) => ToJSON (ValidatorInfo lang era) where
  toJSON ValidatorInfo{..} = do

    object
      [
        "address" .= serialiseAddress viAddress
      , "hash"    .= toJSON viHash
      , "script"  .= toJSON (serialiseToTextEnvelope Nothing (PlutusScript (plutusScriptVersion :: PlutusScriptVersion lang) viScript))
      , "size"    .= toJSON viSize
      , "cost"    .= toJSON viCost
      ]

instance (HasTypeProxy lang, IsScriptLanguage lang, IsShelleyBasedEra era) => FromJSON (ValidatorInfo lang era) where
  parseJSON =
    withObject "ValidatorInfo"
      $ \o ->
        do
          address   <- o .: "address"
          viHash    <- o .: "hash"
          script    <- o .: "script"
          viSize    <- o .: "size"
          viCost    <- o .: "cost"
          viAddress <- case deserialiseAddress (proxyToAsType (Proxy :: Proxy (AddressInEra era))) address of
                         Just address' -> pure address'
                         Nothing       -> fail "Failed deserialising address."

          anyScript <- case deserialiseFromTextEnvelope (proxyToAsType (Proxy :: Proxy (Script lang))) script of
                         Right script' -> pure script'
                         Left message  -> fail $ show message
          (viScript, viBytes) <- case anyScript of
            PlutusScript _ plutusScript@(PlutusScriptSerialised viBytes) -> pure (plutusScript, viBytes)
            _                                                            -> fail "Expecting plutus script."
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
data PayFromScript lang =
  PayFromScript
  {
    txIn     :: TxIn                         -- ^ The eUTxO to be spent.
  , script   :: PlutusScript lang            -- ^ The script.
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


-- | Options for address queries.
data OutputQuery =
    -- | Match all UTxOs.
    AllOutput
    -- | Match pure-ADA UTxOs which pass the check.
  | LovelaceOnly
    {
      amountCheck :: Lovelace -> Bool
    }
    -- | Only require UTxOs containing only the specified asset.
  | AssetOnly
    {
      asset :: AssetId
    }


data OutputQueryResult era = OutputQueryResult
  {
    oqrMatching    :: C.UTxO era
  , oqrNonMatching :: C.UTxO era
  }


data AnyTimeout = AbsoluteTimeout Integer | RelativeTimeout NominalDiffTime
    deriving stock (Eq, Generic, Show)


instance ToJSON AnyTimeout where
  toJSON (AbsoluteTimeout timeout)  = Aeson.object [("absolute", toJSON timeout)]
  toJSON (RelativeTimeout duration) = Aeson.object [("relative", toJSON duration)]


instance FromJSON AnyTimeout where
  parseJSON json = case json of
    Aeson.Object (KeyMap.toList -> [("absolute", absoluteTimeout)]) -> do
      parsedTimeout <- parseJSON absoluteTimeout
      pure $ AbsoluteTimeout parsedTimeout
    Aeson.Object (KeyMap.toList -> [("relative", duration)]) -> do
      parsedDuration <- parseJSON duration
      pure $ RelativeTimeout parsedDuration
    _ -> fail "Expected object with a single field of either `absolute` or `relative`"


toTimeout :: MonadIO m => AnyTimeout -> m E.Timeout
toTimeout (AbsoluteTimeout t)       = pure $ POSIXTime t
toTimeout (RelativeTimeout seconds) = do
  let
    toPOSIXMilliseconds = E.POSIXTime . floor . (1e6 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
  liftIO $ toPOSIXMilliseconds . addUTCTime seconds <$> getCurrentTime


data PublishingStrategy era =
    PublishPermanently C.StakeAddressReference
  | PublishAtAddress (AddressInEra era)


data Publisher era =
    DesignatedPublisher (AddressInEra era)
  | UnspendableValidator
      (ValidatorInfo PlutusScriptV2 era)


publisherAddress :: Publisher era -> AddressInEra era
publisherAddress (DesignatedPublisher addr) = addr
publisherAddress (UnspendableValidator v)   = viAddress v


-- | Information required to publish a script
data PublishScript lang era =
  PublishScript
  {
    psMinAda             :: Lovelace
  , psPublisher          :: Publisher era
  , psReferenceScript    :: CS.ReferenceScript era
  , psReferenceValidator :: ValidatorInfo lang era
  }

newtype PrintStats = PrintStats { unPrintStats :: Bool }

newtype TxBodyFile = TxBodyFile { unTxBodyFile :: FilePath }

newtype SigningKeyFile = SigningKeyFile { unSigningKeyFile :: FilePath }

