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


{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}


module Language.Marlowe.CLI.Types
  ( -- * Marlowe Transactions
    CliEnv(..)
  , CoinSelectionStrategy(..)
  , DatumInfo(..)
  , MarloweInfo(..)
  , MarlowePlutusVersion
  , MarloweTransaction(..)
  , RedeemerInfo(..)
  , SomeMarloweTransaction(..)
  , SomeTimeout(..)
  , SubmitMode(..)
  , ValidatorInfo(..)
    -- * eUTxOs
  , AnUTxO(..)
  , PayFromScript(..)
  , PayToScript(..)
    -- * Minting
  , CurrencyIssuer(..)
  , MintingAction(..)
    -- * Keys
  , SomePaymentSigningKey
  , SomePaymentVerificationKey
    -- * Exceptions
  , CliError(..)
    -- * Queries
  , OutputQuery(..)
  , OutputQueryResult(..)
    -- * Merklization
  , Continuations
    -- * Publishing
  , MarloweScriptsRefs(..)
  , PublishMarloweScripts(..)
  , PublishScript(..)
  , PublishingStrategy(..)
    -- * Newtype wrappers
  , PrintStats(..)
  , SigningKeyFile(..)
  , TxBodyFile(..)
    -- * constants
  , marlowePlutusVersion
    -- * pattern matching boilerplate
  , askEra
  , asksEra
  , doWithCardanoEra
  , doWithShelleyBasedEra
  , somePaymentsigningKeyToTxWitness
  , toAddressAny'
  , toAsType
  , toCardanoEra
  , toCollateralSupportedInEra
  , toEraInMode
  , toExtraKeyWitnessesSupportedInEra
  , toMultiAssetSupportedInEra
  , toShelleyAddress
  , toShelleyBasedEra
  , toSimpleScriptV2LanguageInEra
  , toTxFeesExplicitInEra
  , toTxMetadataSupportedInEra
  , toTxScriptValiditySupportedInEra
  , toValidityLowerBoundSupportedInEra
  , toValidityNoUpperBoundSupportedInEra
  , toValidityUpperBoundSupportedInEra
  , withCardanoEra
  , withShelleyBasedEra
    -- * accessors and converters
  , anUTxOValue
  , getVerificationKey
  , submitModeFromTimeout
  , toMarloweTimeout
  , toPOSIXTime
  , toPaymentVerificationKey
  , toSlotRoundedMarloweTimeout
  , toUTxO
  , validatorInfoScriptOrReference
    -- * constructors and defaults
  , defaultCoinSelectionStrategy
  , fromUTxO
  , validatorInfo
  , validatorInfo'
  ) where


import Cardano.Api
  ( AddressAny
  , AddressInEra(AddressInEra)
  , AsType(..)
  , AssetId
  , CardanoEra(AlonzoEra, BabbageEra)
  , CardanoMode
  , CollateralSupportedInEra(..)
  , EraInMode(..)
  , HasTypeProxy(proxyToAsType)
  , Hash
  , IsCardanoEra
  , IsScriptLanguage
  , IsShelleyBasedEra
  , Lovelace
  , PaymentExtendedKey
  , PaymentKey
  , PlutusScript
  , PlutusScriptVersion
  , Script(PlutusScript)
  , ScriptData
  , ScriptDataSupportedInEra(..)
  , ScriptLanguageInEra(..)
  , ShelleyBasedEra(..)
  , SigningKey
  , SimpleScriptV2
  , SlotNo
  , TxExtraKeyWitnessesSupportedInEra(..)
  , TxFeesExplicitInEra(..)
  , TxIn
  , TxMetadataSupportedInEra(..)
  , TxScriptValiditySupportedInEra(..)
  , ValidityLowerBoundSupportedInEra(..)
  , ValidityNoUpperBoundSupportedInEra(..)
  , ValidityUpperBoundSupportedInEra(..)
  , VerificationKey
  , castVerificationKey
  , deserialiseAddress
  , deserialiseFromTextEnvelope
  , serialiseAddress
  , serialiseToTextEnvelope
  , toAddressAny
  )
import Cardano.Api.Shelley (PlutusScript(..))
import Codec.Serialise (deserialise)
import Data.Aeson (FromJSON(..), ToJSON(..), Value, object, withObject, (.:), (.:?), (.=))
import Data.ByteString.Short (ShortByteString)
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import GHC.Generics (Generic)
import Language.Marlowe.CLI.Orphans ()
import Language.Marlowe.Core.V1.Semantics (Payment)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Input, State)
import Ledger.Orphans ()
import Plutus.V1.Ledger.Api (CurrencySymbol, Datum, DatumHash, ExBudget, Redeemer)
import qualified Plutus.V1.Ledger.Api as P
import Plutus.V1.Ledger.SlotConfig (SlotConfig, posixTimeToEnclosingSlot, slotToBeginPOSIXTime)

import qualified Cardano.Api as Api (Value)
import qualified Cardano.Api as C
import qualified Cardano.Api.Byron as CB
import qualified Cardano.Api.Shelley as C
import qualified Cardano.Api.Shelley as CS
import Control.Exception (Exception)
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader.Class (MonadReader(..), asks)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Lazy as LBS (fromStrict)
import qualified Data.ByteString.Short as SBS (fromShort, length)
import qualified Data.List.NonEmpty as L
import qualified Data.Map.Strict as M (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(Proxy))
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Units (Second)
import GHC.Exts (IsString(fromString))
import GHC.Natural (Natural)
import Language.Marlowe.CLI.Cardano.Api (toMultiAssetSupportedInEra, withShelleyBasedEra)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript
  (IsPlutusScriptLanguage, plutusScriptVersion, withPlutusScriptVersion)
import Language.Marlowe.Extended.V1 (Timeout(POSIXTime))
import qualified Language.Marlowe.Extended.V1 as E


-- | Exception for Marlowe CLI.
newtype CliError = CliError {unCliError :: String}
  deriving (Eq, Ord, Read, Show)

instance IsString CliError where
  fromString = CliError

instance Exception CliError

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

somePaymentsigningKeyToTxWitness :: SomePaymentSigningKey -> CS.ShelleyWitnessSigningKey
somePaymentsigningKeyToTxWitness = either C.WitnessPaymentKey C.WitnessPaymentExtendedKey

-- | Continuations for contracts.
type Continuations = M.Map DatumHash Contract


-- | A marlowe transaction in an existentially quantified era
data SomeMarloweTransaction = forall era lang. IsPlutusScriptLanguage lang => SomeMarloweTransaction
  (PlutusScriptVersion lang)
  (ScriptDataSupportedInEra era)
  (MarloweTransaction lang era)


-- | Plutus version which we use in the current Marlowe script.
type MarlowePlutusVersion = C.PlutusScriptV2

marlowePlutusVersion :: PlutusScriptVersion MarlowePlutusVersion
marlowePlutusVersion = plutusScriptVersion


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

toShelleyAddress :: AddressInEra era -> Maybe (C.Address C.ShelleyAddr)
toShelleyAddress (AddressInEra _ addr) = case addr of
  CB.ByronAddress _      -> Nothing
  s@CS.ShelleyAddress {} -> Just s

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
        C.PlutusScriptV1 -> "PlutusScriptV1"
        C.PlutusScriptV2 -> "PlutusScriptV2"
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
      ("alonzo", "PlutusScriptV1")  -> SomeMarloweTransaction C.PlutusScriptV1 ScriptDataInAlonzoEra <$> obj .: "tx"
      ("alonzo", "PlutusScriptV2")  -> SomeMarloweTransaction C.PlutusScriptV2 ScriptDataInAlonzoEra <$> obj .: "tx"
      ("babbage", "PlutusScriptV1") -> SomeMarloweTransaction C.PlutusScriptV1 ScriptDataInBabbageEra <$> obj .: "tx"
      ("babbage", "PlutusScriptV2") -> SomeMarloweTransaction C.PlutusScriptV2 ScriptDataInBabbageEra <$> obj .: "tx"
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
    miValidatorInfo :: ValidatorInfo lang era  -- ^ Validator information.
  , miDatumInfo     :: DatumInfo               -- ^ Datum information.
  , miRedeemerInfo  :: RedeemerInfo            -- ^ Redeemer information.
  }
    deriving (Eq, Generic, Show)

instance (IsPlutusScriptLanguage lang, IsShelleyBasedEra era) => ToJSON (MarloweInfo lang era) where
  toJSON MarloweInfo{..} =
    object
      [
        "validator" .= toJSON miValidatorInfo
      , "datum"     .= toJSON miDatumInfo
      , "redeemer"  .= toJSON miRedeemerInfo
      ]

instance (IsScriptLanguage lang, IsShelleyBasedEra era) => FromJSON (MarloweInfo lang era) where
  parseJSON =
    withObject "MarloweInfo"
      $ \o ->
        do
          miValidatorInfo <- o .: "validator"
          miDatumInfo     <- o .: "datum"
          miRedeemerInfo  <- o .: "redeemer"
          pure MarloweInfo{..}


-- | Information about Marlowe validator.

-- TODO: Turn this into GADT and introduce two cases - ref and non ref.
-- Non ref should skip `txIn` and ref should change Address into enterprise one.
data ValidatorInfo lang era =
  ValidatorInfo
  {
    viScript  :: PlutusScript lang            -- ^ The Plutus script.
  , viTxIn    :: Maybe C.TxIn                 -- ^ Reference input to use. We don't want to use `PlutusScriptOrReferenceInput` here.
  , viBytes   :: ShortByteString              -- ^ The serialisation of the validator.
  , viHash    :: C.ScriptHash                 -- ^ The validator hash.
  , viAddress :: AddressInEra era             -- ^ The script address.
  , viSize    :: Int                          -- ^ The script size, in bytes.
  , viCost    :: ExBudget                     -- ^ The execution budget for the script.
  }
    deriving (Eq, Generic, Show)


-- | Build validator info.
validatorInfo :: IsPlutusScriptLanguage lang
              => C.PlutusScript lang                         -- ^ The validator.
              -> Maybe TxIn
              -> C.ScriptDataSupportedInEra era              -- ^ The era to build the validator in.
              -> P.ProtocolVersion
              -> P.CostModelParams                           -- ^ The cost model parameters.
              -> C.NetworkId                                 -- ^ The network ID.
              -> C.StakeAddressReference                     -- ^ The stake address.
              -> Either String (ValidatorInfo lang era)      -- ^ The validator information, or an error message.
validatorInfo viScript viTxIn era protocolVersion costModel network stake = do
  let
    C.PlutusScriptSerialised viBytes = viScript
    -- viBytes = SBS.toShort . LBS.toStrict . serialise . Scripts.getValidator $  viScript
    viHash = C.hashScript (C.PlutusScript plutusScriptVersion viScript)
    paymentCredential = C.PaymentCredentialByScript viHash
    viAddress = withShelleyBasedEra era $ C.makeShelleyAddressInEra network paymentCredential stake
    viSize = SBS.length viBytes

  evaluationContext <- Bifunctor.first show $ P.mkEvaluationContext costModel
  case P.evaluateScriptCounting protocolVersion P.Verbose evaluationContext viBytes [] of
    (_, Right viCost) -> pure $ ValidatorInfo{..}
    (_, Left err)     -> Left $ show err


validatorInfo' :: MonadError CliError m
               => IsPlutusScriptLanguage lang
               => C.PlutusScript lang
               -> Maybe C.TxIn
               -> ScriptDataSupportedInEra era
               -> P.ProtocolVersion
               -> P.CostModelParams
               -> C.NetworkId
               -> C.StakeAddressReference
               -> m (ValidatorInfo lang era)
validatorInfo' sc t e p c n st = liftEither . Bifunctor.first CliError $ validatorInfo sc t e p c n st


validatorInfoScriptOrReference :: ValidatorInfo lang era -> C.PlutusScriptOrReferenceInput lang
validatorInfoScriptOrReference ValidatorInfo {..} = case viTxIn of
  Just txIn -> C.PReferenceScript txIn Nothing
  Nothing   -> C.PScript viScript


instance (IsPlutusScriptLanguage lang, IsShelleyBasedEra era) => ToJSON (ValidatorInfo lang era) where
  toJSON ValidatorInfo{..} = do

    object
      [
        "address" .= serialiseAddress viAddress
      , "hash"    .= toJSON viHash
      , "script"  .= toJSON (serialiseToTextEnvelope Nothing (PlutusScript (plutusScriptVersion :: PlutusScriptVersion lang) viScript))
      , "size"    .= toJSON viSize
      , "txIn"    .= toJSON viTxIn
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
          viTxIn    <- o .: "txIn"
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
    txIn     :: TxIn                                  -- ^ The eUTxO to be spent.
  , script   :: C.PlutusScriptOrReferenceInput lang   -- ^ The script.
  , datum    :: Datum                                 -- ^ The datum.
  , redeemer :: Redeemer                              -- ^ The redeemer.
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

data OutputQueryResult era = OutputQueryResult
  {
    oqrMatching    :: C.UTxO era
  , oqrNonMatching :: C.UTxO era
  }


-- | Options for address queries.
data OutputQuery era result where
  -- | Match pure-ADA UTxOs which pass the value check.
  LovelaceOnly :: (Lovelace -> Bool) -> OutputQuery era (OutputQueryResult era)
  -- | Match UTxOs containing only the specified asset.
  AssetOnly :: AssetId -> OutputQuery era (OutputQueryResult era)

  FindReferenceScript :: PlutusScriptVersion lang -> C.ScriptHash -> OutputQuery era (Maybe (AnUTxO era, PlutusScript lang))


data SomeTimeout = AbsoluteTimeout Integer | RelativeTimeout NominalDiffTime
    deriving stock (Eq, Generic, Show)


instance ToJSON SomeTimeout where
  toJSON (AbsoluteTimeout timeout)  = Aeson.object [("absolute", toJSON timeout)]
  toJSON (RelativeTimeout duration) = Aeson.object [("relative", toJSON duration)]


instance FromJSON SomeTimeout where
  parseJSON json = case json of
    Aeson.Object (KeyMap.toList -> [("absolute", absoluteTimeout)]) -> do
      parsedTimeout <- parseJSON absoluteTimeout
      pure $ AbsoluteTimeout parsedTimeout
    Aeson.Object (KeyMap.toList -> [("relative", duration)]) -> do
      parsedDuration <- parseJSON duration
      pure $ RelativeTimeout parsedDuration
    _ -> fail "Expected object with a single field of either `absolute` or `relative`"


someTimeoutToMilliseconds :: MonadIO m => SomeTimeout -> m Integer
someTimeoutToMilliseconds (AbsoluteTimeout t) = pure t
someTimeoutToMilliseconds (RelativeTimeout seconds) = do
  t <- liftIO $ addUTCTime seconds <$> getCurrentTime
  pure $ utcToMilliseconds t
  where
    utcToMilliseconds :: UTCTime -> Integer
    utcToMilliseconds = floor . (1000 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds


toMarloweTimeout :: MonadIO m => SomeTimeout -> m E.Timeout
toMarloweTimeout t = POSIXTime <$> someTimeoutToMilliseconds t


toPOSIXTime :: MonadIO m => SomeTimeout -> m P.POSIXTime
toPOSIXTime t = P.POSIXTime <$> someTimeoutToMilliseconds t


toSlotRoundedMarloweTimeout :: MonadIO m => SlotConfig -> SomeTimeout -> m E.Timeout
toSlotRoundedMarloweTimeout slotConfig t = do
  let
    toSlot = posixTimeToEnclosingSlot slotConfig
  t' <- someTimeoutToMilliseconds t
  let
    P.POSIXTime t'' = slotToBeginPOSIXTime slotConfig . toSlot $ P.POSIXTime t'
  pure $ POSIXTime t''


data PublishingStrategy era =
    PublishPermanently C.StakeAddressReference
  | PublishAtAddress (AddressInEra era)


-- | Information required to publish a script
data PublishScript lang era =
  PublishScript
  {
    psMinAda             :: Lovelace
  , psPublisher          :: AddressInEra era
  , psReferenceValidator :: ValidatorInfo lang era
  }

data PublishMarloweScripts lang era = PublishMarloweScripts
  {
    pmsMarloweScript    :: PublishScript lang era
  , pmsRolePayoutScript :: PublishScript lang era
  }


newtype PrintStats = PrintStats { unPrintStats :: Bool }

newtype TxBodyFile = TxBodyFile { unTxBodyFile :: FilePath }

newtype SigningKeyFile = SigningKeyFile { unSigningKeyFile :: FilePath }


-- | A single UTxO. We preserve the `Tuple` structure for consistency with `UTxO`.
newtype AnUTxO era = AnUTxO { unAnUTxO :: (C.TxIn, C.TxOut C.CtxUTxO era) }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)


fromUTxO :: C.UTxO era -> [AnUTxO era]
fromUTxO (Map.toList . C.unUTxO -> items) = map AnUTxO items


toUTxO :: [AnUTxO era] -> C.UTxO era
toUTxO (map unAnUTxO -> utxos) = C.UTxO . Map.fromList $ utxos


anUTxOValue :: AnUTxO era -> C.Value
anUTxOValue (AnUTxO (_, C.TxOut _ v _ _)) = C.txOutValueToValue v


data MarloweScriptsRefs lang era = MarloweScriptsRefs
  {
    mrMarloweValidator    :: (AnUTxO era, ValidatorInfo lang era)
  , mrRolePayoutValidator :: (AnUTxO era, ValidatorInfo lang era)
  }

data CoinSelectionStrategy = CoinSelectionStrategy
  {
    csPreserveReferenceScripts :: Bool
  , csPreserveInlineDatums     :: Bool
  , csPreserveTxIns            :: [TxIn]
  }

defaultCoinSelectionStrategy :: CoinSelectionStrategy
defaultCoinSelectionStrategy = CoinSelectionStrategy True True []

data CurrencyIssuer era = CurrencyIssuer
  {
    ciIssuer :: AddressInEra era
  , ciSigingKey :: SomePaymentSigningKey
  }

data MintingAction era =
    Mint
    {
      maIssuer :: CurrencyIssuer era
    , maTokenDistribution :: L.NonEmpty (P.TokenName, Natural, AddressInEra era, Maybe Lovelace)
    }
  -- ^ The token names, amount and a possible receipient addresses.
  | BurnAll
    {
      maIssuer :: CurrencyIssuer era
    , maProviders :: L.NonEmpty (AddressInEra era, SomePaymentSigningKey)
    }
  -- ^ Burn all found tokens on the providers UTxOs of a given "private currency".

data SubmitMode = DontSubmit | DoSubmit Second

submitModeFromTimeout :: Maybe Second -> SubmitMode
submitModeFromTimeout Nothing = DontSubmit
submitModeFromTimeout (Just timeout) = DoSubmit timeout
