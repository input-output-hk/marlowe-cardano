-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Types for the Marlowe CLI tool.
module Language.Marlowe.CLI.Types (
  -- * Marlowe Transactions
  CliEnv (..),
  CoinSelectionStrategy (..),
  DatumInfo (..),
  ExecutionLimitsExceeded (..),
  MarloweInfo (..),
  MarlowePlutusVersion,
  MarloweTransaction (..),
  NodeStateInfo (..),
  Percent (..),
  RedeemerInfo (..),
  SomeMarloweTransaction (..),
  SomeTimeout (..),
  SubmitMode (..),
  TxResourceUsage (..),
  ValidatorInfo (..),

  -- * eUTxOs
  AnUTxO (..),
  PayFromScript (..),
  PayToScript (..),

  -- * Minting
  CurrencyIssuer (..),
  TokensRecipient (..),
  MintingAction (..),

  -- * Keys
  SomePaymentSigningKey (..),
  SomePaymentVerificationKey (..),

  -- * Exceptions
  CliError (..),

  -- * Queries
  OutputQuery (..),
  OutputQueryResult (..),
  QueryExecutionContext (..),
  TxBuildupContext (..),

  -- * Publishing
  MarloweScriptsRefs (..),
  PublishingStrategy (..),

  -- * Newtype wrappers
  PrintStats (..),
  SigningKeyFile (..),
  TxBodyFile (..),

  -- * constants
  marlowePlutusVersion,

  -- * pattern matching boilerplate
  askEra,
  asksEra,
  doWithCardanoEra,
  doWithShelleyBasedEra,
  somePaymentSigningKeyToTxWitness,
  toAddressAny',
  toAsType,
  toShelleyAddress,
  txWitnessSigningKeyToSomePaymentSigningKey,

  -- * accessors and converters
  anUTxOValue,
  getVerificationKey,
  queryContextNetworkId,
  toQueryContext,
  submitModeFromTimeout,
  toMarloweExtendedTimeout,
  toPaymentVerificationKey,
  toMarloweTimeout,
  toUTxO,
  toSlotRoundedMarloweTimeout,
  toSlotRoundedPlutusPOSIXTime,
  validatorInfoScriptOrReference,

  -- * constructors and defaults
  defaultCoinSelectionStrategy,
  mkNodeTxBuildup,
  fromUTxO,
  validatorAddress,
  validatorInfo,
  validatorInfo',
) where

import Cardano.Api (
  AddressAny,
  AddressInEra (..),
  AsType (..),
  AssetId,
  BabbageEraOnwards (..),
  GenesisUTxOKey,
  HasTypeProxy (..),
  IsCardanoEra,
  IsScriptLanguage,
  IsShelleyBasedEra,
  Lovelace,
  PaymentExtendedKey,
  PaymentKey,
  PlutusScript,
  PlutusScriptVersion,
  Script (..),
  SigningKey,
  SlotNo,
  TxIn,
  VerificationKey,
  babbageEraOnwardsToCardanoEra,
  babbageEraOnwardsToShelleyBasedEra,
  cardanoEraConstraints,
  castVerificationKey,
  deserialiseAddress,
  deserialiseFromTextEnvelope,
  serialiseAddress,
  serialiseToTextEnvelope,
  shelleyBasedEraConstraints,
  toAddressAny,
 )
import Cardano.Api qualified as C
import Cardano.Api.Byron qualified as CB
import Cardano.Api.Shelley (PlutusScript (..))
import Cardano.Api.Shelley qualified as C
import Cardano.Api.Shelley qualified as CS
import Codec.Serialise (deserialise)
import Contrib.Data.Time.Units as Time.Units
import Contrib.Data.Time.Units.Aeson (Duration (..))
import Control.Concurrent.STM (TVar)
import Control.Exception (Exception)
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (MonadReader (..), asks)
import Control.Monad.Writer (WriterT (..))
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Text qualified as A
import Data.Bifunctor qualified as Bifunctor
import Data.ByteString.Lazy qualified as LBS (fromStrict)
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS (fromShort, length)
import Data.List.NonEmpty qualified as L
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Units (Second)
import GHC.Exts (IsString (fromString))
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Language.Marlowe.CLI.Orphans ()
import Language.Marlowe.Core.V1.Merkle (Continuations)
import Language.Marlowe.Core.V1.Semantics (Payment)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Input, State)
import Language.Marlowe.Core.V1.Semantics.Types qualified as M
import Language.Marlowe.Extended.V1 qualified as E
import Plutus.V1.Ledger.SlotConfig (SlotConfig, posixTimeToEnclosingSlot, slotToBeginPOSIXTime)
import PlutusLedgerApi.V1 (CurrencySymbol, Datum, DatumHash, ExBudget, Redeemer)
import PlutusLedgerApi.V1 qualified as P

-- | Exception for Marlowe CLI.
newtype CliError = CliError {unCliError :: String}
  deriving (Eq, Ord, Read, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsString CliError where
  fromString = CliError

instance Exception CliError

-- | A payment key.
data SomePaymentVerificationKey
  = SomePaymentVerificationKeyPayment (VerificationKey PaymentKey)
  | SomePaymentVerificationKeyPaymentExtended (VerificationKey PaymentExtendedKey)
  | SomePaymentVerificationKeyGenesisUTxO (VerificationKey GenesisUTxOKey)
  deriving (Show)

toPaymentVerificationKey :: SomePaymentVerificationKey -> VerificationKey PaymentKey
toPaymentVerificationKey (SomePaymentVerificationKeyPayment vkey) = vkey
toPaymentVerificationKey (SomePaymentVerificationKeyPaymentExtended vkey) = castVerificationKey vkey
toPaymentVerificationKey (SomePaymentVerificationKeyGenesisUTxO vkey) = castVerificationKey vkey

-- | A payment signing key.
data SomePaymentSigningKey
  = SomePaymentSigningKeyPayment (SigningKey PaymentKey)
  | SomePaymentSigningKeyPaymentExtended (SigningKey PaymentExtendedKey)
  | SomePaymentSigningKeyGenesisUTxO (SigningKey GenesisUTxOKey)
  deriving (Show)

getVerificationKey :: SomePaymentSigningKey -> SomePaymentVerificationKey
getVerificationKey (SomePaymentSigningKeyPayment skey) =
  SomePaymentVerificationKeyPayment $ C.getVerificationKey skey
getVerificationKey (SomePaymentSigningKeyPaymentExtended skey) =
  SomePaymentVerificationKeyPaymentExtended $ C.getVerificationKey skey
getVerificationKey (SomePaymentSigningKeyGenesisUTxO skey) =
  SomePaymentVerificationKeyGenesisUTxO $ C.getVerificationKey skey

somePaymentSigningKeyToTxWitness :: SomePaymentSigningKey -> CS.ShelleyWitnessSigningKey
somePaymentSigningKeyToTxWitness (SomePaymentSigningKeyPayment skey) =
  CS.WitnessPaymentKey skey
somePaymentSigningKeyToTxWitness (SomePaymentSigningKeyPaymentExtended skey) =
  CS.WitnessPaymentExtendedKey skey
somePaymentSigningKeyToTxWitness (SomePaymentSigningKeyGenesisUTxO skey) =
  CS.WitnessGenesisUTxOKey skey

txWitnessSigningKeyToSomePaymentSigningKey :: CS.ShelleyWitnessSigningKey -> Maybe SomePaymentSigningKey
txWitnessSigningKeyToSomePaymentSigningKey = \case
  CS.WitnessPaymentKey skey -> Just $ SomePaymentSigningKeyPayment skey
  CS.WitnessPaymentExtendedKey skey -> Just $ SomePaymentSigningKeyPaymentExtended skey
  CS.WitnessGenesisUTxOKey skey -> Just $ SomePaymentSigningKeyGenesisUTxO skey
  _ -> Nothing

-- | A marlowe transaction in an existentially quantified era
data SomeMarloweTransaction
  = forall era lang.
    (C.IsPlutusScriptLanguage lang) =>
    SomeMarloweTransaction
      (PlutusScriptVersion lang)
      (BabbageEraOnwards era)
      (MarloweTransaction lang era)

-- | Plutus version which we use in the current Marlowe script.
type MarlowePlutusVersion = C.PlutusScriptV2

marlowePlutusVersion :: PlutusScriptVersion MarlowePlutusVersion
marlowePlutusVersion = C.plutusScriptVersion

doWithCardanoEra :: forall era m a. (MonadReader (CliEnv era) m) => ((IsCardanoEra era) => m a) -> m a
doWithCardanoEra m = askEra >>= \era -> cardanoEraConstraints (babbageEraOnwardsToCardanoEra era) m

doWithShelleyBasedEra :: forall era m a. (MonadReader (CliEnv era) m) => ((IsShelleyBasedEra era) => m a) -> m a
doWithShelleyBasedEra m = askEra >>= \era -> shelleyBasedEraConstraints (babbageEraOnwardsToShelleyBasedEra era) m

toAsType :: BabbageEraOnwards era -> AsType era
toAsType = \case
  BabbageEraOnwardsBabbage -> AsBabbageEra
  BabbageEraOnwardsConway -> AsConwayEra

toAddressAny' :: AddressInEra era -> AddressAny
toAddressAny' (AddressInEra _ addr) = toAddressAny addr

toShelleyAddress :: AddressInEra era -> Maybe (C.Address C.ShelleyAddr)
toShelleyAddress (AddressInEra _ addr) = case addr of
  CB.ByronAddress _ -> Nothing
  s@CS.ShelleyAddress{} -> Just s

newtype CliEnv era = CliEnv {era :: BabbageEraOnwards era}

askEra :: (MonadReader (CliEnv era) m) => m (BabbageEraOnwards era)
askEra = asks era

asksEra :: (MonadReader (CliEnv era) m) => (BabbageEraOnwards era -> a) -> m a
asksEra f = f <$> askEra

instance ToJSON SomeMarloweTransaction where
  toJSON (SomeMarloweTransaction plutusVersion era tx) =
    shelleyBasedEraConstraints (babbageEraOnwardsToShelleyBasedEra era) $
      object
        let eraStr :: String
            eraStr = case era of
              BabbageEraOnwardsBabbage -> "babbage"
              BabbageEraOnwardsConway -> "conway"
            plutusVersionStr :: String
            plutusVersionStr = case plutusVersion of
              C.PlutusScriptV1 -> "PlutusScriptV1"
              C.PlutusScriptV2 -> "PlutusScriptV2"
              C.PlutusScriptV3 -> "PlutusScriptV3"
         in [ "era" .= eraStr
            , "plutusVersion" .= plutusVersionStr
            , "tx"
                .= ( ( case plutusVersion of
                        C.PlutusScriptV1 -> toJSON tx
                        C.PlutusScriptV2 -> toJSON tx
                        C.PlutusScriptV3 -> toJSON tx
                     )
                      :: Value
                   )
            ]

instance FromJSON SomeMarloweTransaction where
  parseJSON = withObject "SomeTransaction" $ \obj -> do
    eraStr :: String <- obj .: "era"
    plutusVersionStr :: String <- obj .: "plutusVersion"
    case (eraStr, plutusVersionStr) of
      ("babbage", "PlutusScriptV1") -> SomeMarloweTransaction C.PlutusScriptV1 BabbageEraOnwardsBabbage <$> obj .: "tx"
      ("babbage", "PlutusScriptV2") -> SomeMarloweTransaction C.PlutusScriptV2 BabbageEraOnwardsBabbage <$> obj .: "tx"
      ("conway", "PlutusScriptV1") -> SomeMarloweTransaction C.PlutusScriptV1 BabbageEraOnwardsConway <$> obj .: "tx"
      ("conway", "PlutusScriptV2") -> SomeMarloweTransaction C.PlutusScriptV2 BabbageEraOnwardsConway <$> obj .: "tx"
      _ -> fail $ "Unsupported era " <> show eraStr

-- | Complete description of a Marlowe transaction.
data MarloweTransaction lang era = MarloweTransaction
  { mtValidator :: ValidatorInfo lang era
  -- ^ The Marlowe validator.
  , mtRoleValidator :: ValidatorInfo lang era
  -- ^ The roles validator.
  , mtOpenRoleValidator :: ValidatorInfo lang era
  -- ^ The open roles validator.
  , mtRolesCurrency :: CurrencySymbol
  -- ^ The roles currency.
  , mtState :: State
  -- ^ The Marlowe state after the transaction.
  , mtContract :: Contract
  -- ^ The Marlowe contract after the transaction.
  , mtContinuations :: Continuations
  -- ^ The merkleized continuations for the contract.
  , mtRange :: Maybe (SlotNo, SlotNo)
  -- ^ The slot range for the transaction, if any.
  , mtInputs :: [Input]
  -- ^ The inputs to the transaction.
  , mtPayments :: [Payment]
  -- ^ The payments from the transaction.
  , mtSlotConfig :: SlotConfig
  -- ^ The POSIXTime-to-Slot configuration.
  }
  deriving (Eq, Generic, Show)

instance (C.IsPlutusScriptLanguage lang, IsShelleyBasedEra era) => ToJSON (MarloweTransaction lang era) where
  toJSON MarloweTransaction{..} =
    object
      [ "marloweValidator" .= toJSON mtValidator
      , "rolesValidator" .= toJSON mtRoleValidator
      , "openRolesValidator" .= toJSON mtOpenRoleValidator
      , "roles" .= toJSON mtRolesCurrency
      , "state" .= toJSON mtState
      , "contract" .= toJSON mtContract
      , "continuations" .= toJSON mtContinuations
      , "range" .= toJSON mtRange
      , "inputs" .= toJSON mtInputs
      , "payments" .= toJSON mtPayments
      , "slotConfig" .= toJSON mtSlotConfig
      ]

instance (IsScriptLanguage lang, IsShelleyBasedEra era) => FromJSON (MarloweTransaction lang era) where
  parseJSON =
    withObject "MarloweTransaction" $
      \o ->
        do
          mtValidator <- o .: "marloweValidator"
          mtRoleValidator <- o .: "rolesValidator"
          mtOpenRoleValidator <- o .: "openRolesValidator"
          mtRolesCurrency <- o .: "roles"
          mtState <- o .: "state"
          mtContract <- o .: "contract"
          mtContinuations <- fromMaybe mempty <$> (o .:? "continuations")
          mtRange <- o .: "range"
          mtInputs <- o .: "inputs"
          mtPayments <- o .: "payments"
          mtSlotConfig <- o .: "slotConfig"
          pure MarloweTransaction{..}

-- | Comprehensive information about a Marlowe transaction.
data MarloweInfo lang era = MarloweInfo
  { miValidatorInfo :: ValidatorInfo lang era
  -- ^ Validator information.
  , miDatumInfo :: DatumInfo
  -- ^ Datum information.
  , miRedeemerInfo :: RedeemerInfo
  -- ^ Redeemer information.
  }
  deriving (Eq, Generic, Show)

instance (C.IsPlutusScriptLanguage lang, IsShelleyBasedEra era) => ToJSON (MarloweInfo lang era) where
  toJSON MarloweInfo{..} =
    object
      [ "validator" .= toJSON miValidatorInfo
      , "datum" .= toJSON miDatumInfo
      , "redeemer" .= toJSON miRedeemerInfo
      ]

instance (IsScriptLanguage lang, IsShelleyBasedEra era) => FromJSON (MarloweInfo lang era) where
  parseJSON =
    withObject "MarloweInfo" $
      \o ->
        do
          miValidatorInfo <- o .: "validator"
          miDatumInfo <- o .: "datum"
          miRedeemerInfo <- o .: "redeemer"
          pure MarloweInfo{..}

-- | Information about Marlowe validator.

-- TODO: Turn this into GADT and introduce two cases - ref and non ref.
-- Non ref should skip `txIn` and ref should change Address into enterprise one.
data ValidatorInfo lang era = ValidatorInfo
  { viScript :: PlutusScript lang
  -- ^ The Plutus script.
  , viTxIn :: Maybe C.TxIn
  -- ^ Reference input to use. We don't want to use `PlutusScriptOrReferenceInput` here.
  , viBytes :: ShortByteString
  -- ^ The serialisation of the validator.
  , viHash :: C.ScriptHash
  -- ^ The validator hash.
  , viAddress :: AddressInEra era
  -- ^ The script address.
  , viSize :: Int
  -- ^ The script size, in bytes.
  , viCost :: ExBudget
  -- ^ The execution budget for the script.
  }
  deriving (Eq, Generic, Show)

-- Let's extract validatorAddress build up from the above
validatorAddress
  :: (C.IsPlutusScriptLanguage lang)
  => PlutusScript lang
  -> BabbageEraOnwards era
  -> CS.NetworkId
  -> CS.StakeAddressReference
  -> AddressInEra era
validatorAddress viScript era network stake = do
  let viHash = C.hashScript (C.PlutusScript C.plutusScriptVersion viScript)
      paymentCredential = C.PaymentCredentialByScript viHash
  C.makeShelleyAddressInEra (babbageEraOnwardsToShelleyBasedEra era) network paymentCredential stake

-- | Build validator info.
validatorInfo
  :: (C.IsPlutusScriptLanguage lang)
  => C.PlutusScript lang
  -- ^ The validator.
  -> Maybe TxIn
  -> C.BabbageEraOnwards era
  -- ^ The era to build the validator in.
  -> P.MajorProtocolVersion
  -> [Integer]
  -- ^ The cost model parameters.
  -> C.NetworkId
  -- ^ The network ID.
  -> C.StakeAddressReference
  -- ^ The stake address.
  -> Either String (ValidatorInfo lang era)
  -- ^ The validator information, or an error message.
validatorInfo viScript viTxIn era protocolVersion costModel network stake = do
  let C.PlutusScriptSerialised viBytes = viScript
      viHash = C.hashScript (C.PlutusScript C.plutusScriptVersion viScript)
      viAddress = validatorAddress viScript era network stake
      viSize = SBS.length viBytes

  script <- Bifunctor.first show $ P.deserialiseScript protocolVersion viBytes
  (evaluationContext, _) <- Bifunctor.first show $ runWriterT $ P.mkEvaluationContext costModel
  case P.evaluateScriptCounting protocolVersion P.Verbose evaluationContext script [] of
    (_, Right viCost) -> pure $ ValidatorInfo{..}
    (_, Left err) -> Left $ show err

validatorInfo'
  :: (MonadError CliError m)
  => (C.IsPlutusScriptLanguage lang)
  => C.PlutusScript lang
  -> Maybe C.TxIn
  -> BabbageEraOnwards era
  -> P.MajorProtocolVersion
  -> [Integer]
  -> C.NetworkId
  -> C.StakeAddressReference
  -> m (ValidatorInfo lang era)
validatorInfo' sc t e p c n st = liftEither . Bifunctor.first CliError $ validatorInfo sc t e p c n st

validatorInfoScriptOrReference :: ValidatorInfo lang era -> C.PlutusScriptOrReferenceInput lang
validatorInfoScriptOrReference ValidatorInfo{..} = case viTxIn of
  Just txIn -> C.PReferenceScript txIn Nothing
  Nothing -> C.PScript viScript

instance (C.IsPlutusScriptLanguage lang, IsShelleyBasedEra era) => ToJSON (ValidatorInfo lang era) where
  toJSON ValidatorInfo{..} = do
    object
      [ "address" .= serialiseAddress viAddress
      , "hash" .= toJSON viHash
      , "script"
          .= toJSON (serialiseToTextEnvelope Nothing (PlutusScript (C.plutusScriptVersion :: PlutusScriptVersion lang) viScript))
      , "size" .= toJSON viSize
      , "txIn" .= toJSON viTxIn
      , "cost" .= toJSON viCost
      ]

instance (IsScriptLanguage lang, IsShelleyBasedEra era) => FromJSON (ValidatorInfo lang era) where
  parseJSON =
    withObject "ValidatorInfo" $
      \o ->
        do
          address <- o .: "address"
          viHash <- o .: "hash"
          script <- o .: "script"
          viSize <- o .: "size"
          viTxIn <- o .: "txIn"
          viCost <- o .: "cost"
          viAddress <- case deserialiseAddress (proxyToAsType (Proxy :: Proxy (AddressInEra era))) address of
            Just address' -> pure address'
            Nothing -> fail "Failed deserializing address."

          anyScript <- case deserialiseFromTextEnvelope (proxyToAsType (Proxy :: Proxy (Script lang))) script of
            Right script' -> pure script'
            Left message -> fail $ show message
          (viScript, viBytes) <- case anyScript of
            PlutusScript _ plutusScript@(PlutusScriptSerialised viBytes) -> pure (plutusScript, viBytes)
            _ -> fail "Expecting plutus script."
          pure ValidatorInfo{..}

-- | Information about Marlowe datum.
data DatumInfo = DatumInfo
  { diDatum :: Datum
  -- ^ The datum.
  , diBytes :: ShortByteString
  -- ^ The serialisation of the datum.
  , diJson :: Value
  -- ^ The JSON representation of the datum.
  , diHash :: DatumHash
  -- ^ The hash of the datum.
  , diSize :: Int
  -- ^ The size of the datum, in bytes.
  }
  deriving (Eq, Generic, Show)

instance ToJSON DatumInfo where
  toJSON DatumInfo{..} =
    object
      [ "hash" .= toJSON diHash
      , "cborHex" .= toJSON diBytes
      , "json" .= diJson
      , "size" .= toJSON diSize
      ]

instance FromJSON DatumInfo where
  parseJSON =
    withObject "DatumInfo" $
      \o ->
        do
          diHash <- o .: "hash"
          diBytes <- o .: "cboxHex"
          diJson <- o .: "json"
          diSize <- o .: "size"
          let diDatum = deserialise . LBS.fromStrict $ SBS.fromShort diBytes
          pure DatumInfo{..}

-- | Information about Marlowe redeemer.
data RedeemerInfo = RedeemerInfo
  { riRedeemer :: Redeemer
  -- ^ The redeemer.
  , riBytes :: ShortByteString
  -- ^ The serialisation of the redeemer.
  , riJson :: Value
  -- ^ The JSON representation of the redeemer.
  , riSize :: Int
  -- ^ The size of the redeemer, in bytes.
  }
  deriving (Eq, Generic, Show)

instance ToJSON RedeemerInfo where
  toJSON RedeemerInfo{..} =
    object
      [ "cboxHex" .= toJSON riBytes
      , "json" .= riJson
      , "size" .= toJSON riSize
      ]

instance FromJSON RedeemerInfo where
  parseJSON =
    withObject "RedeemerInfo" $
      \o ->
        do
          riBytes <- o .: "cboxHex"
          riJson <- o .: "json"
          riSize <- o .: "size"
          let riRedeemer = deserialise . LBS.fromStrict $ SBS.fromShort riBytes
          pure RedeemerInfo{..}

-- | Information required to spend from a script.
data PayFromScript lang = PayFromScript
  { txIn :: TxIn
  -- ^ The eUTxO to be spent.
  , script :: C.PlutusScriptOrReferenceInput lang
  -- ^ The script.
  , datum :: Maybe Datum
  -- ^ The datum.
  , redeemer :: Redeemer
  -- ^ The redeemer.
  }
  deriving (Eq, Generic, Show)

-- | Information required to pay to a script.
data PayToScript era = PayToScript
  { address :: AddressInEra era
  -- ^ The script address.
  , value :: C.Value
  -- ^ The value to be paid.
  , datumOut :: C.TxOutDatum C.CtxTx era
  -- ^ The output datum if any.
  }
  deriving (Eq, Generic, Show)

data OutputQueryResult era = OutputQueryResult
  { oqrMatching :: C.UTxO era
  , oqrNonMatching :: C.UTxO era
  }

-- | Options for address queries.
data OutputQuery era result where
  -- | Match pure-ADA UTxOs which pass the value check.
  LovelaceOnly :: (Lovelace -> Bool) -> OutputQuery era (OutputQueryResult era)
  -- | Match UTxOs containing only the specified asset.
  AssetOnly :: AssetId -> OutputQuery era (OutputQueryResult era)
  PolicyIdOnly :: C.PolicyId -> OutputQuery era (OutputQueryResult era)
  FindReferenceScript
    :: PlutusScriptVersion lang -> C.ScriptHash -> OutputQuery era (Maybe (AnUTxO era, PlutusScript lang))

data SomeTimeout = AbsoluteTimeout Integer | RelativeTimeout NominalDiffTime
  deriving stock (Eq, Generic, Show)

instance ToJSON SomeTimeout where
  toJSON (AbsoluteTimeout timeout) = toJSON timeout
  toJSON (RelativeTimeout duration) =
    if duration < 0
      then Aeson.String $ Text.pack $ show duration
      else Aeson.String $ Text.pack $ '+' : show duration

instance FromJSON SomeTimeout where
  parseJSON json = do
    let parseRelativeTime durationJson = do
          Duration microseconds <- parseJSON durationJson
          pure $ RelativeTimeout $ Time.Units.toNominalDiffTime microseconds
        errorMsg =
          "Expecting either relative timeout like +10s, -1h, +1d or just a timestamp"
            <> "or an object with a single field of either `absolute` or `relative` but got:"
            <> TL.unpack (A.encodeToLazyText json)
    case json of
      Aeson.Object (KeyMap.toList -> [("absolute", absoluteTimeout)]) -> do
        parsedTimeout <- parseJSON absoluteTimeout
        pure $ AbsoluteTimeout parsedTimeout
      -- When we use `relative' syntax then we expect seconds
      Aeson.Object (KeyMap.toList -> [("relative", duration)]) -> do
        parsedDuration <- parseJSON duration
        pure $ RelativeTimeout parsedDuration
      -- If starts with `+` or `-` then parse as relative timeout.
      Aeson.String text -> case Text.unpack text of
        ('+' : _) -> parseRelativeTime json
        ('-' : _) -> parseRelativeTime json
        _ -> fail errorMsg
      Aeson.Number _ -> AbsoluteTimeout <$> parseJSON json
      _ -> fail errorMsg

someTimeoutToMilliseconds :: (MonadIO m) => SomeTimeout -> m Integer
someTimeoutToMilliseconds (AbsoluteTimeout t) = pure t
someTimeoutToMilliseconds (RelativeTimeout seconds) = do
  t <- liftIO $ addUTCTime seconds <$> getCurrentTime
  pure $ utcToMilliseconds t
  where
    utcToMilliseconds :: UTCTime -> Integer
    utcToMilliseconds = floor . (1000 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

marloweTimeoutFromPlutusPOSIXTime :: P.POSIXTime -> E.Timeout
marloweTimeoutFromPlutusPOSIXTime = E.POSIXTime . P.getPOSIXTime

toMarloweTimeout :: (MonadIO m) => SomeTimeout -> m M.Timeout
toMarloweTimeout t = P.POSIXTime <$> someTimeoutToMilliseconds t

-- FIXME: s/toMarloweExtendedTimeout/toMarloweExtendedTimeout/
toMarloweExtendedTimeout :: (MonadIO m) => SomeTimeout -> m E.Timeout
toMarloweExtendedTimeout t = marloweTimeoutFromPlutusPOSIXTime <$> toMarloweTimeout t

toSlotRoundedPlutusPOSIXTime :: (MonadIO m) => SlotConfig -> SomeTimeout -> m P.POSIXTime
toSlotRoundedPlutusPOSIXTime slotConfig t = do
  let toSlot = posixTimeToEnclosingSlot slotConfig
  t' <- someTimeoutToMilliseconds t
  pure . slotToBeginPOSIXTime slotConfig . toSlot $ P.POSIXTime t'

toSlotRoundedMarloweTimeout :: (MonadIO m) => SlotConfig -> SomeTimeout -> m E.Timeout
toSlotRoundedMarloweTimeout slotConfig t =
  marloweTimeoutFromPlutusPOSIXTime <$> toSlotRoundedPlutusPOSIXTime slotConfig t

data PublishingStrategy era
  = PublishPermanently C.StakeAddressReference
  | PublishAtAddress (AddressInEra era)

newtype PrintStats = PrintStats {unPrintStats :: Bool}

newtype TxBodyFile = TxBodyFile {unTxBodyFile :: FilePath}

newtype SigningKeyFile = SigningKeyFile {unSigningKeyFile :: FilePath}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | A single UTxO. We preserve the `Tuple` structure for consistency with `UTxO`.
newtype AnUTxO era = AnUTxO {unAnUTxO :: (C.TxIn, C.TxOut C.CtxUTxO era)}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

fromUTxO :: C.UTxO era -> [AnUTxO era]
fromUTxO (Map.toList . C.unUTxO -> items) = map AnUTxO items

toUTxO :: [AnUTxO era] -> C.UTxO era
toUTxO (map unAnUTxO -> utxos) = C.UTxO . Map.fromList $ utxos

anUTxOValue :: forall era. AnUTxO era -> C.Value
anUTxOValue (AnUTxO (_, C.TxOut _ v _ _)) = C.txOutValueToValue v

data MarloweScriptsRefs lang era = MarloweScriptsRefs
  { mrMarloweValidator :: (AnUTxO era, ValidatorInfo lang era)
  , mrRolePayoutValidator :: (AnUTxO era, ValidatorInfo lang era)
  , mrOpenRoleValidator :: (AnUTxO era, ValidatorInfo lang era)
  }

data CoinSelectionStrategy = CoinSelectionStrategy
  { csPreserveReferenceScripts :: Bool
  , csPreserveInlineDatums :: Bool
  , csPreserveTxIns :: [TxIn]
  }

defaultCoinSelectionStrategy :: CoinSelectionStrategy
defaultCoinSelectionStrategy = CoinSelectionStrategy True True []

data CurrencyIssuer era = CurrencyIssuer
  { ciIssuer :: AddressInEra era
  , ciSigningKey :: SomePaymentSigningKey
  }

data TokensRecipient era
  = RegularAddressRecipient (AddressInEra era)
  | ScriptAddressRecipient
      (AddressInEra era)
      -- ^ The value to be paid.
      (C.TxOutDatum C.CtxTx era)
      -- ^ The datum.

data MintingAction era
  = -- | The token names, amount and a possible recipient addresses.
    Mint
      { maIssuer :: CurrencyIssuer era
      , -- , maTokenDistribution
        --     :: L.NonEmpty (AddressInEra era, Maybe Lovelace, [(P.TokenName, Natural)])
        maTokenDistribution
          :: L.NonEmpty (TokensRecipient era, Maybe Lovelace, [(P.TokenName, Natural)])
      }
  | -- | Burn all found tokens on the providers UTxOs of a given "private currency".
    BurnAll
      { maIssuer :: CurrencyIssuer era
      , maProviders :: L.NonEmpty (AddressInEra era, SomePaymentSigningKey)
      }

data SubmitMode = DontSubmit | DoSubmit Second

submitModeFromTimeout :: Maybe Second -> SubmitMode
submitModeFromTimeout Nothing = DontSubmit
submitModeFromTimeout (Just timeout) = DoSubmit timeout

data NodeStateInfo = NodeStateInfo
  { nsiNetworkId :: C.NetworkId
  , nsiProtocolParameters :: C.ProtocolParameters
  , nsiSystemStart :: C.SystemStart
  , nsiEraHistory :: C.EraHistory
  }

-- We slowly migrate all the internal functions to use these types.
-- They allow us to perform fully dry run over contract execution.
data QueryExecutionContext era
  = QueryNode C.LocalNodeConnectInfo
  | PureQueryContext (TVar (C.UTxO era)) NodeStateInfo

queryContextNetworkId :: QueryExecutionContext era -> C.NetworkId
queryContextNetworkId (QueryNode connection) = C.localNodeNetworkId connection
queryContextNetworkId (PureQueryContext _ NodeStateInfo{nsiNetworkId}) = nsiNetworkId

-- Same as above but with timeout information.
data TxBuildupContext era
  = NodeTxBuildup C.LocalNodeConnectInfo SubmitMode
  | PureTxBuildup
      (TVar (C.UTxO era))
      NodeStateInfo

mkNodeTxBuildup :: forall era. C.LocalNodeConnectInfo -> Maybe Second -> TxBuildupContext era
mkNodeTxBuildup connection timeout = NodeTxBuildup connection (submitModeFromTimeout timeout)

toQueryContext :: TxBuildupContext era -> QueryExecutionContext era
toQueryContext (NodeTxBuildup nodeInfo _) = QueryNode nodeInfo
toQueryContext (PureTxBuildup utxo nodeInfo) = PureQueryContext utxo nodeInfo

newtype Percent = Percent {unPercent :: Natural}
  deriving stock (Eq, Generic, Show)
  deriving newtype (FromJSON, ToJSON)

data TxResourceUsage = TxResourceUsage
  { elMemory :: (Natural, Percent)
  , elSteps :: (Natural, Percent)
  , elSize :: (Natural, Percent)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON TxResourceUsage where
  toJSON TxResourceUsage{elMemory, elSteps, elSize} =
    Aeson.object
      [ "memory" .= elMemory
      , "steps" .= elSteps
      , "size" .= elSize
      ]

instance FromJSON TxResourceUsage where
  parseJSON =
    Aeson.withObject "TxResourceUsage" $ \o ->
      TxResourceUsage
        <$> o .: "memory"
        <*> o .: "steps"
        <*> o .: "size"

newtype ExecutionLimitsExceeded = ExecutionLimitsExceeded {unExecutionLimitsExceeded :: TxResourceUsage}
  deriving stock (Eq, Show, Generic)
