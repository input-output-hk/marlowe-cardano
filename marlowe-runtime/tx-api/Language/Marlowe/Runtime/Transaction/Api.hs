{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Transaction.Api (
  ApplyInputsConstraintsBuildupError (..),
  ApplyInputsError (..),
  ConstraintError (..),
  ContractCreated (..),
  ContractCreatedInEra (..),
  CreateBuildupError (..),
  CreateError (..),
  InputsApplied (..),
  InputsAppliedInEra (..),
  JobId (..),
  LoadMarloweContextError (..),
  LoadHelpersContextError (..),
  MarloweTxCommand (..),
  Mint (unMint),
  Destination (..),
  HelperScript (..),
  NFTMetadataFile (..),
  RoleTokenMetadata (..),
  RoleTokensConfig (..),
  SubmitError (..),
  SubmitStatus (..),
  Tag (..),
  WalletAddresses (..),
  WithdrawError (..),
  WithdrawTx (..),
  WithdrawTxInEra (..),
  decodeRoleTokenMetadata,
  encodeRoleTokenMetadata,
  mkMint,
) where

import Cardano.Api (
  AnyCardanoEra (..),
  AsType (..),
  BabbageEra,
  ConwayEra,
  IsCardanoEra,
  IsShelleyBasedEra,
  Tx,
  TxBody,
  TxBodyContent (..),
  cardanoEra,
  createAndValidateTransactionBody,
  deserialiseFromCBOR,
  serialiseToCBOR,
  serialiseToTextEnvelope,
 )
import qualified Cardano.Api as C
import Cardano.Api.Shelley (ReferenceTxInsScriptsInlineDatumsSupportedInEra (..))
import qualified Cardano.Api.Shelley as CS
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, FromJSONKey, ToJSON (..), ToJSONKey, Value (..), object, (.!=), (.:!), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import Data.Aeson.Types ((.:))
import qualified Data.Aeson.Types as Aeson.Types
import Data.Binary (Binary, Get, get, getWord8, put)
import Data.Binary.Put (Put, putWord8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, maybeToList)
import Data.SatInt (SatInt)
import Data.Semigroup (Semigroup (..))
import Data.Set (Set)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Type.Equality (type (:~:) (Refl))
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import GHC.Show (showSpace)
import Language.Marlowe.Analysis.Safety.Types (SafetyError)
import qualified Language.Marlowe.Analysis.Safety.Types as Safety
import qualified Language.Marlowe.Core.V1.Semantics as V1
import Language.Marlowe.Runtime.Cardano.Api (cardanoEraToAsType)
import Language.Marlowe.Runtime.Cardano.Feature (hush)
import Language.Marlowe.Runtime.ChainSync.Api (
  Address,
  AssetId,
  Assets,
  BlockHeader,
  DatumHash,
  Lovelace,
  Metadata (..),
  PlutusScript,
  PolicyId (..),
  ScriptHash,
  SlotNo,
  StakeCredential,
  TokenName (..),
  TxId,
  TxOutRef,
  parseMetadataList,
  parseMetadataMap,
  parseMetadataText,
 )
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.History.Api (ExtractCreationError, ExtractMarloweTransactionError)
import Network.HTTP.Media (MediaType)
import Network.Protocol.Codec.Spec (Variations (..), varyAp)
import Network.Protocol.Handshake.Types (HasSignature (..))
import Network.Protocol.Job.Types
import qualified Network.URI as Network
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU, ExMemory)
import qualified PlutusLedgerApi.V2 as PV2

instance Binary Network.URIAuth
instance Binary Network.URI

instance Variations Network.URIAuth
instance Variations Network.URI

instance Binary MediaType where
  put = put . show
  get = fromString <$> get

instance Variations MediaType where
  variations = pure "text/plain"

instance Aeson.ToJSON MediaType where
  toJSON = Aeson.String . Text.pack . show

instance Aeson.FromJSON MediaType where
  parseJSON = fmap fromString . Aeson.parseJSON

data NFTMetadataFile = NFTMetadataFile
  { name :: Text
  , mediaType :: MediaType
  , src :: Network.URI
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Binary, Variations)

instance Aeson.ToJSON NFTMetadataFile where
  toJSON NFTMetadataFile{..} =
    Aeson.object
      [ ("name", toJSON name)
      , ("mediaType", toJSON mediaType)
      , ("src", toJSON $ show src)
      ]

parseJsonUri :: Text -> Aeson.Types.Parser Network.URI
parseJsonUri (Text.unpack -> s) =
  maybe (fail $ s <> " is not a valid URI!") pure $ Network.parseURI s

instance Aeson.FromJSON NFTMetadataFile where
  parseJSON = Aeson.withObject "NFTMetadataFile" \x ->
    NFTMetadataFile
      <$> x .: "name"
      <*> x .: "mediaType"
      <*> (parseJsonUri =<< x .: "src")

data RoleTokenMetadata = RoleTokenMetadata
  { name :: Text
  , image :: Network.URI
  , mediaType :: Maybe MediaType
  , description :: Maybe Text
  , files :: [NFTMetadataFile]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Binary, Variations)

instance Aeson.ToJSON RoleTokenMetadata where
  toJSON RoleTokenMetadata{..} =
    Aeson.Object $
      Aeson.KeyMap.fromList $
        [ ("name", toJSON name)
        , ("image", toJSON $ show image)
        ]
          <> maybeToList (fmap (("mediaType",) . toJSON) mediaType)
          <> maybeToList (fmap (("description",) . Aeson.String) description)
          <> case files of
            [] -> []
            _ -> [("files", toJSON files)]

instance Aeson.FromJSON RoleTokenMetadata where
  parseJSON = Aeson.withObject "RoleTokenMetadata" \x ->
    RoleTokenMetadata
      <$> x .: "name"
      <*> (parseJsonUri =<< x .: "image")
      <*> x .:! "mediaType"
      <*> x .:! "description"
      <*> x .:! "files" .!= []

decodeRoleTokenMetadata :: Metadata -> Maybe RoleTokenMetadata
decodeRoleTokenMetadata = parseNFTMetadataDetails
  where
    parseNFTMetadataDetails :: Metadata -> Maybe RoleTokenMetadata
    parseNFTMetadataDetails metadata = do
      textKeyMap <- parseMetadataRecord metadata
      name <- parseMetadataText =<< Map.lookup "name" textKeyMap
      image <- Network.parseURI . Text.unpack =<< parseSplittableText =<< Map.lookup "image" textKeyMap
      let mediaType = parseMediaType =<< Map.lookup "mediaType" textKeyMap
          description = parseSplittableText =<< Map.lookup "description" textKeyMap
          parseSingleFileDetails = fmap (: []) . parseNFTMetadataFile
          parseManyFileDetails = parseMetadataList parseNFTMetadataFile
          parseFileDetails md = parseSingleFileDetails md <|> parseManyFileDetails md
          files = fromMaybe [] $ parseFileDetails =<< Map.lookup "files" textKeyMap
      Just $ RoleTokenMetadata{..}

    parseNFTMetadataFile :: Metadata -> Maybe NFTMetadataFile
    parseNFTMetadataFile metadata = do
      textKeyMap <- parseMetadataRecord metadata
      name <- parseMetadataText =<< Map.lookup "name" textKeyMap
      mediaType <- parseMediaType =<< Map.lookup "mediaType" textKeyMap
      src <- Network.parseURI . Text.unpack =<< parseSplittableText =<< Map.lookup "src" textKeyMap
      Just $ NFTMetadataFile{..}

    parseMediaType :: Metadata -> Maybe MediaType
    parseMediaType = fmap (fromString . Text.unpack) . parseMetadataText

    parseMetadataRecord :: Metadata -> Maybe (Map Text Metadata)
    parseMetadataRecord = parseMetadataMap parseMetadataText Just

    parseSplittableText :: Metadata -> Maybe Text
    parseSplittableText md = parseMetadataText md <|> mconcat <$> parseMetadataList parseMetadataText md

encodeRoleTokenMetadata :: RoleTokenMetadata -> Metadata
encodeRoleTokenMetadata = encodeNFTMetadataDetails
  where
    encodeNFTMetadataDetails :: RoleTokenMetadata -> Metadata
    encodeNFTMetadataDetails RoleTokenMetadata{..} =
      MetadataMap $
        [ (MetadataText "name", MetadataText name)
        , (MetadataText "image", encodeText $ fromString $ Network.uriToString id image "")
        ]
          <> maybeToList ((MetadataText "mediaType",) . encodeMediaType <$> mediaType)
          <> maybeToList ((MetadataText "description",) . encodeText <$> description)
          <> case files of
            [] -> []
            [fileDetails] ->
              [(MetadataText "files", encodeNFTMetadataFile fileDetails)]
            fileDetails ->
              [(MetadataText "files", MetadataList $ fmap encodeNFTMetadataFile fileDetails)]

    encodeNFTMetadataFile :: NFTMetadataFile -> Metadata
    encodeNFTMetadataFile NFTMetadataFile{..} =
      MetadataMap
        [ (MetadataText "name", MetadataText name)
        , (MetadataText "mediaType", encodeMediaType mediaType)
        , (MetadataText "src", encodeText $ fromString $ Network.uriToString id src "")
        ]

    encodeMediaType :: MediaType -> Metadata
    encodeMediaType = MetadataText . Text.pack . show

    encodeText :: Text -> Metadata
    encodeText = MetadataList . fmap MetadataText . Text.chunksOf 64

data HelperScript = OpenRoleScript
  deriving stock (Read, Show, Bounded, Enum, Eq, Ord, Generic)
  deriving anyclass (Binary, FromJSON, FromJSONKey, ToJSON, ToJSONKey, Variations)

data Destination
  = ToAddress Address
  | ToSelf
  | ToScript HelperScript
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | Non empty mint request.
newtype Mint = Mint {unMint :: Map TokenName (Destination, Maybe RoleTokenMetadata)}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary, Semigroup, Monoid, ToJSON, Variations)

mkMint :: NonEmpty (TokenName, (Destination, Maybe RoleTokenMetadata)) -> Mint
mkMint = Mint . Map.fromList . NonEmpty.toList

data RoleTokensConfig
  = RoleTokensNone
  | RoleTokensUsePolicy PolicyId
  | RoleTokensMint Mint
  | RoleTokensUsePolicyWithOpenRoles PolicyId TokenName [TokenName]
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

data ContractCreated v where
  ContractCreated
    :: ReferenceTxInsScriptsInlineDatumsSupportedInEra era -> ContractCreatedInEra era v -> ContractCreated v

instance Variations (ContractCreated 'V1) where
  variations =
    sconcat
      [ ContractCreated ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> variations
      , ContractCreated ReferenceTxInsScriptsInlineDatumsInConwayEra <$> variations
      ]

instance Show (ContractCreated 'V1) where
  showsPrec p (ContractCreated ReferenceTxInsScriptsInlineDatumsInBabbageEra created) =
    showParen (p > 10) $
      showString "ContractCreated"
        . showSpace
        . showString "ReferenceTxInsScriptsInlineDatumsInBabbageEra"
        . showsPrec 11 created
  showsPrec p (ContractCreated ReferenceTxInsScriptsInlineDatumsInConwayEra created) =
    showParen (p > 10) $
      showString "ContractCreated"
        . showSpace
        . showString "ReferenceTxInsScriptsInlineDatumsInConwayEra"
        . showsPrec 11 created

instance Eq (ContractCreated 'V1) where
  ContractCreated ReferenceTxInsScriptsInlineDatumsInBabbageEra a == ContractCreated ReferenceTxInsScriptsInlineDatumsInBabbageEra b =
    a == b
  ContractCreated ReferenceTxInsScriptsInlineDatumsInBabbageEra _ == _ = False
  ContractCreated ReferenceTxInsScriptsInlineDatumsInConwayEra a == ContractCreated ReferenceTxInsScriptsInlineDatumsInConwayEra b =
    a == b
  ContractCreated ReferenceTxInsScriptsInlineDatumsInConwayEra _ == _ = False

instance ToJSON (ContractCreated 'V1) where
  toJSON (ContractCreated ReferenceTxInsScriptsInlineDatumsInBabbageEra created) =
    object
      [ "era" .= String "babbage"
      , "contractCreated" .= created
      ]
  toJSON (ContractCreated ReferenceTxInsScriptsInlineDatumsInConwayEra created) =
    object
      [ "era" .= String "conway"
      , "contractCreated" .= created
      ]

instance Binary (ContractCreated 'V1) where
  put (ContractCreated ReferenceTxInsScriptsInlineDatumsInBabbageEra created) = do
    putWord8 0
    put created
  put (ContractCreated ReferenceTxInsScriptsInlineDatumsInConwayEra created) = do
    putWord8 1
    put created
  get = do
    eraTag <- getWord8
    case eraTag of
      0 -> ContractCreated ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> get
      1 -> ContractCreated ReferenceTxInsScriptsInlineDatumsInConwayEra <$> get
      _ -> fail $ "Invalid era tag value: " <> show eraTag

data ContractCreatedInEra era v = ContractCreatedInEra
  { contractId :: ContractId
  , rolesCurrency :: PolicyId
  , metadata :: MarloweTransactionMetadata
  , marloweScriptHash :: ScriptHash
  , marloweScriptAddress :: Address
  , payoutScriptHash :: ScriptHash
  , payoutScriptAddress :: Address
  , version :: MarloweVersion v
  , datum :: Datum v
  , assets :: Assets
  , txBody :: TxBody era
  , safetyErrors :: [SafetyError]
  }

deriving instance Show (ContractCreatedInEra BabbageEra 'V1)
deriving instance Show (ContractCreatedInEra ConwayEra 'V1)
deriving instance Eq (ContractCreatedInEra BabbageEra 'V1)
deriving instance Eq (ContractCreatedInEra ConwayEra 'V1)

instance (IsShelleyBasedEra era) => Variations (ContractCreatedInEra era 'V1) where
  variations =
    ContractCreatedInEra
      <$> variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` pure MarloweV1
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations

instance (IsCardanoEra era) => ToJSON (ContractCreatedInEra era 'V1) where
  toJSON ContractCreatedInEra{..} =
    object
      [ "contract-id" .= contractId
      , "roles-currency" .= rolesCurrency
      , "metadata" .= metadata
      , "marlowe-script-hash" .= marloweScriptHash
      , "marlowe-script-address" .= marloweScriptAddress
      , "payout-script-hash" .= payoutScriptHash
      , "payout-script-address" .= payoutScriptAddress
      , "datum" .= datum
      , "assets" .= assets
      , "tx-body" .= serialiseToTextEnvelope Nothing txBody
      , "safety-errors" .= safetyErrors
      ]

instance (IsCardanoEra era) => Binary (ContractCreatedInEra era 'V1) where
  put ContractCreatedInEra{..} = do
    put contractId
    put rolesCurrency
    put metadata
    put marloweScriptHash
    put marloweScriptAddress
    put payoutScriptHash
    put payoutScriptAddress
    putDatum MarloweV1 datum
    put assets
    putTxBody txBody
    put safetyErrors
  get = do
    contractId <- get
    rolesCurrency <- get
    metadata <- get
    marloweScriptHash <- get
    marloweScriptAddress <- get
    payoutScriptHash <- get
    payoutScriptAddress <- get
    datum <- getDatum MarloweV1
    assets <- get
    txBody <- getTxBody
    safetyErrors <- get
    let version = MarloweV1
    pure ContractCreatedInEra{..}

data InputsApplied v where
  InputsApplied
    :: ReferenceTxInsScriptsInlineDatumsSupportedInEra era -> InputsAppliedInEra era v -> InputsApplied v

instance Variations (InputsApplied 'V1) where
  variations = InputsApplied ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> variations

instance Show (InputsApplied 'V1) where
  showsPrec p (InputsApplied ReferenceTxInsScriptsInlineDatumsInBabbageEra created) =
    showParen (p > 10) $
      showString "InputsApplied"
        . showSpace
        . showString "ReferenceTxInsScriptsInlineDatumsInBabbageEra"
        . showsPrec 11 created
  showsPrec p (InputsApplied ReferenceTxInsScriptsInlineDatumsInConwayEra created) =
    showParen (p > 10) $
      showString "InputsApplied"
        . showSpace
        . showString "ReferenceTxInsScriptsInlineDatumsInConwayEra"
        . showsPrec 11 created

instance Eq (InputsApplied 'V1) where
  InputsApplied ReferenceTxInsScriptsInlineDatumsInBabbageEra a == InputsApplied ReferenceTxInsScriptsInlineDatumsInBabbageEra b =
    a == b
  InputsApplied ReferenceTxInsScriptsInlineDatumsInBabbageEra _ == _ = False
  InputsApplied ReferenceTxInsScriptsInlineDatumsInConwayEra a == InputsApplied ReferenceTxInsScriptsInlineDatumsInConwayEra b =
    a == b
  InputsApplied ReferenceTxInsScriptsInlineDatumsInConwayEra _ == _ = False

instance ToJSON (InputsApplied 'V1) where
  toJSON (InputsApplied ReferenceTxInsScriptsInlineDatumsInBabbageEra created) =
    object
      [ "era" .= String "babbage"
      , "contractCreated" .= created
      ]
  toJSON (InputsApplied ReferenceTxInsScriptsInlineDatumsInConwayEra created) =
    object
      [ "era" .= String "conway"
      , "contractCreated" .= created
      ]

instance Binary (InputsApplied 'V1) where
  put (InputsApplied ReferenceTxInsScriptsInlineDatumsInBabbageEra created) = do
    putWord8 0
    put created
  put (InputsApplied ReferenceTxInsScriptsInlineDatumsInConwayEra created) = do
    putWord8 1
    put created
  get = do
    eraTag <- getWord8
    case eraTag of
      0 -> InputsApplied ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> get
      1 -> InputsApplied ReferenceTxInsScriptsInlineDatumsInConwayEra <$> get
      _ -> fail $ "Invalid era tag value: " <> show eraTag

data InputsAppliedInEra era v = InputsAppliedInEra
  { version :: MarloweVersion v
  , contractId :: ContractId
  , metadata :: MarloweTransactionMetadata
  , input :: TransactionScriptOutput v
  , output :: TransactionOutput v
  , invalidBefore :: UTCTime
  , invalidHereafter :: UTCTime
  , inputs :: Inputs v
  , txBody :: TxBody era
  }

deriving instance Show (InputsAppliedInEra BabbageEra 'V1)
deriving instance Eq (InputsAppliedInEra BabbageEra 'V1)
deriving instance Show (InputsAppliedInEra ConwayEra 'V1)
deriving instance Eq (InputsAppliedInEra ConwayEra 'V1)

instance (IsShelleyBasedEra era) => Variations (InputsAppliedInEra era 'V1) where
  variations =
    InputsAppliedInEra MarloweV1
      <$> variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations

instance (IsCardanoEra era) => Binary (InputsAppliedInEra era 'V1) where
  put InputsAppliedInEra{..} = do
    put contractId
    put metadata
    put input
    put output
    put invalidBefore
    put invalidHereafter
    putInputs MarloweV1 inputs
    putTxBody txBody
  get = do
    let version = MarloweV1
    contractId <- get
    metadata <- get
    input <- get
    output <- get
    invalidBefore <- get
    invalidHereafter <- get
    inputs <- getInputs MarloweV1
    txBody <- getTxBody
    pure InputsAppliedInEra{..}

data WithdrawTx v where
  WithdrawTx
    :: ReferenceTxInsScriptsInlineDatumsSupportedInEra era -> WithdrawTxInEra era v -> WithdrawTx v

instance Variations (WithdrawTx 'V1) where
  variations = WithdrawTx ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> variations

instance Show (WithdrawTx 'V1) where
  showsPrec p (WithdrawTx ReferenceTxInsScriptsInlineDatumsInBabbageEra created) =
    showParen (p > 10) $
      showString "WithdrawTx"
        . showSpace
        . showString "ReferenceTxInsScriptsInlineDatumsInBabbageEra"
        . showsPrec 11 created
  showsPrec p (WithdrawTx ReferenceTxInsScriptsInlineDatumsInConwayEra created) =
    showParen (p > 10) $
      showString "WithdrawTx"
        . showSpace
        . showString "ReferenceTxInsScriptsInlineDatumsInConwayEra"
        . showsPrec 11 created

instance Eq (WithdrawTx 'V1) where
  WithdrawTx ReferenceTxInsScriptsInlineDatumsInBabbageEra a == WithdrawTx ReferenceTxInsScriptsInlineDatumsInBabbageEra b =
    a == b
  WithdrawTx ReferenceTxInsScriptsInlineDatumsInBabbageEra _ == _ = False
  WithdrawTx ReferenceTxInsScriptsInlineDatumsInConwayEra a == WithdrawTx ReferenceTxInsScriptsInlineDatumsInConwayEra b =
    a == b
  WithdrawTx ReferenceTxInsScriptsInlineDatumsInConwayEra _ == _ = False

instance Binary (WithdrawTx 'V1) where
  put (WithdrawTx ReferenceTxInsScriptsInlineDatumsInBabbageEra created) = do
    putWord8 0
    put created
  put (WithdrawTx ReferenceTxInsScriptsInlineDatumsInConwayEra created) = do
    putWord8 1
    put created
  get = do
    eraTag <- getWord8
    case eraTag of
      0 -> WithdrawTx ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> get
      1 -> WithdrawTx ReferenceTxInsScriptsInlineDatumsInConwayEra <$> get
      _ -> fail $ "Invalid era tag value: " <> show eraTag

data WithdrawTxInEra era v = WithdrawTxInEra
  { version :: MarloweVersion v
  , inputs :: Map TxOutRef (Payout v)
  , txBody :: TxBody era
  }

deriving instance Show (WithdrawTxInEra BabbageEra 'V1)
deriving instance Eq (WithdrawTxInEra BabbageEra 'V1)
deriving instance Show (WithdrawTxInEra ConwayEra 'V1)
deriving instance Eq (WithdrawTxInEra ConwayEra 'V1)

instance (IsShelleyBasedEra era) => Variations (WithdrawTxInEra era 'V1) where
  variations = WithdrawTxInEra MarloweV1 <$> variations `varyAp` variations

instance (IsCardanoEra era) => Binary (WithdrawTxInEra era 'V1) where
  put WithdrawTxInEra{..} = do
    put inputs
    putTxBody txBody
  get = do
    let version = MarloweV1
    inputs <- get
    txBody <- getTxBody
    pure WithdrawTxInEra{..}

instance (IsCardanoEra era) => ToJSON (InputsAppliedInEra era 'V1) where
  toJSON InputsAppliedInEra{..} =
    object
      [ "contract-id" .= contractId
      , "input" .= input
      , "output" .= output
      , "invalid-before" .= invalidBefore
      , "invalid-hereafter" .= invalidHereafter
      , "inputs" .= inputs
      , "tx-body" .= serialiseToTextEnvelope Nothing txBody
      ]

-- | The low-level runtime API for building and submitting transactions.
data MarloweTxCommand status err result where
  -- | Construct a transaction that starts a new Marlowe contract. The
  -- resulting, unsigned transaction can be signed via the cardano API or a
  -- wallet provider. When signed, the 'Submit' command can be used to submit
  -- the transaction to the attached Cardano node.
  Create
    :: Maybe StakeCredential
    -- ^ A reference to the stake address to use for script addresses.
    -> MarloweVersion v
    -- ^ The Marlowe version to use
    -> WalletAddresses
    -- ^ The wallet addresses to use when constructing the transaction
    -> RoleTokensConfig
    -- ^ How to initialize role tokens
    -> MarloweTransactionMetadata
    -- ^ Optional metadata to attach to the transaction
    -> Maybe Lovelace
    -- ^ Optional min Lovelace deposit which should be used for the contract output.
    -> Either (Contract v) DatumHash
    -- ^ The contract to run, or the hash of the contract to load from the store.
    -> MarloweTxCommand Void CreateError (ContractCreated v)
  -- | Construct a transaction that advances an active Marlowe contract by
  -- applying a sequence of inputs. The resulting, unsigned transaction can be
  -- signed via the cardano API or a wallet provider. When signed, the 'Submit'
  -- command can be used to submit the transaction to the attached Cardano node.
  ApplyInputs
    :: MarloweVersion v
    -- ^ The Marlowe version to use
    -> WalletAddresses
    -- ^ The wallet addresses to use when constructing the transaction
    -> ContractId
    -- ^ The ID of the contract to apply the inputs to.
    -> MarloweTransactionMetadata
    -- ^ Optional metadata to attach to the transaction
    -> Maybe UTCTime
    -- ^ The "invalid before" bound of the validity interval. If omitted, this
    -- is computed from the contract.
    -> Maybe UTCTime
    -- ^ The "invalid hereafter" bound of the validity interval. If omitted, this
    -- is computed from the contract.
    -> Inputs v
    -- ^ The inputs to apply.
    -> MarloweTxCommand Void ApplyInputsError (InputsApplied v)
  -- | Construct a transaction that withdraws available assets from a set of
  -- Marlowe contract payouts. The resulting, unsigned transaction can be signed
  -- via the cardano API or a wallet provider. When signed, the 'Submit' command
  -- can be used to submit the transaction to the attached Cardano node.
  Withdraw
    :: MarloweVersion v
    -- ^ The Marlowe version to use
    -> WalletAddresses
    -- ^ The wallet addresses to use when constructing the transaction
    -> Set TxOutRef
    -- ^ The payouts to withdraw.
    -> MarloweTxCommand
        Void
        WithdrawError
        ( WithdrawTx v -- The unsigned tx body, to be signed by a wallet.
        )
  -- | Submits a signed transaction to the attached Cardano node.
  Submit
    :: ReferenceTxInsScriptsInlineDatumsSupportedInEra era
    -> Tx era
    -- ^ A signed transaction to submit
    -> MarloweTxCommand
        SubmitStatus -- This job reports the status of the tx submission, which can take some time.
        SubmitError
        BlockHeader -- The block header of the block this transaction was added to.

instance HasSignature MarloweTxCommand where
  signature _ = "MarloweTxCommand"

instance OTelCommand MarloweTxCommand where
  commandTypeName _ = "marlowe_tx_command"
  commandName = \case
    TagCreate _ -> "create"
    TagApplyInputs _ -> "apply_inputs"
    TagWithdraw _ -> "withdraw"
    TagSubmit -> "submit"

instance Command MarloweTxCommand where
  data Tag MarloweTxCommand status err result where
    TagCreate :: MarloweVersion v -> Tag MarloweTxCommand Void CreateError (ContractCreated v)
    TagApplyInputs :: MarloweVersion v -> Tag MarloweTxCommand Void ApplyInputsError (InputsApplied v)
    TagWithdraw :: MarloweVersion v -> Tag MarloweTxCommand Void WithdrawError (WithdrawTx v)
    TagSubmit :: Tag MarloweTxCommand SubmitStatus SubmitError BlockHeader

  data JobId MarloweTxCommand stats err result where
    JobIdSubmit :: TxId -> JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader

  tagFromCommand = \case
    Create _ version _ _ _ _ _ -> TagCreate version
    ApplyInputs version _ _ _ _ _ _ -> TagApplyInputs version
    Withdraw version _ _ -> TagWithdraw version
    Submit _ _ -> TagSubmit

  tagFromJobId = \case
    JobIdSubmit _ -> TagSubmit

  tagEq t1 t2 = case (t1, t2) of
    (TagCreate MarloweV1, TagCreate MarloweV1) -> pure (Refl, Refl, Refl)
    (TagCreate _, _) -> Nothing
    (TagApplyInputs MarloweV1, TagApplyInputs MarloweV1) -> pure (Refl, Refl, Refl)
    (TagApplyInputs _, _) -> Nothing
    (TagWithdraw MarloweV1, TagWithdraw MarloweV1) -> pure (Refl, Refl, Refl)
    (TagWithdraw _, _) -> Nothing
    (TagSubmit, TagSubmit) -> pure (Refl, Refl, Refl)
    (TagSubmit, _) -> Nothing

  putTag = \case
    TagCreate version -> putWord8 0x01 *> put (SomeMarloweVersion version)
    TagApplyInputs version -> putWord8 0x02 *> put (SomeMarloweVersion version)
    TagWithdraw version -> putWord8 0x03 *> put (SomeMarloweVersion version)
    TagSubmit -> putWord8 0x04

  getTag = do
    tag <- getWord8
    case tag of
      0x01 -> do
        SomeMarloweVersion version <- get
        pure $ SomeTag $ TagCreate version
      0x02 -> do
        SomeMarloweVersion version <- get
        pure $ SomeTag $ TagApplyInputs version
      0x03 -> do
        SomeMarloweVersion version <- get
        pure $ SomeTag $ TagWithdraw version
      0x04 -> pure $ SomeTag TagSubmit
      _ -> fail $ "Invalid command tag: " <> show tag

  putJobId = \case
    JobIdSubmit txId -> put txId

  getJobId = \case
    TagCreate _ -> fail "create has no job ID"
    TagApplyInputs _ -> fail "apply inputs has no job ID"
    TagWithdraw _ -> fail "withdraw has no job ID"
    TagSubmit -> JobIdSubmit <$> get

  putCommand = \case
    Create mStakeCredential MarloweV1 walletAddresses roles metadata minAda contract -> do
      put mStakeCredential
      put walletAddresses
      put roles
      put metadata
      put minAda
      put contract
    ApplyInputs version walletAddresses contractId metadata invalidBefore invalidHereafter redeemer -> do
      put walletAddresses
      put contractId
      put metadata
      maybe (putWord8 0) (\t -> putWord8 1 *> put t) invalidBefore
      maybe (putWord8 0) (\t -> putWord8 1 *> put t) invalidHereafter
      putInputs version redeemer
    Withdraw _ walletAddresses payoutIds -> do
      put walletAddresses
      put payoutIds
    Submit era tx -> case era of
      ReferenceTxInsScriptsInlineDatumsInBabbageEra -> do
        putWord8 0
        put $ serialiseToCBOR tx
      ReferenceTxInsScriptsInlineDatumsInConwayEra -> do
        putWord8 1
        put $ serialiseToCBOR tx

  getCommand = \case
    TagCreate MarloweV1 -> do
      mStakeCredential <- get
      walletAddresses <- get
      roles <- get
      metadata <- get
      minAda <- get
      Create mStakeCredential MarloweV1 walletAddresses roles metadata minAda <$> get
    TagApplyInputs version -> do
      walletAddresses <- get
      contractId <- get
      metadata <- get
      invalidBefore <-
        getWord8 >>= \case
          0 -> pure Nothing
          1 -> Just <$> get
          t -> fail $ "Invalid Maybe tag: " <> show t
      invalidHereafter <-
        getWord8 >>= \case
          0 -> pure Nothing
          1 -> Just <$> get
          t -> fail $ "Invalid Maybe tag: " <> show t
      redeemer <- getInputs version
      pure $ ApplyInputs version walletAddresses contractId metadata invalidBefore invalidHereafter redeemer
    TagWithdraw version -> do
      walletAddresses <- get
      Withdraw version walletAddresses <$> get
    TagSubmit -> do
      eraTag <- getWord8
      case eraTag of
        0 -> do
          bytes <- get @ByteString
          Submit ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> case deserialiseFromCBOR (AsTx AsBabbage) bytes of
            Left err -> fail $ show err
            Right tx -> pure tx
        1 -> do
          bytes <- get @ByteString
          Submit ReferenceTxInsScriptsInlineDatumsInConwayEra <$> case deserialiseFromCBOR (AsTx AsConway) bytes of
            Left err -> fail $ show err
            Right tx -> pure tx
        _ -> fail $ "Invalid era tag: " <> show eraTag

  putStatus = \case
    TagCreate _ -> absurd
    TagApplyInputs _ -> absurd
    TagWithdraw _ -> absurd
    TagSubmit -> put

  getStatus = \case
    TagCreate _ -> fail "create has no status"
    TagApplyInputs _ -> fail "apply inputs has no status"
    TagWithdraw _ -> fail "withdraw has no status"
    TagSubmit -> get

  putErr = \case
    TagCreate MarloweV1 -> put
    TagApplyInputs MarloweV1 -> put
    TagWithdraw MarloweV1 -> put
    TagSubmit -> put

  getErr = \case
    TagCreate MarloweV1 -> get
    TagApplyInputs MarloweV1 -> get
    TagWithdraw MarloweV1 -> get
    TagSubmit -> get

  putResult = \case
    TagCreate MarloweV1 -> put
    TagApplyInputs MarloweV1 -> put
    TagWithdraw MarloweV1 -> put
    TagSubmit -> put

  getResult = \case
    TagCreate MarloweV1 -> get
    TagApplyInputs MarloweV1 -> get
    TagWithdraw MarloweV1 -> get
    TagSubmit -> get

putTxBody :: (IsCardanoEra era) => TxBody era -> Put
putTxBody = put . serialiseToCBOR

getTxBody :: forall era. (IsCardanoEra era) => Get (TxBody era)
getTxBody = do
  bytes <- get @ByteString
  case deserialiseFromCBOR (AsTxBody $ cardanoEraToAsType $ cardanoEra @era) bytes of
    Left err -> fail $ show err
    Right txBody -> pure txBody

data WalletAddresses = WalletAddresses
  { changeAddress :: Address
  , extraAddresses :: Set Address
  , collateralUtxos :: Set TxOutRef
  }
  deriving (Eq, Show, Generic, Binary, ToJSON, Variations)

-- | Errors that can occur when trying to solve the constraints.
data ConstraintError
  = MintingUtxoNotFound TxOutRef
  | RoleTokenNotFound AssetId
  | ToCardanoError
  | MissingMarloweInput
  | PayoutNotFound TxOutRef
  | InvalidPayoutDatum TxOutRef (Maybe Chain.Datum)
  | InvalidHelperDatum TxOutRef (Maybe Chain.Datum)
  | InvalidPayoutScriptAddress TxOutRef Address
  | CalculateMinUtxoFailed String
  | CoinSelectionFailed String
  | BalancingError String
  | MarloweInputInWithdraw
  | MarloweOutputInWithdraw
  | PayoutOutputInWithdraw
  | PayoutInputInCreateOrApply
  | UnknownPayoutScript ScriptHash
  | HelperScriptNotFound Chain.TokenName
  deriving (Generic)

deriving instance Eq ConstraintError
deriving instance Ord ConstraintError
deriving instance Show ConstraintError
deriving instance Binary ConstraintError
deriving instance Variations ConstraintError
deriving instance ToJSON ConstraintError

data CreateError
  = CreateEraUnsupported AnyCardanoEra
  | CreateConstraintError ConstraintError
  | CreateLoadMarloweContextFailed LoadMarloweContextError
  | CreateLoadHelpersContextFailed LoadHelpersContextError
  | CreateBuildupFailed CreateBuildupError
  | CreateToCardanoError
  | CreateSafetyAnalysisError String -- FIXME: This is a placeholder, pending design of error handling for safety analysis.
  | CreateContractNotFound
  | ProtocolParamNoUTxOCostPerByte
  | InsufficientMinAdaDeposit Lovelace
  | RequiresSingleThreadToken
  deriving (Generic)

deriving instance Eq CreateError
deriving instance Show CreateError
deriving instance Ord CreateError
instance Binary CreateError
instance Variations CreateError
instance ToJSON CreateError

data CreateBuildupError
  = MintingUtxoSelectionFailed
  | AddressDecodingFailed Address
  | MintingScriptDecodingFailed PlutusScript
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

data ApplyInputsError
  = ApplyInputsEraUnsupported AnyCardanoEra
  | ApplyInputsConstraintError ConstraintError
  | ScriptOutputNotFound
  | ApplyInputsLoadMarloweContextFailed LoadMarloweContextError
  | ApplyInputsLoadHelpersContextFailed LoadHelpersContextError
  | ApplyInputsConstraintsBuildupFailed ApplyInputsConstraintsBuildupError
  | SlotConversionFailed String
  | TipAtGenesis
  | ValidityLowerBoundTooHigh SlotNo SlotNo
  deriving (Generic)

deriving instance Eq ApplyInputsError
deriving instance Show ApplyInputsError
instance Binary ApplyInputsError
instance Variations ApplyInputsError
instance ToJSON ApplyInputsError

data ApplyInputsConstraintsBuildupError
  = MarloweComputeTransactionFailed String
  | UnableToDetermineTransactionTimeout
  deriving (Eq, Show, Generic)
  deriving anyclass (Binary, Variations, ToJSON)

data WithdrawError
  = WithdrawEraUnsupported AnyCardanoEra
  | WithdrawConstraintError ConstraintError
  | EmptyPayouts
  | WithdrawLoadHelpersContextFailed LoadHelpersContextError
  deriving (Generic)

deriving instance Eq WithdrawError
deriving instance Show WithdrawError
instance Binary WithdrawError
instance Variations WithdrawError
instance ToJSON WithdrawError

data LoadMarloweContextError
  = LoadMarloweContextErrorNotFound
  | LoadMarloweContextErrorVersionMismatch SomeMarloweVersion
  | LoadMarloweContextToCardanoError
  | MarloweScriptNotPublished ScriptHash
  | PayoutScriptNotPublished ScriptHash
  | ExtractCreationError ExtractCreationError
  | ExtractMarloweTransactionError ExtractMarloweTransactionError
  deriving (Eq, Show, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

data LoadHelpersContextError
  = HelperScriptNotFoundInRegistry HelperScript
  | LoadHelpersContextErrorNotFound TxId
  | LoadHelpersContextErrorVersionMismatch SomeMarloweVersion
  | ContractNotExtractedError ExtractCreationError
  | RollForwardToGenesisError
  | LoadHelpersContextTxOutRefNotFoundError TxOutRef
  deriving (Eq, Show, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

data SubmitError
  = SubmitException String
  | SubmitFailed String -- should be from show TxValidationErrorInMode
  | TxDiscarded
  deriving (Eq, Show, Generic, Binary, ToJSON, Variations)

data SubmitStatus
  = Submitting
  | Accepted
  deriving (Eq, Show, Generic, Binary, ToJSON, Variations)

instance CommandEq MarloweTxCommand where
  commandEq = \case
    Create stake MarloweV1 wallet roleTokenConfig metadata minAda contract -> \case
      Create stake' MarloweV1 wallet' roleTokenConfig' metadata' minAda' contract' ->
        stake == stake'
          && wallet == wallet'
          && roleTokenConfig == roleTokenConfig'
          && metadata == metadata'
          && minAda == minAda'
          && contract == contract'
    ApplyInputs MarloweV1 wallet contractId metadata invalidBefore invalidHereafter inputs -> \case
      ApplyInputs MarloweV1 wallet' contractId' metadata' invalidBefore' invalidHereafter' inputs' ->
        wallet == wallet'
          && contractId == contractId'
          && metadata == metadata'
          && invalidBefore == invalidBefore'
          && invalidHereafter == invalidHereafter'
          && inputs == inputs'
    Withdraw MarloweV1 wallet payoutIds -> \case
      Withdraw MarloweV1 wallet' payoutIds' ->
        wallet == wallet'
          && payoutIds == payoutIds'
    Submit ReferenceTxInsScriptsInlineDatumsInBabbageEra tx -> \case
      Submit ReferenceTxInsScriptsInlineDatumsInBabbageEra tx' -> tx == tx'
      _ -> False
    Submit ReferenceTxInsScriptsInlineDatumsInConwayEra tx -> \case
      Submit ReferenceTxInsScriptsInlineDatumsInConwayEra tx' -> tx == tx'
      _ -> False

  jobIdEq = \case
    JobIdSubmit txId -> \case
      JobIdSubmit txId' -> txId == txId'

  statusEq = \case
    TagCreate MarloweV1 -> (==)
    TagApplyInputs MarloweV1 -> (==)
    TagWithdraw MarloweV1 -> (==)
    TagSubmit -> (==)

  errEq = \case
    TagCreate MarloweV1 -> (==)
    TagApplyInputs MarloweV1 -> (==)
    TagWithdraw MarloweV1 -> (==)
    TagSubmit -> (==)

  resultEq = \case
    TagCreate MarloweV1 -> (==)
    TagApplyInputs MarloweV1 -> (==)
    TagWithdraw MarloweV1 -> (==)
    TagSubmit -> (==)

instance ShowCommand MarloweTxCommand where
  showsPrecTag p = \case
    TagCreate MarloweV1 ->
      showParen
        (p >= 11)
        ( showString "TagCreate"
            . showSpace
            . showString "MarloweV1"
        )
    TagApplyInputs MarloweV1 ->
      showParen
        (p >= 11)
        ( showString "TagApplyInputs"
            . showSpace
            . showString "MarloweV1"
        )
    TagWithdraw MarloweV1 ->
      showParen
        (p >= 11)
        ( showString "TagWithdraw"
            . showSpace
            . showString "MarloweV1"
        )
    TagSubmit -> showString "TagSubmit"

  showsPrecCommand p =
    showParen (p >= 11) . \case
      Create stake MarloweV1 wallet roleTokenConfig metadata minAda contract ->
        ( showString "Create"
            . showSpace
            . showsPrec 11 stake
            . showSpace
            . showsPrec 11 MarloweV1
            . showSpace
            . showsPrec 11 wallet
            . showSpace
            . showsPrec 11 roleTokenConfig
            . showSpace
            . showsPrec 11 metadata
            . showSpace
            . showsPrec 11 minAda
            . showSpace
            . showsPrec 11 contract
        )
      ApplyInputs MarloweV1 wallet contractId metadata invalidBefore invalidHereafter inputs ->
        ( showString "ApplyInputs"
            . showSpace
            . showsPrec 11 MarloweV1
            . showSpace
            . showsPrec 11 wallet
            . showSpace
            . showsPrec 11 contractId
            . showSpace
            . showsPrec 11 metadata
            . showSpace
            . showsPrec 11 invalidBefore
            . showSpace
            . showsPrec 11 invalidHereafter
            . showSpace
            . showsPrec 11 inputs
        )
      Withdraw MarloweV1 wallet payoutIds ->
        ( showString "Withdraw"
            . showSpace
            . showsPrec 11 MarloweV1
            . showSpace
            . showsPrec 11 wallet
            . showSpace
            . showsPrec 11 payoutIds
        )
      Submit ReferenceTxInsScriptsInlineDatumsInBabbageEra tx ->
        ( showString "Submit"
            . showSpace
            . showString "ReferenceTxInsScriptsInlineDatumsInBabbageEra"
            . showSpace
            . showsPrec 11 tx
        )
      Submit ReferenceTxInsScriptsInlineDatumsInConwayEra tx ->
        ( showString "Submit"
            . showSpace
            . showString "ReferenceTxInsScriptsInlineDatumsInConwayEra"
            . showSpace
            . showsPrec 11 tx
        )

  showsPrecJobId p = \case
    JobIdSubmit txId ->
      showParen
        (p >= 11)
        ( showString "JobIdSubmit"
            . showSpace
            . showsPrec 11 txId
        )

  showsPrecStatus p = \case
    TagCreate MarloweV1 -> showsPrec p
    TagApplyInputs MarloweV1 -> showsPrec p
    TagWithdraw MarloweV1 -> showsPrec p
    TagSubmit -> showsPrec p

  showsPrecErr p = \case
    TagCreate MarloweV1 -> showsPrec p
    TagApplyInputs MarloweV1 -> showsPrec p
    TagWithdraw MarloweV1 -> showsPrec p
    TagSubmit -> showsPrec p

  showsPrecResult p = \case
    TagCreate MarloweV1 -> showsPrec p
    TagApplyInputs MarloweV1 -> showsPrec p
    TagWithdraw MarloweV1 -> showsPrec p
    TagSubmit -> showsPrec p

instance Variations V1.TransactionError
instance Variations V1.TransactionWarning
instance Variations V1.Payment
instance Variations V1.TransactionOutput
instance Variations Safety.Transaction
instance Variations SatInt
instance Variations ExCPU
instance Variations ExMemory
instance Variations ExBudget
instance Variations PV2.DatumHash where
  variations = PV2.DatumHash . PV2.toBuiltin <$> variations @ByteString
instance Variations SafetyError

instance (IsShelleyBasedEra era) => Variations (TxBody era) where
  variations =
    either (error . show) pure $
      createAndValidateTransactionBody
        TxBodyContent
          { txIns =
              [
                ( C.TxIn
                    (fromJust $ hush $ C.deserialiseFromRawBytes C.AsTxId $ BS.pack $ replicate 32 0)
                    $ C.TxIx 0
                , C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending
                )
              ]
          , txInsCollateral = C.TxInsCollateralNone
          , txTotalCollateral = C.TxTotalCollateralNone
          , txReturnCollateral = C.TxReturnCollateralNone
          , txInsReference = C.TxInsReferenceNone
          , txOuts = [C.TxOut addr value C.TxOutDatumNone CS.ReferenceScriptNone]
          , txFee = case C.txFeesExplicitInEra (cardanoEra @era) of
              Left implicit -> C.TxFeeImplicit implicit
              Right explicit -> C.TxFeeExplicit explicit 1
          , txValidityRange =
              ( C.TxValidityNoLowerBound
              , case era of
                  C.ShelleyBasedEraShelley -> C.TxValidityUpperBound C.ValidityUpperBoundInShelleyEra $ C.SlotNo 0
                  C.ShelleyBasedEraAllegra -> C.TxValidityNoUpperBound C.ValidityNoUpperBoundInAllegraEra
                  C.ShelleyBasedEraMary -> C.TxValidityNoUpperBound C.ValidityNoUpperBoundInMaryEra
                  C.ShelleyBasedEraAlonzo -> C.TxValidityNoUpperBound C.ValidityNoUpperBoundInAlonzoEra
                  C.ShelleyBasedEraBabbage -> C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra
                  C.ShelleyBasedEraConway -> C.TxValidityNoUpperBound C.ValidityNoUpperBoundInConwayEra
              )
          , txMetadata = C.TxMetadataNone
          , txAuxScripts = C.TxAuxScriptsNone
          , txExtraKeyWits = C.TxExtraKeyWitnessesNone
          , txProtocolParams = C.BuildTxWith Nothing
          , txWithdrawals = C.TxWithdrawalsNone
          , txCertificates = C.TxCertificatesNone
          , txUpdateProposal = C.TxUpdateProposalNone
          , txMintValue = C.TxMintNone
          , txScriptValidity = C.TxScriptValidityNone
          }
    where
      era = C.shelleyBasedEra @era

      addr :: C.AddressInEra era
      addr =
        either (error . show) (C.AddressInEra $ C.ShelleyAddressInEra era) $
          C.deserialiseFromBech32 (C.AsAddress C.AsShelleyAddr) "addr1vy9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupceql82h"

      value :: C.TxOutValue era
      value = case era of
        C.ShelleyBasedEraShelley -> C.TxOutAdaOnly C.AdaOnlyInShelleyEra 1
        C.ShelleyBasedEraAllegra -> C.TxOutAdaOnly C.AdaOnlyInAllegraEra 1
        C.ShelleyBasedEraMary -> C.TxOutValue C.MultiAssetInMaryEra $ C.lovelaceToValue 1
        C.ShelleyBasedEraAlonzo -> C.TxOutValue C.MultiAssetInAlonzoEra $ C.lovelaceToValue 1
        C.ShelleyBasedEraBabbage -> C.TxOutValue C.MultiAssetInBabbageEra $ C.lovelaceToValue 1
        C.ShelleyBasedEraConway -> C.TxOutValue C.MultiAssetInConwayEra $ C.lovelaceToValue 1

instance (IsShelleyBasedEra era) => Variations (Tx era) where
  variations = C.signShelleyTransaction <$> variations <*> pure []
