{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module defines the request and response types in the Marlowe Runtime
-- | Web API.
module Language.Marlowe.Runtime.Web.Types where

import Control.Applicative ((<|>))
import Control.Lens hiding ((.=))
import Control.Monad (unless, (<=<))
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AMap
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (JSONPathElement (..), Parser, parseFail, prependFailure, toJSONKeyText, typeMismatch)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (decodeBase16, encodeBase16)
import Data.Char (isSpace)
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.OpenApi (
  AdditionalProperties (..),
  Definitions,
  HasAdditionalProperties (..),
  HasType (..),
  NamedSchema (..),
  OpenApiItems (..),
  OpenApiType (..),
  Reference (..),
  Referenced (..),
  Schema,
  ToParamSchema,
  ToSchema,
  declareSchema,
  declareSchemaRef,
  enum_,
  example,
  oneOf,
  pattern,
  properties,
  required,
  toParamSchema,
 )
import qualified Data.OpenApi as OpenApi
import Data.OpenApi.Declare (Declare)
import Data.OpenApi.Schema (ToSchema (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString (..))
import Data.Text (Text, intercalate, splitOn)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Traversable (for)
import Data.Version (Version)
import Data.Word (Word16, Word32, Word64)
import GHC.Exts (IsList)
import GHC.Generics (Generic)
import Language.Marlowe.Analysis.Safety.Types (SafetyError)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as Semantics
import Language.Marlowe.Object.Types (Label (..))
import Language.Marlowe.Runtime.Web.Orphans ()
import Network.URI (parseURI)
import Servant
import Servant.Pagination (HasPagination (..))

-- | A newtype for Base16 decoding and encoding ByteStrings
newtype Base16 = Base16 {unBase16 :: ByteString}
  deriving (Eq, Ord)

instance Show Base16 where
  show = T.unpack . encodeBase16 . unBase16

instance IsString Base16 where
  fromString = either (error . T.unpack) Base16 . decodeBase16 . encodeUtf8 . T.pack

instance ToJSON Base16 where
  toJSON = String . toUrlPiece

instance ToJSONKey Base16 where
  toJSONKey = toJSONKeyText toUrlPiece

instance FromJSON Base16 where
  parseJSON =
    withText "Base16" $ either (parseFail . T.unpack) pure . parseUrlPiece

instance FromJSONKey Base16 where
  fromJSONKey = FromJSONKeyTextParser $ either (parseFail . T.unpack) pure . parseUrlPiece

instance ToHttpApiData Base16 where
  toUrlPiece = encodeBase16 . unBase16

instance FromHttpApiData Base16 where
  parseUrlPiece = fmap Base16 . decodeBase16 . encodeUtf8

instance ToSchema Base16 where
  declareNamedSchema _ = NamedSchema Nothing <$> declareSchema (Proxy @String)

data Assets = Assets
  { lovelace :: Integer
  , tokens :: Tokens
  }
  deriving (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype Tokens = Tokens {unTokens :: Map PolicyId (Map Text Integer)}
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToSchema)

newtype ContractSourceId = ContractSourceId {unContractSourceId :: ByteString}
  deriving (Eq, Ord, Generic)
  deriving (Show, ToHttpApiData, ToJSON) via Base16

instance FromHttpApiData ContractSourceId where
  parseUrlPiece = fmap ContractSourceId . (hasLength 32 . unBase16 <=< parseUrlPiece)

instance FromJSON ContractSourceId where
  parseJSON =
    withText "ContractSourceId" $ either (parseFail . T.unpack) pure . parseUrlPiece

instance ToSchema ContractSourceId where
  declareNamedSchema = pure . NamedSchema (Just "ContractSourceId") . toParamSchema

instance ToParamSchema ContractSourceId where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description ?~ "The hex-encoded identifier of a Marlowe contract source"
      & pattern ?~ "^[a-fA-F0-9]{64}$"

newtype TxId = TxId {unTxId :: ByteString}
  deriving (Eq, Ord, Generic)
  deriving (Show, ToHttpApiData, ToJSON) via Base16

instance FromHttpApiData TxId where
  parseUrlPiece = fmap TxId . (hasLength 32 . unBase16 <=< parseUrlPiece)

instance FromJSON TxId where
  parseJSON =
    withText "TxId" $ either (parseFail . T.unpack) pure . parseUrlPiece

hasLength :: Int -> ByteString -> Either T.Text ByteString
hasLength l bytes
  | BS.length bytes == l = pure bytes
  | otherwise = Left $ "Expected " <> T.pack (show l) <> " bytes"

instance ToSchema TxId where
  declareNamedSchema = pure . NamedSchema (Just "TxId") . toParamSchema

instance ToParamSchema TxId where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description ?~ "The hex-encoded identifier of a Cardano transaction"
      & pattern ?~ "^[a-fA-F0-9]{64}$"

newtype Address = Address {unAddress :: T.Text}
  deriving (Eq, Ord, Generic)
  deriving newtype (Show, ToHttpApiData, FromHttpApiData, ToJSON, FromJSON)

instance ToSchema Address where
  declareNamedSchema = pure . NamedSchema (Just "Address") . toParamSchema

instance ToParamSchema Address where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description ?~ "A cardano address, in Bech32 format"
      & example ?~ "addr1w94f8ywk4fg672xasahtk4t9k6w3aql943uxz5rt62d4dvq8evxaf"

newtype StakeAddress = StakeAddress {unStakeAddress :: T.Text}
  deriving (Eq, Ord, Generic)
  deriving newtype (Show, ToHttpApiData, FromHttpApiData, ToJSON, FromJSON)

instance ToSchema StakeAddress where
  declareNamedSchema = pure . NamedSchema (Just "StakeAddress") . toParamSchema

instance ToParamSchema StakeAddress where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description ?~ "A cardano stake address, in Bech32 format"
      & example ?~ "stake1ux7lyy9nhecm033qsmel9awnr22up6jadlzkrxufr78w82gsfsn0d"

newtype CommaList a = CommaList {unCommaList :: [a]}
  deriving (Eq, Ord, Generic, Functor)
  deriving newtype (Show, ToJSON, FromJSON, IsList)

instance ToParamSchema (CommaList a) where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description ?~ "A comma-separated list of values"

instance (ToSchema a) => ToSchema (CommaList a)

instance (ToHttpApiData a) => ToHttpApiData (CommaList a) where
  toUrlPiece = T.intercalate "," . fmap toUrlPiece . unCommaList
  toQueryParam = T.intercalate "," . fmap toQueryParam . unCommaList

instance (FromHttpApiData a) => FromHttpApiData (CommaList a) where
  parseUrlPiece =
    fmap CommaList
      . traverse (parseUrlPiece . T.dropWhileEnd isSpace . T.dropWhile isSpace)
      . splitOnNonEmpty ","
  parseQueryParam =
    fmap CommaList
      . traverse (parseQueryParam . T.dropWhileEnd isSpace . T.dropWhile isSpace)
      . splitOnNonEmpty ","

splitOnNonEmpty :: Text -> Text -> [Text]
splitOnNonEmpty sep t
  | T.null t = []
  | otherwise = T.splitOn sep t

newtype PolicyId = PolicyId {unPolicyId :: ByteString}
  deriving (Eq, Ord, Generic)
  deriving (Show, ToHttpApiData, FromHttpApiData, ToJSON, ToJSONKey, FromJSON, FromJSONKey) via Base16

instance ToSchema PolicyId where
  declareNamedSchema proxy = pure $ NamedSchema (Just "PolicyId") $ toParamSchema proxy

instance ToParamSchema PolicyId where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description ?~ "The hex-encoded minting policy ID for a native Cardano token"
      & pattern ?~ "^[a-fA-F0-9]*$"

data AssetId = AssetId
  { policyId :: PolicyId
  , assetName :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance ToSchema AssetId
instance FromJSON AssetId
instance ToJSON AssetId

instance ToParamSchema AssetId where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description
        ?~ "A minting policy ID and a token name identifying a specific asset type. Encoded as policyId.tokenName."
      & pattern ?~ "^[a-fA-F0-9]*\\..*$"

instance FromHttpApiData AssetId where
  parseUrlPiece piece = case T.breakOn "." piece of
    (_, "") -> Left "Expected ^[a-fA-F0-9]*(\\.).*$"
    (policyId, tokenNameStartingWitPeriodCharacter) -> AssetId <$> parseUrlPiece policyId <*> parseUrlPiece (T.drop 1 tokenNameStartingWitPeriodCharacter)

instance ToHttpApiData AssetId where
  toUrlPiece AssetId{..} = toUrlPiece policyId <> "." <> toUrlPiece assetName

newtype ScriptHash = ScriptHash {unScriptHash :: ByteString}
  deriving (Eq, Ord, Generic)
  deriving (Show, ToHttpApiData, ToJSON, FromJSON) via Base16

instance ToSchema ScriptHash where
  declareNamedSchema proxy = pure $ NamedSchema (Just "ScriptHash") $ toParamSchema proxy

instance ToParamSchema ScriptHash where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description ?~ "The hex-encoded hash of a Plutus script"
      & pattern ?~ "^[a-fA-F0-9]*$"

newtype Party = Party {unParty :: T.Text}
  deriving (Eq, Ord, Generic)
  deriving newtype (Show, ToHttpApiData, FromHttpApiData, ToJSON, FromJSON, FromJSONKey, ToJSONKey)

instance ToSchema Party where
  declareNamedSchema proxy = pure $ NamedSchema (Just "Party") $ toParamSchema proxy

instance ToParamSchema Party where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description ?~ "Party (A role name or an Address)"

data TxOutRef = TxOutRef
  { txId :: TxId
  , txIx :: Word16
  }
  deriving (Show, Eq, Ord, Generic)

instance FromHttpApiData TxOutRef where
  parseUrlPiece t = case splitOn "#" t of
    [idText, ixText] -> TxOutRef <$> parseUrlPiece idText <*> parseUrlPiece ixText
    _ -> case parseUrlPiece @TxId t of
      Right _ -> Left "Expected [a-fA-F0-9]{64}#[0-9]+ (hint: do you need to URL-encode the '#' as \"%23\"?)"
      _ -> Left "Expected [a-fA-F0-9]{64}#[0-9]+"

instance ToHttpApiData TxOutRef where
  toUrlPiece TxOutRef{..} = toUrlPiece txId <> "#" <> toUrlPiece txIx

instance FromJSON TxOutRef where
  parseJSON =
    withText "TxOutRef" $ either (parseFail . T.unpack) pure . parseUrlPiece

instance FromJSONKey TxOutRef where
  fromJSONKey = FromJSONKeyTextParser $ either (parseFail . T.unpack) pure . parseUrlPiece

instance ToSchema TxOutRef where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "TxOutRef") $
        mempty
          & type_ ?~ OpenApiString
          & OpenApi.description
            ?~ "A reference to a transaction output with a transaction ID and index."
          & pattern ?~ "^[a-fA-F0-9]{64}#[0-9]+$"
          & example ?~ "98d601c9307dd43307cf68a03aad0086d4e07a789b66919ccf9f7f7676577eb7#1"

instance ToParamSchema TxOutRef where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description
        ?~ "A reference to a transaction output with a transaction ID and index. The value must be URL encoded by replacing the '#' character with %23."
      & pattern ?~ "^[a-fA-F0-9]{64}%23[0-9]+$"
      & example ?~ "98d601c9307dd43307cf68a03aad0086d4e07a789b66919ccf9f7f7676577eb7%231"

instance ToJSON TxOutRef where
  toJSON = String . toUrlPiece

instance ToJSONKey TxOutRef where
  toJSONKey = toJSONKeyText toUrlPiece

data MarloweVersion = V1
  deriving (Show, Eq, Ord)

instance ToJSON MarloweVersion where
  toJSON V1 = String "v1"

instance FromJSON MarloweVersion where
  parseJSON =
    withText "MarloweVersion" $ either (parseFail . T.unpack) pure . parseUrlPiece

instance ToHttpApiData MarloweVersion where
  toUrlPiece V1 = "v1"

instance FromHttpApiData MarloweVersion where
  parseUrlPiece "v1" = Right V1
  parseUrlPiece _ =
    Left $
      fold @[]
        [ "expected one of "
        , intercalate "; " ["v1"]
        ]

instance ToSchema MarloweVersion where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "MarloweVersion") $
        mempty
          & type_ ?~ OpenApiString
          & OpenApi.description ?~ "A version of the Marlowe language."
          & enum_ ?~ ["v1"]

data Payout = Payout
  { payoutId :: TxOutRef
  , role :: Text
  , assets :: Assets
  }
  deriving (FromJSON, ToJSON, ToSchema, Show, Eq, Generic)

data ContractState = ContractState
  { contractId :: TxOutRef
  , roleTokenMintingPolicyId :: PolicyId
  , version :: MarloweVersion
  , tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  , initialContract :: Semantics.Contract
  , initialState :: Semantics.State
  , currentContract :: Maybe Semantics.Contract
  , state :: Maybe Semantics.State
  , utxo :: Maybe TxOutRef
  , assets :: Assets
  , txBody :: Maybe TextEnvelope
  , unclaimedPayouts :: [Payout]
  }
  deriving (Show, Eq, Generic)

instance ToJSON ContractState
instance FromJSON ContractState
instance ToSchema ContractState

data ContractHeader = ContractHeader
  { contractId :: TxOutRef
  , roleTokenMintingPolicyId :: PolicyId
  , version :: MarloweVersion
  , tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ContractHeader
instance FromJSON ContractHeader
instance ToSchema ContractHeader

data WithdrawalHeader = WithdrawalHeader
  { withdrawalId :: TxId
  , status :: TxStatus
  , block :: Maybe BlockHeader
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON WithdrawalHeader
instance FromJSON WithdrawalHeader
instance ToSchema WithdrawalHeader

instance HasPagination WithdrawalHeader "withdrawalId" where
  type RangeType WithdrawalHeader "withdrawalId" = TxId
  getFieldValue _ WithdrawalHeader{..} = withdrawalId

data PayoutStatus
  = Available
  | Withdrawn
  deriving (Show, Eq, Ord, Generic)

instance ToJSON PayoutStatus where
  toJSON =
    String . \case
      Available -> "available"
      Withdrawn -> "withdrawn"

instance FromJSON PayoutStatus where
  parseJSON = withText "PayoutStatus" \str -> case T.toLower str of
    "available" -> pure Available
    "withdrawn" -> pure Withdrawn
    _ -> fail "expected \"available\" or \"withdrawn\""

instance ToHttpApiData PayoutStatus where
  toQueryParam = \case
    Available -> "available"
    Withdrawn -> "withdrawn"

instance FromHttpApiData PayoutStatus where
  parseQueryParam str = case T.toLower str of
    "available" -> pure Available
    "withdrawn" -> pure Withdrawn
    _ -> Left "expected \"available\" or \"withdrawn\""

instance ToSchema PayoutStatus where
  declareNamedSchema = pure . NamedSchema (Just "PayoutStatus") . toParamSchema

instance ToParamSchema PayoutStatus where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & enum_ ?~ ["available", "withdrawn"]
      & OpenApi.description
        ?~ "The status of a payout. Either it is available to be withdrawn, or it has already been withdrawn."

data PayoutHeader = PayoutHeader
  { payoutId :: TxOutRef
  , contractId :: TxOutRef
  , withdrawalId :: Maybe TxId
  , role :: AssetId
  , status :: PayoutStatus
  }
  deriving (Show, Eq, Ord, Generic)

instance HasPagination PayoutHeader "payoutId" where
  type RangeType PayoutHeader "payoutId" = TxOutRef
  getFieldValue _ PayoutHeader{..} = payoutId

instance ToJSON PayoutHeader
instance FromJSON PayoutHeader
instance ToSchema PayoutHeader

data PayoutState = PayoutState
  { payoutId :: TxOutRef
  , contractId :: TxOutRef
  , withdrawalId :: Maybe TxId
  , role :: AssetId
  , payoutValidatorAddress :: Address
  , status :: PayoutStatus
  , assets :: Assets
  }
  deriving (FromJSON, ToJSON, ToSchema, Show, Eq, Generic)

data Withdrawal = Withdrawal
  { payouts :: Set PayoutHeader
  , withdrawalId :: TxId
  , status :: TxStatus
  , block :: Maybe BlockHeader
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Withdrawal
instance FromJSON Withdrawal
instance ToSchema Withdrawal

instance HasPagination ContractHeader "contractId" where
  type RangeType ContractHeader "contractId" = TxOutRef
  getFieldValue _ ContractHeader{..} = contractId

newtype Metadata = Metadata {unMetadata :: Value}
  deriving (Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

instance ToSchema Metadata where
  declareNamedSchema _ = do
    integerSchema <- declareSchemaRef $ Proxy @Integer
    let metadataSchema = Ref $ Reference "Metadata"
        binaryTextSchema =
          mempty
            & OpenApi.description ?~ "Hex-encoded binary data of up to 64 bytes"
            & OpenApi.type_ ?~ OpenApiString
            & OpenApi.pattern ?~ "0x[A-Fa-f0-9]{0,128}"
        plainTextSchema =
          mempty
            & OpenApi.description ?~ "Text data of up to 64 characters"
            & OpenApi.type_ ?~ OpenApiString
        metadataArraySchema =
          mempty
            & OpenApi.description ?~ "Array of metadata values"
            & OpenApi.type_ ?~ OpenApiArray
            & OpenApi.items ?~ OpenApiItemsObject metadataSchema
        metadataObjectSchema =
          mempty
            & OpenApi.description ?~ "Object of metadata values"
            & OpenApi.type_ ?~ OpenApiObject
            & OpenApi.additionalProperties ?~ AdditionalPropertiesSchema metadataSchema
    pure $
      NamedSchema (Just "Metadata") $
        mempty
          & OpenApi.description ?~ "Arbitrary JSON-encoded transaction metadata"
          & oneOf
            ?~ [ integerSchema
               , Inline binaryTextSchema
               , Inline plainTextSchema
               , Inline metadataArraySchema
               , Inline metadataObjectSchema
               ]

data TxHeader = TxHeader
  { contractId :: TxOutRef
  , transactionId :: TxId
  , tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  , utxo :: Maybe TxOutRef
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON TxHeader
instance FromJSON TxHeader
instance ToSchema TxHeader

data Tx = Tx
  { contractId :: TxOutRef
  , transactionId :: TxId
  , tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  , inputUtxo :: TxOutRef
  , inputContract :: Semantics.Contract
  , inputState :: Semantics.State
  , inputs :: [Semantics.Input]
  , outputUtxo :: Maybe TxOutRef
  , outputContract :: Maybe Semantics.Contract
  , outputState :: Maybe Semantics.State
  , assets :: Assets
  , payouts :: [Payout]
  , consumingTx :: Maybe TxId
  , invalidBefore :: UTCTime
  , invalidHereafter :: UTCTime
  , reconstructedSemanticInput :: V1.TransactionInput
  , reconstructedSemanticOutput :: V1.TransactionOutput
  , txBody :: Maybe TextEnvelope
  }
  deriving (Show, Eq, Generic)

instance ToJSON Tx
instance FromJSON Tx
instance ToSchema Tx

instance HasPagination TxHeader "transactionId" where
  type RangeType TxHeader "transactionId" = TxId
  getFieldValue _ TxHeader{..} = transactionId

data TxStatus
  = Unsigned
  | Submitted
  | Confirmed
  deriving (Show, Eq, Ord)

instance ToJSON TxStatus where
  toJSON Unsigned = String "unsigned"
  toJSON Submitted = String "submitted"
  toJSON Confirmed = String "confirmed"

instance FromJSON TxStatus where
  parseJSON (String "unsigned") = pure Unsigned
  parseJSON (String "submitted") = pure Submitted
  parseJSON (String "confirmed") = pure Confirmed
  parseJSON _ = parseFail "invalid status"

instance ToSchema TxStatus where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "TxStatus") $
        mempty
          & type_ ?~ OpenApiString
          & enum_ ?~ ["unsigned", "submitted", "confirmed"]
          & OpenApi.description ?~ "The status of a transaction on the local node."

data BlockHeader = BlockHeader
  { slotNo :: Word64
  , blockNo :: Word64
  , blockHeaderHash :: Base16
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON BlockHeader
instance FromJSON BlockHeader
instance ToSchema BlockHeader

data CardanoTx
data CardanoTxBody

data WithdrawTxEnvelope tx = WithdrawTxEnvelope
  { withdrawalId :: TxId
  , txEnvelope :: TextEnvelope
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON (WithdrawTxEnvelope CardanoTx) where
  toJSON WithdrawTxEnvelope{..} =
    object
      [ ("withdrawalId", toJSON withdrawalId)
      , ("tx", toJSON txEnvelope)
      ]
instance ToJSON (WithdrawTxEnvelope CardanoTxBody) where
  toJSON WithdrawTxEnvelope{..} =
    object
      [ ("withdrawalId", toJSON withdrawalId)
      , ("txBody", toJSON txEnvelope)
      ]

instance FromJSON (WithdrawTxEnvelope CardanoTx) where
  parseJSON = withObject "WithdrawTxEnvelope" \obj ->
    WithdrawTxEnvelope
      <$> obj .: "withdrawalId"
      <*> obj .: "tx"

instance FromJSON (WithdrawTxEnvelope CardanoTxBody) where
  parseJSON = withObject "WithdrawTxEnvelope" \obj ->
    WithdrawTxEnvelope
      <$> obj .: "withdrawalId"
      <*> obj .: "txBody"

instance ToSchema (WithdrawTxEnvelope CardanoTx) where
  declareNamedSchema _ = do
    withdrawalIdSchema <- declareSchemaRef (Proxy :: Proxy TxId)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    return $
      NamedSchema (Just "WithdrawTxEnvelope") $
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "The \"type\" property of \"tx\" must be \"Tx BabbageEra\" or \"Tx ConwayEra\""
          & properties
            .~ [ ("withdrawalId", withdrawalIdSchema)
               , ("tx", txEnvelopeSchema)
               ]
          & required .~ ["withdrawalId", "tx"]

instance ToSchema (WithdrawTxEnvelope CardanoTxBody) where
  declareNamedSchema _ = do
    withdrawalIdSchema <- declareSchemaRef (Proxy :: Proxy TxId)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    return $
      NamedSchema (Just "WithdrawTxBodyEnvelope") $
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "The \"type\" property of \"txBody\" must be \"TxBody BabbageEra\" or \"TxBody ConwayEra\""
          & properties
            .~ [ ("withdrawalId", withdrawalIdSchema)
               , ("txBody", txEnvelopeSchema)
               ]
          & required .~ ["withdrawalId", "txBody"]

data CreateTxEnvelope tx = CreateTxEnvelope
  { contractId :: TxOutRef
  , txEnvelope :: TextEnvelope
  , safetyErrors :: [SafetyError]
  }
  deriving (Show, Eq, Generic)

instance ToJSON (CreateTxEnvelope CardanoTx) where
  toJSON CreateTxEnvelope{..} =
    object
      [ ("contractId", toJSON contractId)
      , ("tx", toJSON txEnvelope)
      , ("safetyErrors", toJSON safetyErrors)
      ]
instance ToJSON (CreateTxEnvelope CardanoTxBody) where
  toJSON CreateTxEnvelope{..} =
    object
      [ ("contractId", toJSON contractId)
      , ("txBody", toJSON txEnvelope)
      , ("safetyErrors", toJSON safetyErrors)
      ]

instance FromJSON (CreateTxEnvelope CardanoTx) where
  parseJSON = withObject "CreateTxEnvelope" \obj ->
    CreateTxEnvelope
      <$> obj .: "contractId"
      <*> obj .: "tx"
      <*> obj .: "safetyErrors"

instance FromJSON (CreateTxEnvelope CardanoTxBody) where
  parseJSON = withObject "CreateTxEnvelope" \obj ->
    CreateTxEnvelope
      <$> obj .: "contractId"
      <*> obj .: "txBody"
      <*> obj .: "safetyErrors"

instance ToSchema (CreateTxEnvelope CardanoTx) where
  declareNamedSchema _ = do
    contractIdSchema <- declareSchemaRef (Proxy :: Proxy TxOutRef)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    safetyErrorsSchema <- declareSchemaRef (Proxy :: Proxy [SafetyError])
    return $
      NamedSchema (Just "CreateTxEnvelope") $
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "The \"type\" property of \"tx\" must be \"Tx BabbageEra\" or \"Tx ConwayEra\""
          & properties
            .~ [ ("contractId", contractIdSchema)
               , ("tx", txEnvelopeSchema)
               , ("safetyErrors", safetyErrorsSchema)
               ]
          & required .~ ["contractId", "tx"]

instance ToSchema (CreateTxEnvelope CardanoTxBody) where
  declareNamedSchema _ = do
    contractIdSchema <- declareSchemaRef (Proxy :: Proxy TxOutRef)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    safetyErrorsSchema <- declareSchemaRef (Proxy :: Proxy [SafetyError])
    return $
      NamedSchema (Just "CreateTxBodyEnvelope") $
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "The \"type\" property of \"txBody\" must be \"TxBody BabbageEra\" or \"TxBody ConwayEra\""
          & properties
            .~ [ ("contractId", contractIdSchema)
               , ("txBody", txEnvelopeSchema)
               , ("safetyErrors", safetyErrorsSchema)
               ]
          & required .~ ["contractId", "txBody"]

data TextEnvelope = TextEnvelope
  { teType :: Text
  , teDescription :: Text
  , teCborHex :: Base16
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON TextEnvelope where
  toJSON TextEnvelope{..} =
    object
      [ ("type", toJSON teType)
      , ("description", toJSON teDescription)
      , ("cborHex", toJSON teCborHex)
      ]

instance FromJSON TextEnvelope where
  parseJSON = withObject "TextEnvelope" \obj ->
    TextEnvelope
      <$> obj .: "type"
      <*> obj .: "description"
      <*> obj .: "cborHex"

instance ToSchema TextEnvelope where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy @Text)
    let typeSchema =
          mempty
            & type_ ?~ OpenApiString
            & OpenApi.description
              ?~ "What type of data is encoded in the CBOR Hex. Valid values include \"Tx <era>\", \"TxBody <era>\", and \"ShelleyTxWitness <era>\" where <era> is one of \"BabbageEra\", \"ConwayEra\"."
    pure $
      NamedSchema (Just "TextEnvelope") $
        mempty
          & type_ ?~ OpenApiObject
          & required .~ ["type", "description", "cborHex"]
          & properties
            .~ [ ("type", Inline typeSchema)
               , ("description", textSchema)
               , ("cborHex", textSchema)
               ]

data PostContractSourceResponse = PostContractSourceResponse
  { contractSourceId :: ContractSourceId
  , intermediateIds :: Map Label ContractSourceId
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PostContractSourceResponse
instance ToJSON PostContractSourceResponse
instance ToSchema PostContractSourceResponse

newtype PostWithdrawalsRequest = PostWithdrawalsRequest
  { payouts :: Set TxOutRef
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PostWithdrawalsRequest
instance ToJSON PostWithdrawalsRequest
instance ToSchema PostWithdrawalsRequest

data PostContractsRequest = PostContractsRequest
  { tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , version :: MarloweVersion
  , roles :: Maybe RolesConfig
  , threadTokenName :: Maybe Text
  , contract :: ContractOrSourceId
  , accounts :: Map Party Assets
  , minUTxODeposit :: Maybe Word64
  }
  deriving (Show, Eq, Generic)

instance FromJSON PostContractsRequest
instance ToJSON PostContractsRequest
instance ToSchema PostContractsRequest

newtype ContractOrSourceId = ContractOrSourceId (Either Semantics.Contract ContractSourceId)
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ContractOrSourceId where
  parseJSON =
    fmap ContractOrSourceId . \case
      String "close" -> pure $ Left Semantics.Close
      String s -> Right <$> parseJSON (String s)
      j -> Left <$> parseJSON j

instance ToJSON ContractOrSourceId where
  toJSON = \case
    ContractOrSourceId (Left contract) -> toJSON contract
    ContractOrSourceId (Right hash) -> toJSON hash

instance ToSchema ContractOrSourceId where
  declareNamedSchema _ = do
    contractSchema <- declareSchemaRef $ Proxy @Semantics.Contract
    contractSourceIdSchema <- declareSchemaRef $ Proxy @ContractSourceId
    pure $
      NamedSchema Nothing $
        mempty
          & oneOf ?~ [contractSchema, contractSourceIdSchema]

data RolesConfig
  = UsePolicy PolicyId
  | UsePolicyWithOpenRoles PolicyId [Text]
  | Mint (Map Text RoleTokenConfig)
  deriving (Show, Eq, Ord, Generic)

instance FromJSON RolesConfig where
  parseJSON (String s) = UsePolicy <$> parseJSON (String s)
  parseJSON value =
    withObject
      "RolesConfig"
      ( \obj ->
          let parseMint = Mint <$> parseJSON value
              parseOpen =
                do
                  script <- obj .: "script"
                  unless (script == ("OpenRole" :: String)) $ fail "AllowedValues: \"OpenRole\""
                  UsePolicyWithOpenRoles <$> obj .: "policyId" <*> obj .: "openRoleNames"
           in parseOpen <|> parseMint
      )
      value

instance ToJSON RolesConfig where
  toJSON (UsePolicy policy) = toJSON policy
  toJSON (UsePolicyWithOpenRoles policy openRoleNames) =
    object
      [ "script" .= ("OpenRole" :: String)
      , "policyId" .= policy
      , "openRoleNames" .= openRoleNames
      ]
  toJSON (Mint configs) = toJSON configs

instance ToSchema RolesConfig where
  declareNamedSchema _ = do
    policySchema <- declareSchemaRef (Proxy @PolicyId)
    mintSchema <- declareSchemaRef (Proxy @(Map Text RoleTokenConfig))
    pure $
      NamedSchema (Just "RolesConfig") $
        mempty
          & oneOf ?~ [policySchema, mintSchema]

data RoleTokenConfig = RoleTokenConfig
  { recipients :: RoleTokenRecipients
  , metadata :: Maybe TokenMetadata
  }
  deriving (Show, Eq, Ord, Generic)

type RoleTokenRecipients = Map RoleTokenRecipient Word64

data RoleTokenRecipient
  = ClosedRole Address
  | OpenRole
  deriving (Show, Eq, Ord, Generic)

roleTokenRecipientToText :: RoleTokenRecipient -> Text
roleTokenRecipientToText = \case
  ClosedRole addr -> unAddress addr
  OpenRole -> "OpenRole"

roleTokenRecipientFromText :: Text -> RoleTokenRecipient
roleTokenRecipientFromText = \case
  "OpenRole" -> OpenRole
  addr -> ClosedRole $ Address addr

instance ToJSON RoleTokenRecipient where
  toJSON = String . roleTokenRecipientToText

instance ToJSONKey RoleTokenRecipient where
  toJSONKey = toJSONKeyText roleTokenRecipientToText

instance FromJSON RoleTokenRecipient where
  parseJSON = withText "RoleTokenRecipient" $ pure . roleTokenRecipientFromText

instance FromJSONKey RoleTokenRecipient where
  fromJSONKey = FromJSONKeyText roleTokenRecipientFromText

instance FromJSON RoleTokenConfig where
  parseJSON (String "OpenRole") =
    pure
      . flip RoleTokenConfig Nothing
      $ Map.singleton OpenRole 1
  parseJSON (String s) =
    pure
      . flip RoleTokenConfig Nothing
      . flip Map.singleton 1
      . ClosedRole
      $ Address s
  parseJSON value =
    withObject
      "RoleTokenConfig"
      ( \obj -> do
          mRecipients <- obj .:? "recipients"
          mAddress <- obj .:? "address"
          mScriptRole <- do
            mScript :: Maybe String <- obj .:? "script"
            for mScript \case
              "OpenRole" -> pure OpenRole
              _ -> fail "Expected \'OpenRole\""
          metadata <- obj .:? "metadata"
          recipients <- case (mRecipients, mAddress, mScriptRole) of
            (Just recipients, _, _) -> pure recipients
            (_, Just address, _) -> pure $ Map.singleton (ClosedRole address) 1
            (_, _, Just scriptRole) -> pure $ Map.singleton scriptRole 1
            _ -> fail "one of recipients, address, or script required"
          pure RoleTokenConfig{..}
      )
      value

instance ToJSON RoleTokenConfig where
  toJSON (RoleTokenConfig recipients metadata) =
    object
      [ "recipients" .= recipients
      , "metadata" .= metadata
      ]

instance ToSchema RoleTokenConfig where
  declareNamedSchema _ = do
    simpleSchema <- declareSchemaRef (Proxy @Address)
    metadataSchema <- declareSchemaRef (Proxy @TokenMetadata)
    quantitySchema <- declareSchemaRef (Proxy @Word64)
    let multiSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ ["recipients"]
            & properties
              .~ [
                   ( "recipients"
                   , Inline $
                      mempty
                        & type_ ?~ OpenApiObject
                        & additionalProperties ?~ AdditionalPropertiesSchema quantitySchema
                   )
                 , ("metadata", metadataSchema)
                 ]
        advancedSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ ["address"]
            & properties
              .~ [ ("address", simpleSchema)
                 , ("metadata", metadataSchema)
                 ]
        scriptSchema =
          mempty
            & type_ ?~ OpenApiString
            & OpenApi.description ?~ "The type of script receiving the role token."
            & enum_ ?~ ["OpenRole"]
        openSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ ["script"]
            & properties
              .~ [ ("script", Inline scriptSchema)
                 , ("metadata", metadataSchema)
                 ]
    pure $
      NamedSchema (Just "RoleTokenConfig") $
        mempty
          & oneOf ?~ [Inline multiSchema, simpleSchema, Inline advancedSchema, Inline openSchema]

data TokenMetadata = TokenMetadata
  { name :: Text
  , image :: URI
  , mediaType :: Maybe Text
  , description :: Maybe Text
  , files :: Maybe [TokenMetadataFile]
  , additionalProps :: Aeson.Object
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON TokenMetadata where
  parseJSON = withObject "TokenMetadata" \obj -> do
    imageJSON <- obj .: "image"
    let additionalProps =
          AMap.delete "name"
            . AMap.delete "image"
            . AMap.delete "mediaType"
            . AMap.delete "description"
            . AMap.delete "files"
            $ obj
    TokenMetadata
      <$> obj .: "name"
      <*> uriFromJSON imageJSON
      <*> obj .:? "mediaType"
      <*> obj .:? "description"
      <*> obj .:? "files"
      <*> pure additionalProps

instance ToJSON TokenMetadata where
  toJSON TokenMetadata{..} =
    object $
      [ "name" .= name
      , "image" .= uriToJSON image
      , "mediaType" .= mediaType
      , "description" .= description
      , "files" .= files
      ]
        <> AMap.toList additionalProps

instance ToSchema TokenMetadata where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (Proxy @Text)
    filesSchema <- declareSchemaRef (Proxy @[TokenMetadataFile])
    metadataSchema <- declareSchemaRef (Proxy @Metadata)
    pure $
      NamedSchema (Just "TokenMetadata") $
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "Metadata for an NFT, as described by https://cips.cardano.org/cips/cip25/"
          & required .~ ["name", "image"]
          & properties
            .~ [ ("name", stringSchema)
               , ("image", stringSchema)
               , ("mediaType", stringSchema)
               , ("description", stringSchema)
               , ("files", filesSchema)
               ]
          & additionalProperties ?~ AdditionalPropertiesSchema metadataSchema

data TokenMetadataFile = TokenMetadataFile
  { name :: Text
  , src :: URI
  , mediaType :: Text
  , additionalProps :: Aeson.Object
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON TokenMetadataFile where
  parseJSON = withObject "TokenMetadataFile" \obj -> do
    srcJSON <- obj .: "src"
    let additionalProps =
          AMap.delete "name"
            . AMap.delete "mediaType"
            . AMap.delete "src"
            $ obj
    TokenMetadataFile
      <$> obj .: "name"
      <*> uriFromJSON srcJSON
      <*> obj .: "mediaType"
      <*> pure additionalProps

instance ToJSON TokenMetadataFile where
  toJSON TokenMetadataFile{..} =
    object $
      [ ("name", toJSON name)
      , ("src", uriToJSON src)
      , ("mediaType", toJSON mediaType)
      ]
        <> AMap.toList additionalProps

instance ToSchema TokenMetadataFile where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (Proxy @Text)
    metadataSchema <- declareSchemaRef (Proxy @Metadata)
    pure $
      NamedSchema (Just "TokenMetadataFile") $
        mempty
          & type_ ?~ OpenApiObject
          & required .~ ["name", "src", "mediaType"]
          & properties
            .~ [ ("name", stringSchema)
               , ("src", stringSchema)
               , ("mediaType", stringSchema)
               ]
          & additionalProperties ?~ AdditionalPropertiesSchema metadataSchema

uriFromJSON :: Value -> Parser URI
uriFromJSON = withText "URI" $ maybe (parseFail "invalid URI") pure . parseURI . T.unpack

uriToJSON :: URI -> Value
uriToJSON = String . T.pack . show

data PostTransactionsRequest = PostTransactionsRequest
  { version :: MarloweVersion
  , tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , invalidBefore :: Maybe UTCTime
  , invalidHereafter :: Maybe UTCTime
  , inputs :: [Semantics.Input]
  }
  deriving (Show, Eq, Generic)

instance FromJSON PostTransactionsRequest
instance ToJSON PostTransactionsRequest
instance ToSchema PostTransactionsRequest

data ApplyInputsTxEnvelope tx = ApplyInputsTxEnvelope
  { contractId :: TxOutRef
  , transactionId :: TxId
  , txEnvelope :: TextEnvelope
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON (ApplyInputsTxEnvelope CardanoTx) where
  toJSON ApplyInputsTxEnvelope{..} =
    object
      [ ("contractId", toJSON contractId)
      , ("transactionId", toJSON transactionId)
      , ("tx", toJSON txEnvelope)
      ]
instance ToJSON (ApplyInputsTxEnvelope CardanoTxBody) where
  toJSON ApplyInputsTxEnvelope{..} =
    object
      [ ("contractId", toJSON contractId)
      , ("transactionId", toJSON transactionId)
      , ("txBody", toJSON txEnvelope)
      ]

instance FromJSON (ApplyInputsTxEnvelope CardanoTx) where
  parseJSON = withObject "ApplyInputsTxEnvelope" \obj -> do
    contractId <- obj .: "contractId"
    transactionId <- obj .: "transactionId"
    txEnvelope <- obj .: "tx"
    pure ApplyInputsTxEnvelope{..}

instance FromJSON (ApplyInputsTxEnvelope CardanoTxBody) where
  parseJSON = withObject "ApplyInputsTxEnvelope" \obj -> do
    contractId <- obj .: "contractId"
    transactionId <- obj .: "transactionId"
    txEnvelope <- obj .: "txBody"
    pure ApplyInputsTxEnvelope{..}

instance ToSchema (ApplyInputsTxEnvelope CardanoTx) where
  declareNamedSchema _ = do
    contractIdSchema <- declareSchemaRef (Proxy :: Proxy TxOutRef)
    transactionIdSchema <- declareSchemaRef (Proxy :: Proxy TxId)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    return $
      NamedSchema (Just "ApplyInputsTxEnvelope") $
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "The \"type\" property of \"tx\" must be \"Tx BabbageEra\" or \"Tx ConwayEra\""
          & properties
            .~ [ ("contractId", contractIdSchema)
               , ("transactionId", transactionIdSchema)
               , ("tx", txEnvelopeSchema)
               ]
          & required .~ ["contractId", "transactionId", "tx"]

instance ToSchema (ApplyInputsTxEnvelope CardanoTxBody) where
  declareNamedSchema _ = do
    contractIdSchema <- declareSchemaRef (Proxy :: Proxy TxOutRef)
    transactionIdSchema <- declareSchemaRef (Proxy :: Proxy TxId)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    return $
      NamedSchema (Just "ApplyInputsTxEnvelope") $
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "The \"type\" property of \"txBody\" must be \"TxBody BabbageEra\" or \"TxBody ConwayEra\""
          & properties
            .~ [ ("contractId", contractIdSchema)
               , ("transactionId", transactionIdSchema)
               , ("txBody", txEnvelopeSchema)
               ]
          & required .~ ["contractId", "transactionId", "txBody"]

data NetworkId
  = Mainnet
  | Testnet Word32
  deriving (Show, Eq, Ord)

instance ToSchema NetworkId where
  declareNamedSchema _ = do
    let mainnetSchema =
          mempty
            & type_ ?~ OpenApiString
            & enum_ ?~ ["mainnet"]
    testnetSchema <- declareSchemaRef (Proxy @Word32)
    pure $
      NamedSchema (Just "NetworkId") $
        mempty
          & oneOf ?~ [Inline mainnetSchema, testnetSchema]

data ChainTip
  = ChainTipGenesis UTCTime
  | ChainTip BlockHeader UTCTime
  deriving (Show, Eq, Ord)

data RuntimeStatus = RuntimeStatus
  { nodeTip :: ChainTip
  , runtimeChainTip :: ChainTip
  , runtimeTip :: ChainTip
  , networkId :: NetworkId
  , runtimeVersion :: Version
  }
  deriving (Show, Eq, Ord)

instance ToJSON ChainTip where
  toJSON = \case
    ChainTipGenesis time -> object ["genesisTimeUTC" .= iso8601Show time]
    ChainTip blockHeader time ->
      object
        [ "blockHeader" .= blockHeader
        , "slotTimeUTC" .= iso8601Show time
        ]

instance FromJSON ChainTip where
  parseJSON = withObject "ChainTip" \obj -> do
    genesisTimeUTC <- obj .:? "genesisTimeUTC"
    blockHeader <- obj .:? "blockHeader"
    slotTimeUTC <- obj .:? "slotTimeUTC"
    case (genesisTimeUTC, blockHeader, slotTimeUTC) of
      (Nothing, Just blockHeader', Just slotTimeUTC') -> pure $ ChainTip blockHeader' slotTimeUTC'
      (Just genesisTimeUTC', Nothing, Nothing) -> pure $ ChainTipGenesis genesisTimeUTC'
      _ -> parseFail "Invalid keys, expecting ([\"genesisTimeUTC\"] | [\"blockHeader\", \"slotTimeUTC\"])"

instance ToHttpApiData ChainTip where
  toUrlPiece = TL.toStrict . encodeToLazyText

instance FromHttpApiData ChainTip where
  parseUrlPiece = first T.pack . eitherDecodeStrict . encodeUtf8

instance ToHttpApiData NetworkId where
  toUrlPiece = \case
    Mainnet -> "mainnet"
    Testnet n -> toUrlPiece n

instance ToHttpApiData Label where
  toUrlPiece = unLabel

instance FromHttpApiData Label where
  parseUrlPiece = pure . Label

instance FromHttpApiData NetworkId where
  parseUrlPiece = \case
    "mainnet" -> pure Mainnet
    n -> Testnet <$> parseUrlPiece n

instance ToParamSchema ChainTip where
  toParamSchema _ =
    mempty
      & oneOf ?~ [Inline genesisSchema, Inline tipSchema]
      & OpenApi.description ?~ "The latest known point in the chain on a peer."
    where
      genesisSchema =
        mempty
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("genesisTimeUTC", Inline $ toParamSchema $ Proxy @UTCTime)
               ]
          & required .~ ["genesisTimeUTC"]

      tipSchema =
        mempty
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("blockHeader", Ref $ Reference "BlockHeader")
               , ("slotTimeUTC", Inline $ toParamSchema $ Proxy @UTCTime)
               ]
          & required .~ ["blockHeader", "slotTimeUTC"]

instance ToParamSchema NetworkId where
  toParamSchema _ =
    mempty
      & oneOf ?~ [Inline (mempty & type_ ?~ OpenApiString), Inline (mempty & type_ ?~ OpenApiInteger)]
      & OpenApi.description ?~ "The latest known point in the chain on a peer."

data RoleTokenFilter
  = RoleTokenAnd RoleTokenFilter RoleTokenFilter
  | RoleTokenOr RoleTokenFilter RoleTokenFilter
  | RoleTokenNot RoleTokenFilter
  | RoleTokenFilterNone
  | RoleTokenFilterByContracts (Set TxOutRef)
  | RoleTokenFilterByPolicies (Set PolicyId)
  | RoleTokenFilterByTokens (Set AssetId)
  | RoleTokenFilterAny
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON RoleTokenFilter where
  toJSON = \case
    RoleTokenAnd a b -> object ["and" .= (a, b)]
    RoleTokenOr a b -> object ["or" .= (a, b)]
    RoleTokenNot a -> object ["not" .= a]
    RoleTokenFilterNone -> toJSON False
    RoleTokenFilterByContracts contracts -> object ["contract_id" .= contracts]
    RoleTokenFilterByPolicies policies -> object ["roles_currency" .= policies]
    RoleTokenFilterByTokens tokens -> object ["role_tokens" .= tokens]
    RoleTokenFilterAny -> toJSON True

instance FromJSON RoleTokenFilter where
  parseJSON =
    prependFailure "Parsing RoleTokenFilter failed" . \case
      Object o -> case KeyMap.toList o of
        [(k, v)] -> case k of
          "and" -> uncurry RoleTokenAnd <$> parseJSON v <?> Key "and"
          "or" -> uncurry RoleTokenOr <$> parseJSON v <?> Key "or"
          "not" -> RoleTokenNot <$> parseJSON v <?> Key "not"
          "contract_id" -> RoleTokenFilterByContracts <$> parseSetOrSingle v <?> Key "contract_id"
          "roles_currency" -> RoleTokenFilterByPolicies <$> parseSetOrSingle v <?> Key "roles_currency"
          "role_tokens" -> RoleTokenFilterByTokens <$> parseSetOrSingle v <?> Key "role_tokens"
          _ -> fail $ "Unexpected key: " <> show k
        _ -> fail "Unexpected number of keys, expected exactly 1."
      Bool True -> pure RoleTokenFilterAny
      Bool False -> pure RoleTokenFilterNone
      v -> typeMismatch "object|boolean" v

parseSetOrSingle :: (FromJSON a, Ord a) => Value -> Parser (Set a)
parseSetOrSingle = \case
  Array arr -> parseJSON $ Array arr
  v -> Set.singleton <$> parseJSON v

instance ToSchema RoleTokenFilter where
  declareNamedSchema _ = do
    roleTokenFilterSchema <- declareSchemaRef $ Proxy @RoleTokenFilter
    roleTokenFilterPairSchema <- declareSchemaRef $ Proxy @(RoleTokenFilter, RoleTokenFilter)
    let setOrSingleSchema
          :: forall a
           . (ToSchema a)
          => Proxy a
          -> Declare (Definitions Schema) (Referenced Schema)
        setOrSingleSchema p = do
          singleSchema <- declareSchemaRef p
          setSchema <- declareSchemaRef $ Proxy @(Set a)
          pure $ Inline $ mempty & oneOf ?~ [singleSchema, setSchema]
    txOutRefSchema <- setOrSingleSchema $ Proxy @TxOutRef
    policyIdSchema <- setOrSingleSchema $ Proxy @PolicyId
    assetIdSchema <- setOrSingleSchema $ Proxy @AssetId
    let andSchema =
          mempty
            & type_ ?~ OpenApiObject
            & OpenApi.description ?~ "Matches any role tokens matched by both sub-filters."
            & required .~ ["and"]
            & properties .~ [("and", roleTokenFilterPairSchema)]
        orSchema =
          mempty
            & type_ ?~ OpenApiObject
            & OpenApi.description ?~ "Matches any role tokens matched by either sub-filter."
            & required .~ ["or"]
            & properties .~ [("or", roleTokenFilterPairSchema)]
        notSchema =
          mempty
            & type_ ?~ OpenApiObject
            & OpenApi.description ?~ "Matches any role tokens not matched by the sub-filter."
            & required .~ ["not"]
            & properties .~ [("not", roleTokenFilterSchema)]
        anySchema =
          mempty
            & type_ ?~ OpenApiBoolean
            & OpenApi.description ?~ "Matches any role token."
            & enum_ ?~ [Bool True]
        noneSchema =
          mempty
            & type_ ?~ OpenApiBoolean
            & OpenApi.description ?~ "Matches no role token."
            & enum_ ?~ [Bool False]
        contractsSchema =
          mempty
            & type_ ?~ OpenApiObject
            & OpenApi.description ?~ "Matches any role tokens used by the given contract(s)."
            & required .~ ["contract_id"]
            & properties .~ [("contract_id", txOutRefSchema)]
        policiesSchema =
          mempty
            & type_ ?~ OpenApiObject
            & OpenApi.description ?~ "Matches any role tokens with the given currency symbol(s)."
            & required .~ ["roles_currency"]
            & properties .~ [("roles_currency", policyIdSchema)]
        tokensSchema =
          mempty
            & type_ ?~ OpenApiObject
            & OpenApi.description ?~ "Matches only the given role token(s)."
            & required .~ ["role_tokens"]
            & properties .~ [("role_tokens", assetIdSchema)]
    pure $
      NamedSchema (Just "RoleTokenFilter") $
        mempty
          & OpenApi.description ?~ "A filter that selects role tokens for burning."
          & oneOf
            ?~ fmap
              Inline
              [ andSchema
              , orSchema
              , notSchema
              , anySchema
              , noneSchema
              , contractsSchema
              , policiesSchema
              , tokensSchema
              ]
