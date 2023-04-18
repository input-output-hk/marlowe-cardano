{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the request and response types in the Marlowe Runtime
-- | Web API.

module Language.Marlowe.Runtime.Web.Types
  where

import Control.Lens hiding ((.=))
import Control.Monad ((<=<))
import Data.Aeson
import Data.Aeson.Types (Parser, parseFail)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (decodeBase16, encodeBase16)
import Data.Char (isSpace)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.OpenApi
  ( HasType(..)
  , NamedSchema(..)
  , OpenApiType(..)
  , Referenced(..)
  , ToParamSchema
  , ToSchema
  , declareSchema
  , declareSchemaRef
  , enum_
  , example
  , oneOf
  , pattern
  , properties
  , required
  , toParamSchema
  )
import qualified Data.OpenApi as OpenApi
import Data.OpenApi.Schema (ToSchema(..))
import Data.Set (Set)
import Data.String (IsString(..))
import Data.Text (Text, intercalate, splitOn)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.Word (Word16, Word64)
import GHC.Exts (IsList)
import GHC.Generics (Generic)
import qualified Language.Marlowe.Core.V1.Semantics.Types as Semantics
import Language.Marlowe.Runtime.Web.Orphans ()
import Network.URI (parseURI)
import Servant
import Servant.Pagination (HasPagination(..))

-- | A newtype for Base16 decoding and encoding ByteStrings
newtype Base16 = Base16 { unBase16 :: ByteString }
  deriving (Eq, Ord)

instance Show Base16 where
  show = T.unpack . encodeBase16 . unBase16

instance IsString Base16 where
  fromString = either (error . T.unpack) Base16 . decodeBase16 . encodeUtf8 . T.pack

instance ToJSON Base16 where
  toJSON = String . toUrlPiece

instance FromJSON Base16 where
  parseJSON =
    withText "Base16" $ either (parseFail . T.unpack) pure . parseUrlPiece

instance ToHttpApiData Base16 where
  toUrlPiece = encodeBase16 . unBase16

instance FromHttpApiData Base16 where
  parseUrlPiece = fmap Base16 . decodeBase16 . encodeUtf8

instance ToSchema Base16 where
  declareNamedSchema _ = NamedSchema Nothing <$> declareSchema (Proxy @String)

newtype TxId = TxId { unTxId :: ByteString }
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
  toParamSchema _ = mempty
    & type_ ?~ OpenApiString
    & OpenApi.description ?~ "The hex-encoded identifier of a Cardano transaction"
    & pattern ?~ "^[a-fA-F0-9]{64}$"

newtype Address = Address { unAddress :: T.Text }
  deriving (Eq, Ord, Generic)
  deriving newtype (Show, ToHttpApiData, FromHttpApiData, ToJSON, FromJSON)

instance ToSchema Address where
  declareNamedSchema = pure . NamedSchema (Just "Address") . toParamSchema

instance ToParamSchema Address where
  toParamSchema _ = mempty
    & type_ ?~ OpenApiString
    & OpenApi.description ?~ "A cardano address"
    & example ?~ "addr1w94f8ywk4fg672xasahtk4t9k6w3aql943uxz5rt62d4dvq8evxaf"

newtype CommaList a = CommaList { unCommaList :: [a] }
  deriving (Eq, Ord, Generic, Functor)
  deriving newtype (Show, ToJSON, FromJSON, IsList)

instance ToParamSchema (CommaList a) where
  toParamSchema _ = mempty
    & type_ ?~ OpenApiString
    & OpenApi.description ?~ "A comma-separated list of values"

instance ToSchema a => ToSchema (CommaList a)

instance ToHttpApiData a => ToHttpApiData (CommaList a) where
  toUrlPiece = T.intercalate "," . fmap toUrlPiece . unCommaList
  toQueryParam = T.intercalate "," . fmap toQueryParam . unCommaList

instance FromHttpApiData a => FromHttpApiData (CommaList a) where
  parseUrlPiece = fmap CommaList
    . traverse (parseUrlPiece . T.dropWhileEnd isSpace . T.dropWhile isSpace)
    . splitOnNonEmpty ","
  parseQueryParam = fmap CommaList
    . traverse (parseQueryParam . T.dropWhileEnd isSpace . T.dropWhile isSpace)
    . splitOnNonEmpty ","

splitOnNonEmpty :: Text -> Text -> [Text]
splitOnNonEmpty sep t
  | T.null t = []
  | otherwise = T.splitOn sep t

newtype PolicyId = PolicyId { unPolicyId :: ByteString }
  deriving (Eq, Ord, Generic)
  deriving (Show, ToHttpApiData, FromHttpApiData, ToJSON, FromJSON) via Base16

instance ToSchema PolicyId where
  declareNamedSchema proxy = pure $ NamedSchema (Just "PolicyId") $ toParamSchema proxy

instance ToParamSchema PolicyId where
  toParamSchema _ = mempty
    & type_ ?~ OpenApiString
    & OpenApi.description ?~ "The hex-encoded minting policy ID for a native Cardano token"
    & pattern ?~ "^[a-fA-F0-9]*$"

data TxOutRef = TxOutRef
  { txId :: TxId
  , txIx :: Word16
  } deriving (Show, Eq, Ord, Generic)

instance FromHttpApiData TxOutRef where
  parseUrlPiece t = case splitOn "#" t of
    [idText, ixText] -> TxOutRef <$> parseUrlPiece idText <*> parseUrlPiece ixText
    _ -> Left "Expected [a-fA-F0-9]{64}#[0-9]+"

instance ToHttpApiData TxOutRef where
  toUrlPiece TxOutRef{..} = toUrlPiece txId <> "#" <> toUrlPiece txIx

instance FromJSON TxOutRef where
  parseJSON =
    withText "TxOutRef" $ either (parseFail . T.unpack) pure . parseUrlPiece

instance ToSchema TxOutRef where
  declareNamedSchema proxy = pure $ NamedSchema (Just "TxOutRef") $ toParamSchema proxy

instance ToParamSchema TxOutRef where
  toParamSchema _ = mempty
    & type_ ?~ OpenApiString
    & OpenApi.description ?~ "A reference to a transaction output with a transaction ID and index."
    & pattern ?~ "^[a-fA-F0-9]{64}#[0-9]+$"
    & example ?~ "98d601c9307dd43307cf68a03aad0086d4e07a789b66919ccf9f7f7676577eb7#1"

instance ToJSON TxOutRef where
  toJSON = String . toUrlPiece

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
  parseUrlPiece _ = Left $ fold @[]
    [ "expected one of "
    , intercalate "; " ["v1"]
    ]

instance ToSchema MarloweVersion where
  declareNamedSchema _ = pure $ NamedSchema (Just "MarloweVersion") $ mempty
    & type_ ?~ OpenApiString
    & OpenApi.description ?~ "A version of the Marlowe language."
    & enum_ ?~ ["v1"]

data ContractState = ContractState
  { contractId :: TxOutRef
  , roleTokenMintingPolicyId :: PolicyId
  , version :: MarloweVersion
  , continuations :: Maybe Text
  , tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  , initialContract :: Semantics.Contract
  , currentContract :: Maybe Semantics.Contract
  , state :: Maybe Semantics.State
  , utxo :: Maybe TxOutRef
  , txBody :: Maybe TextEnvelope
  } deriving (Show, Eq, Generic)

instance ToJSON ContractState
instance FromJSON ContractState
instance ToSchema ContractState

data ContractHeader = ContractHeader
  { contractId :: TxOutRef
  , roleTokenMintingPolicyId :: PolicyId
  , version :: MarloweVersion
  , continuations :: Maybe Text
  , tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON ContractHeader
instance FromJSON ContractHeader
instance ToSchema ContractHeader

data WithdrawalHeader = WithdrawalHeader
  { withdrawalId :: TxId
  , status :: TxStatus
  , block :: Maybe BlockHeader
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON WithdrawalHeader
instance FromJSON WithdrawalHeader
instance ToSchema WithdrawalHeader

instance HasPagination WithdrawalHeader "withdrawalId" where
  type RangeType WithdrawalHeader "withdrawalId" = TxId
  getFieldValue _ WithdrawalHeader{..} = withdrawalId

data PayoutRef = PayoutRef
  { contractId :: TxOutRef
  , payout :: TxOutRef
  , roleTokenMintingPolicyId :: PolicyId
  , role :: Text
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON PayoutRef
instance FromJSON PayoutRef
instance ToSchema PayoutRef

data Withdrawal = Withdrawal
  { payouts :: Set PayoutRef
  , withdrawalId :: TxId
  , status :: TxStatus
  , block :: Maybe BlockHeader
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON Withdrawal
instance FromJSON Withdrawal
instance ToSchema Withdrawal

instance HasPagination ContractHeader "contractId" where
  type RangeType ContractHeader "contractId" = TxOutRef
  getFieldValue _ ContractHeader{..} = contractId

newtype Metadata = Metadata { unMetadata :: Value }
  deriving (Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

instance ToSchema Metadata where
  declareNamedSchema _ = pure $ NamedSchema (Just "Metadata") $ mempty
    & OpenApi.description ?~ "An arbitrary JSON value for storage in a metadata key"

data TxHeader = TxHeader
  { contractId :: TxOutRef
  , transactionId :: TxId
  , continuations :: Maybe Text
  , tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  , utxo :: Maybe TxOutRef
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON TxHeader
instance FromJSON TxHeader
instance ToSchema TxHeader

data Tx = Tx
  { contractId :: TxOutRef
  , transactionId :: TxId
  , continuations :: Maybe Text
  , tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  , inputUtxo :: TxOutRef
  , inputs :: [Semantics.Input]
  , outputUtxo :: Maybe TxOutRef
  , outputContract :: Maybe Semantics.Contract
  , outputState :: Maybe Semantics.State
  , consumingTx :: Maybe TxId
  , invalidBefore :: UTCTime
  , invalidHereafter :: UTCTime
  , txBody :: Maybe TextEnvelope
  } deriving (Show, Eq, Generic)

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
  declareNamedSchema _ = pure
    $ NamedSchema (Just "TxStatusHeader")
    $ mempty
      & type_ ?~ OpenApiString
      & enum_ ?~ ["unsigned", "submitted", "confirmed"]
      & OpenApi.description ?~ "A header of the status of a transaction on the local node."

data BlockHeader = BlockHeader
  { slotNo :: Word64
  , blockNo :: Word64
  , blockHeaderHash :: Base16
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON BlockHeader
instance FromJSON BlockHeader
instance ToSchema BlockHeader

data CardanoTx
data CardanoTxBody

data WithdrawTxEnvelope tx = WithdrawTxEnvelope
  { withdrawalId :: TxId
  , txEnvelope :: TextEnvelope
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON (WithdrawTxEnvelope CardanoTx) where
  toJSON WithdrawTxEnvelope{..} = object
    [ ("withdrawalId", toJSON withdrawalId)
    , ("tx", toJSON txEnvelope)
    ]
instance ToJSON (WithdrawTxEnvelope CardanoTxBody) where
  toJSON WithdrawTxEnvelope{..} = object
    [ ("withdrawalId", toJSON withdrawalId)
    , ("txBody", toJSON txEnvelope)
    ]

instance FromJSON (WithdrawTxEnvelope CardanoTx) where
  parseJSON = withObject "WithdrawTxEnvelope" \obj -> WithdrawTxEnvelope
    <$> obj .: "withdrawalId"
    <*> obj .: "tx"

instance FromJSON (WithdrawTxEnvelope CardanoTxBody) where
  parseJSON = withObject "WithdrawTxEnvelope" \obj -> WithdrawTxEnvelope
    <$> obj .: "withdrawalId"
    <*> obj .: "txBody"

instance ToSchema (WithdrawTxEnvelope CardanoTx) where
  declareNamedSchema _ = do
    withdrawalIdSchema <- declareSchemaRef (Proxy :: Proxy TxId)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    return $ NamedSchema (Just "ApplyInputsTxEnvelope") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~
          [ ("withdrawalId", withdrawalIdSchema)
          , ("tx", txEnvelopeSchema)
          ]
      & required .~ [ "withdrawalId", "tx" ]

instance ToSchema (WithdrawTxEnvelope CardanoTxBody) where
  declareNamedSchema _ = do
    withdrawalIdSchema <- declareSchemaRef (Proxy :: Proxy TxId)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    return $ NamedSchema (Just "ApplyInputsTxEnvelope") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~
          [ ("withdrawalId", withdrawalIdSchema)
          , ("txBody", txEnvelopeSchema)
          ]
      & required .~ [ "withdrawalId", "txBody" ]

data CreateTxEnvelope tx = CreateTxEnvelope
  { contractId :: TxOutRef
  , txEnvelope :: TextEnvelope
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON (CreateTxEnvelope CardanoTx) where
  toJSON CreateTxEnvelope{..} = object
    [ ("contractId", toJSON contractId)
    , ("tx", toJSON txEnvelope)
    ]
instance ToJSON (CreateTxEnvelope CardanoTxBody) where
  toJSON CreateTxEnvelope{..} = object
    [ ("contractId", toJSON contractId)
    , ("txBody", toJSON txEnvelope)
    ]

instance FromJSON (CreateTxEnvelope CardanoTx) where
  parseJSON = withObject "CreateTxEnvelope" \obj -> CreateTxEnvelope
    <$> obj .: "contractId"
    <*> obj .: "tx"

instance FromJSON (CreateTxEnvelope CardanoTxBody) where
  parseJSON = withObject "CreateTxEnvelope" \obj -> CreateTxEnvelope
    <$> obj .: "contractId"
    <*> obj .: "txBody"

instance ToSchema (CreateTxEnvelope CardanoTx) where
  declareNamedSchema _ = do
    contractIdSchema <- declareSchemaRef (Proxy :: Proxy TxOutRef)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    return $ NamedSchema (Just "ApplyInputsTxEnvelope") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~
          [ ("contractId", contractIdSchema)
          , ("tx", txEnvelopeSchema)
          ]
      & required .~ [ "contractId", "tx" ]

instance ToSchema (CreateTxEnvelope CardanoTxBody) where
  declareNamedSchema _ = do
    contractIdSchema <- declareSchemaRef (Proxy :: Proxy TxOutRef)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    return $ NamedSchema (Just "ApplyInputsTxEnvelope") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~
          [ ("contractId", contractIdSchema)
          , ("txBody", txEnvelopeSchema)
          ]
      & required .~ [ "contractId", "txBody" ]

data TextEnvelope = TextEnvelope
  { teType :: Text
  , teDescription :: Text
  , teCborHex :: Base16
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON TextEnvelope where
  toJSON TextEnvelope{..} = object
    [ ("type", toJSON teType)
    , ("description", toJSON teDescription)
    , ("cborHex", toJSON teCborHex)
    ]

instance FromJSON TextEnvelope where
  parseJSON = withObject "TextEnvelope" \obj -> TextEnvelope
    <$> obj .: "type"
    <*> obj .: "description"
    <*> obj .: "cborHex"

instance ToSchema TextEnvelope where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy @Text)
    pure $ NamedSchema (Just "TextEnvelope") $ mempty
      & type_ ?~ OpenApiObject
      & required .~ ["type", "description", "cborHex"]
      & properties .~
          [ ("type", textSchema)
          , ("description", textSchema)
          , ("cborHex", textSchema)
          ]

data PostWithdrawalsRequest = PostWithdrawalsRequest
  { role :: Text
  , contractId :: TxOutRef
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON PostWithdrawalsRequest
instance ToJSON PostWithdrawalsRequest
instance ToSchema PostWithdrawalsRequest

data PostContractsRequest = PostContractsRequest
  { tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , version :: MarloweVersion
  , roles :: Maybe RolesConfig
  , contract :: Semantics.Contract
  , minUTxODeposit :: Word64
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON PostContractsRequest
instance ToJSON PostContractsRequest
instance ToSchema PostContractsRequest

data RolesConfig
  = UsePolicy PolicyId
  | Mint (Map Text RoleTokenConfig)
  deriving (Show, Eq, Ord, Generic)

instance FromJSON RolesConfig where
  parseJSON (String s) = UsePolicy <$> parseJSON (String s)
  parseJSON value = Mint <$> parseJSON value

instance ToJSON RolesConfig where
  toJSON (UsePolicy policy) = toJSON policy
  toJSON (Mint configs) = toJSON configs

instance ToSchema RolesConfig where
  declareNamedSchema _ = do
    policySchema <- declareSchemaRef (Proxy @PolicyId)
    mintSchema <- declareSchemaRef (Proxy @(Map Text RoleTokenConfig))
    pure $ NamedSchema (Just "RolesConfig") $ mempty
      & oneOf ?~ [policySchema, mintSchema]

data RoleTokenConfig
  = RoleTokenSimple Address
  | RoleTokenAdvanced Address TokenMetadata
  deriving (Show, Eq, Ord, Generic)

instance FromJSON RoleTokenConfig where
  parseJSON (String s) = pure $ RoleTokenSimple $ Address s
  parseJSON value = withObject
    "RoleTokenConfig"
    (\obj -> RoleTokenAdvanced <$> obj .: "address" <*> obj .: "metadata")
    value

instance ToJSON RoleTokenConfig where
  toJSON (RoleTokenSimple address) = toJSON address
  toJSON (RoleTokenAdvanced address config) = object
    [ ("address", toJSON address)
    , ("metadata", toJSON config)
    ]

instance ToSchema RoleTokenConfig where
  declareNamedSchema _ = do
    simpleSchema <- declareSchemaRef (Proxy @Address)
    metadataSchema <- declareSchemaRef (Proxy @TokenMetadata)
    let
      advancedSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ ["address", "metadata"]
        & properties .~
            [ ("address", simpleSchema)
            , ("metadata", metadataSchema)
            ]
    pure $ NamedSchema (Just "RoleTokenConfig") $ mempty
      & oneOf ?~ [simpleSchema, Inline advancedSchema]

data TokenMetadata = TokenMetadata
  { name :: Text
  , image :: URI
  , mediaType :: Maybe Text
  , description :: Maybe Text
  , files :: Maybe [TokenMetadataFile]
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON TokenMetadata where
  parseJSON = withObject "TokenMetadata" \obj -> do
    imageJSON <- obj .: "image"
    TokenMetadata
      <$> obj .: "name"
      <*> uriFromJSON imageJSON
      <*> obj .:? "mediaType"
      <*> obj .:? "description"
      <*> obj .:? "files"

instance ToJSON TokenMetadata where
  toJSON TokenMetadata{..} = object
    [ ("name", toJSON name)
    , ("image", uriToJSON image)
    , ("mediaType", toJSON mediaType)
    , ("description", toJSON description)
    , ("files", toJSON files)
    ]

instance ToSchema TokenMetadata where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (Proxy @Text)
    filesSchema <- declareSchemaRef (Proxy @[TokenMetadataFile])
    pure $ NamedSchema (Just "TokenMetadata") $ mempty
      & type_ ?~ OpenApiObject
      & OpenApi.description ?~ "Metadata for an NFT, as described by https://cips.cardano.org/cips/cip25/"
      & required .~ ["name", "image"]
      & properties .~
          [ ("name", stringSchema)
          , ("image", stringSchema)
          , ("mediaType", stringSchema)
          , ("description", stringSchema)
          , ("files", filesSchema)
          ]

data TokenMetadataFile = TokenMetadataFile
  { name :: Text
  , src :: URI
  , mediaType :: Text
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON TokenMetadataFile where
  parseJSON = withObject "TokenMetadataFile" \obj -> do
    srcJSON <- obj .: "src"
    TokenMetadataFile
      <$> obj .: "name"
      <*> uriFromJSON srcJSON
      <*> obj .: "mediaType"

instance ToJSON TokenMetadataFile where
  toJSON TokenMetadataFile{..} = object
    [ ("name", toJSON name)
    , ("src", uriToJSON src)
    , ("mediaType", toJSON mediaType)
    ]

instance ToSchema TokenMetadataFile where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (Proxy @Text)
    pure $ NamedSchema (Just "TokenMetadataFile") $ mempty
      & type_ ?~ OpenApiObject
      & required .~ ["name", "src", "mediaType"]
      & properties .~
          [ ("name", stringSchema)
          , ("src", stringSchema)
          , ("mediaType", stringSchema)
          ]

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
  } deriving (Show, Eq, Generic)

instance FromJSON PostTransactionsRequest
instance ToJSON PostTransactionsRequest
instance ToSchema PostTransactionsRequest

data ApplyInputsTxEnvelope tx = ApplyInputsTxEnvelope
  { contractId :: TxOutRef
  , transactionId :: TxId
  , txEnvelope :: TextEnvelope
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON (ApplyInputsTxEnvelope CardanoTx) where
  toJSON ApplyInputsTxEnvelope{..} = object
    [ ("contractId", toJSON contractId)
    , ("transactionId", toJSON transactionId)
    , ("tx", toJSON txEnvelope)
    ]
instance ToJSON (ApplyInputsTxEnvelope CardanoTxBody) where
  toJSON ApplyInputsTxEnvelope{..} = object
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
    return $ NamedSchema (Just "ApplyInputsTxEnvelope") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~
          [ ("contractId", contractIdSchema)
          , ("transactionId", transactionIdSchema)
          , ("tx", txEnvelopeSchema)
          ]
      & required .~ [ "contractId", "transactionId", "tx" ]

instance ToSchema (ApplyInputsTxEnvelope CardanoTxBody) where
  declareNamedSchema _ = do
    contractIdSchema <- declareSchemaRef (Proxy :: Proxy TxOutRef)
    transactionIdSchema <- declareSchemaRef (Proxy :: Proxy TxId)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    return $ NamedSchema (Just "ApplyInputsTxEnvelope") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~
          [ ("contractId", contractIdSchema)
          , ("transactionId", transactionIdSchema)
          , ("txBody", txEnvelopeSchema)
          ]
      & required .~ [ "contractId", "transactionId", "txBody" ]
