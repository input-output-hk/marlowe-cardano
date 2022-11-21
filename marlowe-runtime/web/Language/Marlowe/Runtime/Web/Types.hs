{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the request and response types in the Marlowe Runtime
-- | Web API.

module Language.Marlowe.Runtime.Web.Types
  where

import Control.Lens hiding ((.=))
import Control.Monad ((<=<))
import Data.Aeson
import Data.Aeson.KeyMap (toMap)
import Data.Aeson.Types (Parser, parseFail)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (decodeBase16, encodeBase16)
import Data.Char (isSpace)
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.OpenApi
  ( HasType(..)
  , NamedSchema(..)
  , OpenApiType(..)
  , ToParamSchema
  , ToSchema
  , declareSchema
  , enum_
  , example
  , pattern
  , toParamSchema
  )
import qualified Data.OpenApi as OpenApi
import Data.OpenApi.Schema (ToSchema(..))
import qualified Data.Set as Set
import Data.String (IsString(..))
import Data.Text (Text, intercalate, splitOn)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
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
  declareNamedSchema _ = pure $ NamedSchema (Just "TxId") $ mempty
    & type_ ?~ OpenApiString
    & OpenApi.description ?~ "The hex-encoded identifier of a Cardano transaction"
    & pattern ?~ "^[a-fA-F0-9]{64}$"

newtype Address = Address { unAddress :: T.Text }
  deriving (Eq, Ord, Generic)
  deriving newtype (Show, ToHttpApiData, FromHttpApiData, ToJSON, FromJSON)

instance ToSchema Address where
  declareNamedSchema _ = pure $ NamedSchema (Just "Address") $ mempty
    & type_ ?~ OpenApiString
    & OpenApi.description ?~ "A cardano address"
    & example ?~ "addr1w94f8ywk4fg672xasahtk4t9k6w3aql943uxz5rt62d4dvq8evxaf"

newtype CommaList a = CommaList { unCommaList :: [a] }
  deriving (Eq, Ord, Generic, Functor)
  deriving newtype (Show, ToJSON, FromJSON, IsList)

instance ToSchema a => ToSchema (CommaList a)

instance ToHttpApiData a => ToHttpApiData (CommaList a) where
  toUrlPiece = T.intercalate "," . fmap toUrlPiece . unCommaList
  toQueryParam = T.intercalate "," . fmap toQueryParam . unCommaList

instance FromHttpApiData a => FromHttpApiData (CommaList a) where
  parseUrlPiece = fmap CommaList
    . traverse (parseUrlPiece . T.dropWhileEnd isSpace . T.dropWhile isSpace)
    . T.splitOn ","
  parseQueryParam = fmap CommaList
    . traverse (parseQueryParam . T.dropWhileEnd isSpace . T.dropWhile isSpace)
    . T.splitOn ","

newtype PolicyId = PolicyId { unPolicyId :: ByteString }
  deriving (Eq, Ord, Generic)
  deriving (Show, ToHttpApiData, FromHttpApiData, ToJSON, FromJSON) via Base16

instance ToSchema PolicyId where
  declareNamedSchema _ = pure $ NamedSchema (Just "PolicyId") $ mempty
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
  , metadata :: Map Word64 Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  , initialContract :: Semantics.Contract
  , currentContract :: Semantics.Contract
  , state :: Maybe Semantics.State
  , utxo :: Maybe TxOutRef
  } deriving (Show, Eq, Generic)

instance ToJSON ContractState
instance ToSchema ContractState

data ContractHeader = ContractHeader
  { contractId :: TxOutRef
  , roleTokenMintingPolicyId :: PolicyId
  , version :: MarloweVersion
  , metadata :: Map Word64 Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON ContractHeader
instance ToSchema ContractHeader

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
  , status :: TxStatus
  , block :: Maybe BlockHeader
  , utxo :: Maybe TxOutRef
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON TxHeader
instance ToSchema TxHeader

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
instance ToSchema BlockHeader

data UnsignedCreateTx = UnsignedCreateTx
  { contractId :: TxOutRef
  , unsignedTx :: Base16
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON UnsignedCreateTx
instance ToSchema UnsignedCreateTx

data PostContractsRequest = PostContractsRequest
  { metadata :: Map Word64 Metadata
  , version :: MarloweVersion
  , roles :: Maybe RolesConfig
  , contract :: Semantics.Contract
  , minUTxODeposit :: Word64
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON PostContractsRequest
instance ToJSON PostContractsRequest

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

data TokenMetadata = TokenMetadata
  { name :: Text
  , image :: URI
  , mediaType :: Maybe Text
  , description :: Maybe Text
  , files :: Maybe [TokenMetadataFile]
  , otherProperties :: Map Key Metadata
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
      <*> pure (Metadata <$> Map.withoutKeys (toMap obj) (Set.fromList ["name", "image", "mediaType", "description", "files"]))

instance ToJSON TokenMetadata where
  toJSON TokenMetadata{..} = object $
    [ ("name", toJSON name)
    , ("image", uriToJSON image)
    , ("mediaType", toJSON mediaType)
    , ("description", toJSON description)
    , ("files", toJSON files)
    ] <> (fmap . fmap) unMetadata (Map.toList otherProperties)

data TokenMetadataFile = TokenMetadataFile
  { name :: Text
  , src :: URI
  , mediaType :: Text
  , otherProperties :: Map Key Metadata
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON TokenMetadataFile where
  parseJSON = withObject "TokenMetadataFile" \obj -> do
    srcJSON <- obj .: "src"
    TokenMetadataFile
      <$> obj .: "name"
      <*> uriFromJSON srcJSON
      <*> obj .: "mediaType"
      <*> pure (Metadata <$> Map.withoutKeys (toMap obj) (Set.fromList ["name", "src", "mediaType"]))

instance ToJSON TokenMetadataFile where
  toJSON TokenMetadataFile{..} = object $
    [ ("name", toJSON name)
    , ("src", uriToJSON src)
    , ("mediaType", toJSON mediaType)
    ] <> (fmap . fmap) unMetadata (Map.toList otherProperties)

uriFromJSON :: Value -> Parser URI
uriFromJSON = withText "URI" $ maybe (parseFail "invalid URI") pure . parseURI . T.unpack

uriToJSON :: URI -> Value
uriToJSON = String . T.pack . show
