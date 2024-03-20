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
module Language.Marlowe.Runtime.Web.Core.Roles (
  RolesConfig (..),
  RoleTokenConfig (..),
  RoleTokenRecipient (..),
  TokenMetadata (..),
  TokenMetadataFile (..),
  RoleTokenFilter (..),
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (.~), (?~))
import Control.Monad (unless)
import Data.Aeson (
  FromJSON (parseJSON),
  FromJSONKey (fromJSONKey),
  FromJSONKeyFunction (FromJSONKeyText),
  KeyValue ((.=)),
  ToJSON (toJSON),
  ToJSONKey (toJSONKey),
  Value (Array, Bool, Object, String),
  object,
  withObject,
  withText,
  (.:),
  (.:?),
  (<?>),
 )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AMap
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (JSONPathElement (..), Parser, prependFailure, toJSONKeyText, typeMismatch)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.OpenApi (
  AdditionalProperties (..),
  Definitions,
  HasAdditionalProperties (..),
  HasType (..),
  NamedSchema (..),
  OpenApiType (..),
  Referenced (..),
  Schema,
  ToSchema,
  declareSchemaRef,
  enum_,
  oneOf,
  properties,
  required,
 )
import qualified Data.OpenApi as OpenApi
import Data.OpenApi.Declare (Declare)
import Data.OpenApi.Schema (ToSchema (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Traversable (for)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Web.Adapter.URI (
  uriFromJSON,
  uriToJSON,
 )
import Language.Marlowe.Runtime.Web.Core.Address (Address (..))
import Language.Marlowe.Runtime.Web.Core.Asset (
  AssetId,
  PolicyId,
 )
import Language.Marlowe.Runtime.Web.Core.Metadata (Metadata)
import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()
import Language.Marlowe.Runtime.Web.Core.Tx (TxOutRef)
import Servant (
  Proxy (..),
  URI,
 )

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
            for
              mScript
              ( \case
                  "OpenRole" -> pure OpenRole
                  _ -> fail "Expected \'OpenRole\""
              )
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
  parseJSON =
    withObject
      "TokenMetadata"
      ( \obj -> do
          imageJSON <- obj .: "image"
          let additionalProps =
                AMap.delete "name"
                  . AMap.delete "image"
                  . AMap.delete "mediaType"
                  . AMap.delete "description"
                  . AMap.delete "files"
                  $ obj
          TokenMetadata
            <$> obj
              .: "name"
            <*> uriFromJSON imageJSON
            <*> obj
              .:? "mediaType"
            <*> obj
              .:? "description"
            <*> obj
              .:? "files"
            <*> pure additionalProps
      )

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
  parseJSON =
    withObject
      "TokenMetadataFile"
      ( \obj -> do
          srcJSON <- obj .: "src"
          let additionalProps =
                AMap.delete "name"
                  . AMap.delete "mediaType"
                  . AMap.delete "src"
                  $ obj
          TokenMetadataFile
            <$> obj
              .: "name"
            <*> uriFromJSON srcJSON
            <*> obj
              .: "mediaType"
            <*> pure additionalProps
      )

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
