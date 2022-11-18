{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the request and response types in the Marlowe Runtime
-- | Web API.

module Language.Marlowe.Runtime.Web.Types
  where

import Control.Lens hiding ((.=))
import Control.Monad ((<=<))
import Data.Aeson
import Data.Aeson.Types (parseFail)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (decodeBase16, encodeBase16)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.OpenApi
  ( Definitions
  , HasType(..)
  , NamedSchema(..)
  , OpenApiType(..)
  , Referenced(Inline)
  , Schema
  , ToParamSchema
  , ToSchema
  , declareSchema
  , declareSchemaRef
  , description
  , enum_
  , example
  , pattern
  , properties
  , required
  , toParamSchema
  )
import Data.OpenApi.Declare (Declare)
import Data.OpenApi.Schema (ToSchema(..))
import Data.String (IsString(..))
import Data.Text (intercalate, splitOn)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import Data.Word (Word16, Word64)
import GHC.Base (Symbol)
import GHC.Exts (IsList(fromList))
import GHC.Generics (Generic)
import GHC.Show (showSpace)
import GHC.TypeLits (KnownSymbol, symbolVal)
import qualified Language.Marlowe.Core.V1.Semantics.Types as Semantics
import Language.Marlowe.Runtime.Web.Orphans ()
import Servant
import Servant.Pagination (HasPagination(..))

class HasNamedLink a api (name :: Symbol) where
  namedLink :: Proxy api -> Proxy name -> a -> Link

data WithLink (name :: Symbol) a where
  Include
    :: HasNamedLink a api name
    => Proxy api
    -> Proxy name
    -> a
    -> WithLink name a
  Omit :: a -> WithLink name a

deriving instance Typeable (WithLink name a)

instance (Show a, KnownSymbol name) => Show (WithLink name a) where
  showsPrec p (Include _ name a) = showParen (p >= 11)
    ( showString "Include _ (Proxy @"
    . showSpace
    . showsPrec 11 (symbolVal name)
    . showString ")"
    . showSpace
    . showsPrec 11 a
    )
  showsPrec p (Omit a) = showParen (p >= 11)
    ( showString "Omit"
    . showSpace
    . showsPrec 11 a
    )

class ToJSONWithLinks a where
  toJSONWithLinks :: a -> ([(String, Link)], Value)

instance {-# OVERLAPPING #-}
  ( ToJSONWithLinks a
  , KnownSymbol name
  ) => ToJSONWithLinks (WithLink name a) where
  toJSONWithLinks (Include api name a) = (link : links, value)
    where
      (links, value) = toJSONWithLinks a
      link = (symbolVal name, namedLink api name a)
  toJSONWithLinks (Omit a) = toJSONWithLinks a

instance {-# OVERLAPPING #-} ToJSON a => ToJSONWithLinks a where
  toJSONWithLinks a = ([], toJSON a)

instance
  ( ToJSONWithLinks a
  , KnownSymbol name
  ) => ToJSON (WithLink name a) where
  toJSON = toJSON' . toJSONWithLinks
    where
      toJSON' (links, value) = object
        [ "resource" .= value
        , "links" .= object (bimap fromString (toJSON . show . linkURI) <$> links)
        ]

instance HasPagination resource field => HasPagination (WithLink name resource) field where
  type RangeType (WithLink name resource) field = RangeType resource field
  getFieldValue p (Include _ _ resource) = getFieldValue p resource
  getFieldValue p (Omit resource) = getFieldValue p resource

class ToSchemaWithLinks a where
  declareNamedSchemaWithLinks :: Proxy a -> Declare (Definitions Schema) ([String], Referenced Schema)

instance {-# OVERLAPPING #-}
  ( ToSchemaWithLinks a
  , KnownSymbol name
  ) => ToSchemaWithLinks (WithLink name a) where
  declareNamedSchemaWithLinks _  = do
    (links, namedSchema) <- declareNamedSchemaWithLinks (Proxy @a)
    pure (symbolVal (Proxy @name) : links, namedSchema)

instance {-# OVERLAPPING #-} ToSchema a => ToSchemaWithLinks a where
  declareNamedSchemaWithLinks p = ([],) <$> declareSchemaRef p

instance
  ( Typeable a
  , ToSchemaWithLinks a
  , KnownSymbol name
  ) => ToSchema (WithLink name a) where
  declareNamedSchema _  = do
    (links, schema) <- declareNamedSchemaWithLinks (Proxy @(WithLink name a))
    stringSchema <- declareSchemaRef (Proxy @String)
    pure $ NamedSchema Nothing $ mempty
      & type_ ?~ OpenApiObject
      & required .~ ["resource", "links"]
      & properties .~
          [ ("resource", schema)
          , ( "links", Inline $ mempty
                & type_ ?~ OpenApiObject
                & properties .~ fromList ((,stringSchema) . fromString <$> links)
            )
          ]


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
    & description ?~ "The hex-encoded identifier of a Cardano transaction"
    & pattern ?~ "^[a-fA-F0-9]{64}$"

newtype PolicyId = PolicyId { unPolicyId :: ByteString }
  deriving (Eq, Ord, Generic)
  deriving (Show, ToHttpApiData, FromHttpApiData, ToJSON, FromJSON) via Base16

instance ToSchema PolicyId where
  declareNamedSchema _ = pure $ NamedSchema (Just "PolicyId") $ mempty
    & type_ ?~ OpenApiString
    & description ?~ "The hex-encoded minting policy ID for a native Cardano token"
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
    & description ?~ "A reference to a transaction output with a transaction ID and index."
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
    & description ?~ "A version of the Marlowe language."
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
    & description ?~ "An arbitrary JSON value for storage in a metadata key"

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
      & description ?~ "A header of the status of a transaction on the local node."

data BlockHeader = BlockHeader
  { slotNo :: Word64
  , blockNo :: Word64
  , blockHeaderHash :: Base16
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON BlockHeader
instance ToSchema BlockHeader
