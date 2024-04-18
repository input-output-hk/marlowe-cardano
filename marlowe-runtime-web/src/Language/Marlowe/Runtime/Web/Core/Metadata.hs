{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Language.Marlowe.Runtime.Web.Core.Metadata (
  Metadata (..),
) where

import Control.DeepSeq (NFData)
import Control.Lens ((&), (?~))
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.OpenApi (
  AdditionalProperties (..),
  NamedSchema (..),
  OpenApiItems (..),
  OpenApiType (..),
  Reference (..),
  Referenced (..),
  ToSchema,
  declareSchemaRef,
  oneOf,
 )
import qualified Data.OpenApi as OpenApi
import Data.OpenApi.Schema (ToSchema (..))
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()
import Servant (
  Proxy (..),
 )

newtype Metadata = Metadata {unMetadata :: Value}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON)

instance NFData Metadata

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
