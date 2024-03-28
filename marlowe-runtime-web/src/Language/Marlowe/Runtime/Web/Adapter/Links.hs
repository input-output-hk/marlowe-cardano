{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.Marlowe.Runtime.Web.Adapter.Links (
  WithLink (..),
  retractLink,
  HasLinkParser (..),
  ToJSONWithLinks (..),
  FromJSONWithLinks (..),
  ToSchemaWithLinks (..),
) where

import Data.Data (Proxy, Typeable)
import Data.OpenApi (
  Definitions,
  NamedSchema (..),
  OpenApiType (..),
  Referenced (..),
  Schema,
  ToSchema,
  declareSchemaRef,
  properties,
  required,
  type_,
 )
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.Marlowe.Runtime.Web.Contract.Next.Schema ()

import Control.Lens ((&), (.~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value)
import qualified Data.Aeson.Types as A
import Data.OpenApi.Declare (Declare)
import Data.OpenApi.Schema (ToSchema (..))
import GHC.Exts (IsList (fromList), IsString (fromString))
import GHC.Show (showSpace)
import Servant (
  HasLink (..),
  Link,
  Proxy (..),
 )
import Servant.Pagination (
  HasPagination (RangeType, getFieldValue),
 )
import Text.Parsec.String (Parser)

data WithLink (name :: Symbol) a where
  IncludeLink :: Proxy name -> a -> WithLink name a
  OmitLink :: a -> WithLink name a

class ToJSONWithLinks a where
  toJSONWithLinks :: a -> ([(String, Link)], Value)

class FromJSONWithLinks a where
  fromJSONWithLinks :: ([(String, String)], Value) -> A.Parser a

instance {-# OVERLAPPING #-} (FromJSON a) => FromJSONWithLinks a where
  fromJSONWithLinks = parseJSON . snd

class ToSchemaWithLinks a where
  declareNamedSchemaWithLinks :: Proxy a -> Declare (Definitions Schema) ([String], Referenced Schema)

class (HasLink endpoint) => HasLinkParser endpoint where
  linkParser :: Bool -> Proxy endpoint -> Parser (MkLink endpoint a -> a)

instance (HasPagination resource field) => HasPagination (WithLink name resource) field where
  type RangeType (WithLink name resource) field = RangeType resource field
  getFieldValue p (IncludeLink _ resource) = getFieldValue p resource
  getFieldValue p (OmitLink resource) = getFieldValue p resource

instance {-# OVERLAPPING #-} (ToJSON a) => ToJSONWithLinks a where
  toJSONWithLinks a = ([], toJSON a)

retractLink :: WithLink name a -> a
retractLink (IncludeLink _ a) = a
retractLink (OmitLink a) = a

deriving instance Typeable (WithLink name a)

instance {-# OVERLAPPING #-} (ToSchema a) => ToSchemaWithLinks a where
  declareNamedSchemaWithLinks p = ([],) <$> declareSchemaRef p

instance
  {-# OVERLAPPING #-}
  ( ToSchemaWithLinks a
  , KnownSymbol name
  )
  => ToSchemaWithLinks (WithLink name a)
  where
  declareNamedSchemaWithLinks _ = do
    (links, namedSchema) <- declareNamedSchemaWithLinks (Proxy @a)
    pure (symbolVal (Proxy @name) : links, namedSchema)

instance
  ( Typeable a
  , ToSchemaWithLinks a
  , KnownSymbol name
  )
  => ToSchema (WithLink name a)
  where
  declareNamedSchema _ = do
    (links, schema) <- declareNamedSchemaWithLinks (Proxy @(WithLink name a))
    stringSchema <- declareSchemaRef (Proxy @String)
    pure $
      NamedSchema Nothing $
        mempty
          & type_ ?~ OpenApiObject
          & required .~ ["resource", "links"]
          & properties
            .~ [ ("resource", schema)
               ,
                 ( "links"
                 , Inline $
                    mempty
                      & type_ ?~ OpenApiObject
                      & properties .~ fromList ((,stringSchema) . fromString <$> links)
                 )
               ]

instance (Show a, KnownSymbol name) => Show (WithLink name a) where
  showsPrec p (IncludeLink name a) =
    showParen
      (p >= 11)
      ( showString "IncludeLink (Proxy @"
          . showSpace
          . showsPrec 11 (symbolVal name)
          . showString ")"
          . showSpace
          . showsPrec 11 a
      )
  showsPrec p (OmitLink a) =
    showParen
      (p >= 11)
      ( showString "OmitLink"
          . showSpace
          . showsPrec 11 a
      )
