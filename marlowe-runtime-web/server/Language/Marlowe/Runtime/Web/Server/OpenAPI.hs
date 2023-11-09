{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module defines the API and server for serving the Open API
-- specification.
module Language.Marlowe.Runtime.Web.Server.OpenAPI where

import Control.Applicative ((<|>))
import Control.Lens hiding (allOf, anyOf)
import qualified Control.Lens as Optics
import qualified Control.Monad as Control
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.List as List
import Data.OpenApi hiding (Server)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import Data.Version (showVersion)
import GHC.Exts (toList)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Language.Marlowe.Runtime.Web as Web
import qualified Paths_marlowe_runtime_web
import Servant
import Servant.OpenApi (toOpenApi)
import Servant.Pagination

instance ToParamSchema (Ranges fields resource) where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString

instance ToParamSchema (ContentRange fields resource) where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString

instance (KnownSymbolList fields) => ToParamSchema (AcceptRanges fields) where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiArray
      & items ?~ OpenApiItemsObject (Inline fieldsSchema)
    where
      fieldsSchema =
        mempty
          & type_ ?~ OpenApiString
          & enum_ ?~ (fromString <$> symbolListVal (Proxy @fields))

class KnownSymbolList (ss :: [Symbol]) where
  symbolListVal :: Proxy ss -> [String]

instance KnownSymbolList '[] where
  symbolListVal _ = []

instance (KnownSymbolList ss, KnownSymbol s) => KnownSymbolList (s ': ss) where
  symbolListVal _ = symbolVal (Proxy @s) : symbolListVal (Proxy @ss)

newtype OpenApiWithEmptySecurity = OpenApiWithEmptySecurity OpenApi

instance ToJSON OpenApiWithEmptySecurity where
  toJSON (OpenApiWithEmptySecurity oa) =
    oa
      & toJSON
      & key "paths" . members . members . atKey "security" %~ (<|> Just (Array mempty))

type API = "openapi.json" :> Get '[JSON] OpenApiWithEmptySecurity

data OpenApiLintIssue = OpenApiLintIssue
  { trace :: Text
  , message :: Text
  }
  deriving (Show, Eq)

lintOpenApi :: OpenApi -> [OpenApiLintIssue]
lintOpenApi oa = definitionLints
  where
    showStackTrace :: [Text] -> Text
    showStackTrace = Text.concat . List.intersperse "/" . List.reverse

    definitions :: [(Text, Schema)]
    definitions = toList $ Optics.view (components . schemas) oa

    schemaRule1Check :: [Text] -> Schema -> [OpenApiLintIssue]
    schemaRule1Check stackTrace s = do
      let schemaRequiredFields = Optics.view required s
      -- TODO: schemaType = Optics.view type_ s
      Control.when (null schemaRequiredFields) []
      let schemaProperties = Optics.view properties s
      -- TODO: schemaAnyOf = Optics.view anyOf s
      -- TODO: schemaOneOf = Optics.view oneOf s
      -- TODO: schemaAllOf = Optics.view allOf s
      schemaRequiredField <- schemaRequiredFields
      if IOHM.member schemaRequiredField schemaProperties
        then mempty
        else
          pure $
            OpenApiLintIssue
              { trace = showStackTrace stackTrace
              , message = "Missing type for required field '" <> schemaRequiredField <> "'!"
              }

    lintSchema :: [Text] -> Schema -> [OpenApiLintIssue]
    lintSchema = schemaRule1Check

    definitionLints :: [OpenApiLintIssue]
    definitionLints = do
      let stackTrace = ["schemas", "components"]
      (definitionName, definitionSchema) <- definitions
      lintSchema (definitionName : stackTrace) definitionSchema

openApi :: OpenApiWithEmptySecurity
openApi =
  OpenApiWithEmptySecurity $
    toOpenApi Web.api
      & info
        %~ (title .~ "Marlowe Runtime REST API")
          . (version .~ T.pack (showVersion Paths_marlowe_runtime_web.version))
          . (description ?~ "REST API for Marlowe Runtime")
          . ( license
                ?~ License
                  { _licenseName = "Apache 2.0"
                  , _licenseUrl = Just $ URL "https://www.apache.org/licenses/LICENSE-2.0.html"
                  }
            )
      & servers
        .~ [ "https://marlowe-runtime-preprod-web.scdev.aws.iohkdev.io"
           , "https://marlowe-runtime-preview-web.scdev.aws.iohkdev.io"
           , "https://marlowe-runtime-mainnet-web.scdev.aws.iohkdev.io"
           , "http://localhost:3780"
           ]

server :: (Applicative m) => ServerT API m
server = pure openApi
