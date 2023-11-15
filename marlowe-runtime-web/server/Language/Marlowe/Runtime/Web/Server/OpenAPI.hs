{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module defines the API and server for serving the Open API
-- specification.
module Language.Marlowe.Runtime.Web.Server.OpenAPI where

import Control.Applicative ((<|>))
import Control.Lens hiding (allOf, anyOf)
import qualified Control.Lens as Optics
import Control.Monad ((<=<))
import qualified Control.Monad as Control
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.List as List
import qualified Data.Maybe as Maybe
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

    definitions :: Definitions Schema
    definitions = Optics.view (components . schemas) oa

    schemaRef :: Referenced Schema -> Maybe Schema
    schemaRef = \case
      Inline s -> Just s
      Ref (Reference ((`IOHM.lookup` definitions) -> s)) -> s

    schemaRule1Check :: [Text] -> Schema -> [OpenApiLintIssue]
    schemaRule1Check stackTrace s = do
      let schemaRequiredFields :: [Text]
          schemaRequiredFields = Optics.view required s

          checkIfPropertyHaveTypedef :: Text -> Schema -> Maybe OpenApiType
          checkIfPropertyHaveTypedef fieldName ss =
            IOHM.lookup fieldName (Optics.view properties ss) >>= schemaRef >>= Optics.view type_

          schemaAnyOf :: Maybe [Referenced Schema]
          schemaAnyOf = Optics.view anyOf s

          schemaOneOf :: Maybe [Referenced Schema]
          schemaOneOf = Optics.view oneOf s

          schemaAllOf :: Maybe [Referenced Schema]
          schemaAllOf = Optics.view allOf s

      schemaRequiredField <- schemaRequiredFields
      let checkForType :: Referenced Schema -> Bool
          checkForType = Maybe.isJust . (checkIfPropertyHaveTypedef schemaRequiredField <=< schemaRef)

          typeIsInProperties = Maybe.isJust $ checkIfPropertyHaveTypedef schemaRequiredField s
          typeIsInEveryAnyOf = maybe False (and . fmap checkForType) schemaAnyOf
          typeIsInEveryOneOf = maybe False (and . fmap checkForType) schemaOneOf
          typeIsInOneAllOf = maybe False (or . fmap checkForType) schemaAllOf

      Control.when (typeIsInProperties || typeIsInEveryAnyOf || typeIsInEveryOneOf || typeIsInOneAllOf) []

      pure $
        OpenApiLintIssue
          { trace = showStackTrace stackTrace
          , message = "Missing type for required field '" <> schemaRequiredField <> "'!"
          }

    lintSchema :: [Text] -> Schema -> [OpenApiLintIssue]
    lintSchema = schemaRule1Check

    -- DONE:
    definitionLints :: [OpenApiLintIssue]
    definitionLints = do
      let stackTrace = ["schemas", "components"]
      (definitionName, definitionSchema) <- toList definitions
      lintSchema (definitionName : stackTrace) definitionSchema

-- TODO: path->param
-- TODO: path->operation*

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
