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
import Servant hiding (Param)
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
lintOpenApi oa = schemaDefinitionLints <> pathParametersLints <> pathOperationLints
  where
    showStackTrace :: [Text] -> Text
    showStackTrace = Text.concat . List.intersperse "/" . List.reverse

    schemaDefinitions :: Definitions Schema
    schemaDefinitions = Optics.view (components . schemas) oa

    paramDefinitions :: Definitions Param
    paramDefinitions = Optics.view (components . parameters) oa

    schemaRef :: Referenced Schema -> Maybe Schema
    schemaRef = \case
      Inline s -> Just s
      Ref (Reference ((`IOHM.lookup` schemaDefinitions) -> s)) -> s

    paramRef :: Referenced Param -> Maybe Param
    paramRef = \case
      Inline s -> Just s
      Ref (Reference ((`IOHM.lookup` paramDefinitions) -> s)) -> s

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

      schemaRequiredField :: Text <- schemaRequiredFields

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

    lintParam :: [Text] -> Param -> [OpenApiLintIssue]
    lintParam stacktrace param = do
      s :: Schema <- Maybe.maybeToList (schemaRef =<< Optics.view schema param)
      lintSchema (Optics.view name param : stacktrace) s

    schemaDefinitionLints :: [OpenApiLintIssue]
    schemaDefinitionLints = do
      (definitionName, definitionSchema) <- toList schemaDefinitions
      lintSchema [definitionName, "schemas", "components"] definitionSchema

    pathParametersLints :: [OpenApiLintIssue]
    pathParametersLints = do
      (Text.pack -> path, endpoint) <- toList $ Optics.view paths oa
      param :: Param <- Maybe.mapMaybe paramRef $ Optics.view parameters endpoint
      lintParam ["parameters", path, "paths"] param

    -- DONE: path->operation*->param
    -- TODO: path->operation*->request
    -- TODO: path->operation*->response
    pathOperationLints :: [OpenApiLintIssue]
    pathOperationLints = do
      (Text.pack -> path, endpoint) <- toList $ Optics.view paths oa
      (operationName :: Text, operation :: Operation) <-
        zip
          ["get", "put", "post", "delete", "options", "head", "patch", "trace"]
          $ Maybe.catMaybes
            [ Optics.view get endpoint
            , Optics.view put endpoint
            , Optics.view post endpoint
            , Optics.view delete endpoint
            , Optics.view options endpoint
            , Optics.view head_ endpoint
            , Optics.view patch endpoint
            , Optics.view Data.OpenApi.trace endpoint
            ]
      param :: Param <- Maybe.mapMaybe paramRef $ Optics.view parameters operation
      lintParam ["parameters", operationName, path, "paths"] param

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
