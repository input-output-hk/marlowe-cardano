{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module defines the API and server for serving the Open API
-- specification.
module Language.Marlowe.Runtime.Web.Server.OpenAPI where

import Control.Lens
import Data.OpenApi hiding (Server)
import Data.String (fromString)
import qualified Data.Text as T
import Data.Version (showVersion)
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

type API = "openapi.json" :> Get '[JSON] OpenApi

server :: (Applicative m) => ServerT API m
server =
  pure $
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
