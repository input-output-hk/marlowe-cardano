{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module defines the API and server for serving the Open API
-- specification.

module Language.Marlowe.Runtime.Web.Server.OpenAPI
  where

import Control.Lens
import Data.OpenApi hiding (Server)
import Data.String (fromString)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Language.Marlowe.Runtime.Web as Web
import Servant
import Servant.OpenApi (toOpenApi)
import Servant.Pagination

instance ToParamSchema (Ranges fields resource) where
  toParamSchema _ = mempty
    & type_ ?~ OpenApiString

instance ToParamSchema (ContentRange fields resource) where
  toParamSchema _ = mempty
    & type_ ?~ OpenApiString

instance KnownSymbolList fields => ToParamSchema (AcceptRanges fields) where
  toParamSchema _ = mempty
    & type_ ?~ OpenApiArray
    & items ?~ OpenApiItemsObject (Inline fieldsSchema)
    where
      fieldsSchema = mempty
        & type_ ?~ OpenApiString
        & enum_ ?~ (fromString <$> symbolListVal (Proxy @fields))

class KnownSymbolList (ss :: [Symbol]) where
  symbolListVal :: Proxy ss -> [String]

instance KnownSymbolList '[] where
  symbolListVal _ = []

instance (KnownSymbolList ss, KnownSymbol s) => KnownSymbolList (s ': ss) where
  symbolListVal _ = symbolVal (Proxy @s) : symbolListVal (Proxy @ss)

type API = "openapi.json" :> Get '[JSON] OpenApi

server :: Applicative m => ServerT API m
server = pure $ toOpenApi Web.api
