{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OpenAPI
  where

import Control.Lens
import Data.OpenApi hiding (Server)
import Data.String (fromString)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant
import Servant.OpenApi (toOpenApi)
import Servant.Pagination
import qualified Web

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

type API = "swagger.json" :> Get '[JSON] OpenApi

server :: Applicative m => ServerT API m
server = pure $ toOpenApi Web.api
