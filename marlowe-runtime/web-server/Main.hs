{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
module Main
  where

import Control.Lens
import Data.OpenApi hiding (Server, server)
import Data.String (IsString(fromString))
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol, symbolVal)
import qualified Language.Marlowe.Runtime.Web as Web
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.OpenApi (HasOpenApi(toOpenApi))
import Servant.Pagination (AcceptRanges, ContentRange, Ranges)

main :: IO ()
main = run 8080 app

type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi

type API = SwaggerAPI :<|> Web.API

webApi :: Proxy Web.API
webApi = Proxy

api :: Proxy API
api = Proxy

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

openApi :: OpenApi
openApi = toOpenApi webApi

app :: Application
app = serve api server

server :: Server API
server = pure openApi :<|> error "not implemented"
