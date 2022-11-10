{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Server
  where

import Flags
import qualified Language.Marlowe.Runtime.Web as Web
import Monad (AppM)
import qualified OpenAPI
import Servant
import qualified Web

type family API openAPIFlag where
  API Enabled = OpenAPI.API :<|> Web.API
  API Disabled = Web.API

api :: Flag openAPIFlag -> Proxy (API openAPIFlag)
api _ = Proxy

server :: Flag openAPIFlag -> ServerT (API openAPIFlag) AppM
server = \case
  Enabled -> OpenAPI.server :<|> Web.server
  Disabled -> Web.server
