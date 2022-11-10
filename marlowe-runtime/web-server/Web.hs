module Web
  where

import Language.Marlowe.Runtime.Web (API)
import Servant

api :: Proxy API
api = Proxy

server :: Server API
server = error "not implemented"
