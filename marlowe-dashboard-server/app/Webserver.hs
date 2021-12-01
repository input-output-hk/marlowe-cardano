{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Webserver where

import           Data.Proxy                   (Proxy (Proxy))
import           Marlowe.Run.Webserver.API    (API)
import qualified Marlowe.Run.Webserver.Server as Server
import           Network.Wai.Handler.Warp     as Warp
import           Servant                      (serve)

run :: FilePath -> Settings -> IO ()
run staticPath settings = do
  let server = Server.handlers
  Warp.runSettings settings (serve (Proxy @API) (server staticPath))
