{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Webserver where

import           Control.Monad.IO.Class       (liftIO)
import           Data.Proxy                   (Proxy (Proxy))
import           Marlowe.Run.Webserver.API    (API)
import           Marlowe.Run.Webserver.Server (AppConfig (..), WBEConfig (..), initializeServerContext)
import qualified Marlowe.Run.Webserver.Server as Server
import           Network.HTTP.Client          (defaultManagerSettings, newManager)
import           Network.Wai.Handler.Warp     as Warp
import           Servant                      (serve)
import           Servant.Client               (BaseUrl (BaseUrl, baseUrlHost, baseUrlPath, baseUrlPort, baseUrlScheme),
                                               Scheme (Http), mkClientEnv)

run :: FilePath -> Settings -> IO ()
run configPath settings = do
  appConfig <- initializeServerContext configPath
  let
    wbeHost = _wbeHost . getWbeConfig $ appConfig
    wbePort = _wbePort . getWbeConfig $ appConfig
    staticPath = getStaticPath appConfig

  manager <- liftIO $ newManager defaultManagerSettings

  let baseUrl = BaseUrl{baseUrlScheme=Http,baseUrlHost=wbeHost,baseUrlPort=wbePort,baseUrlPath=""}
      clientEnv = mkClientEnv manager baseUrl

  let server = Server.handlers
  Warp.runSettings settings (serve (Proxy @API) (server staticPath clientEnv))
