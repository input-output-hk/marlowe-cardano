{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Webserver where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Marlowe.Run.API (API)
import qualified Marlowe.Run.Server as Server
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp as Warp
import Servant (serve)
import Servant.Client (BaseUrl (BaseUrl, baseUrlHost, baseUrlPath, baseUrlPort, baseUrlScheme), Scheme (Http),
                       mkClientEnv)

normalizeFieldLabel :: String -> String -> String
normalizeFieldLabel prefix label =
  maybe label lower1 $ stripPrefix ('_' : prefix) label

lower1 :: String -> String
lower1 (c:cs) = toLower c : cs
lower1 []     = []

data WBEConfig = WBEConfig { _wbeHost :: String, _wbePort :: Int }
    deriving (Eq, Generic, Show)

instance Aeson.FromJSON WBEConfig where
    parseJSON = Aeson.genericParseJSON $ Aeson.defaultOptions
        { Aeson.fieldLabelModifier = normalizeFieldLabel "wbe" }

newtype AppConfig = AppConfig { _appWbeConfig :: WBEConfig }
    deriving (Eq, Generic, Show)

instance Aeson.FromJSON AppConfig where
    parseJSON = Aeson.genericParseJSON $ Aeson.defaultOptions
        { Aeson.fieldLabelModifier = normalizeFieldLabel "app" }

run :: FilePath -> Settings -> IO ()
run configPath settings = do
  mConfig <- Aeson.eitherDecodeFileStrict configPath

  appConfig <- case mConfig of
      Right config -> pure config
      Left err     -> ioError $ userError $ "Config file has invalid format: " <> err
  let wbeHost = _wbeHost . _appWbeConfig $ appConfig
  let wbePort = _wbePort . _appWbeConfig $ appConfig
  let baseUrl = BaseUrl{baseUrlScheme=Http,baseUrlHost=wbeHost,baseUrlPort=wbePort,baseUrlPath=""}
  let server = Server.handlers
  manager <- liftIO $ newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager baseUrl
  Warp.runSettings settings (serve (Proxy @API) (server clientEnv))
