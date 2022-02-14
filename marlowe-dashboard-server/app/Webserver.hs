{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Webserver where

import Prelude

import qualified Colog
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (parseJSON, (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Marlowe.Run.API (API)
import Marlowe.Run.Env (Env (..))
import qualified Marlowe.Run.Server as Server
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp as Warp
import Servant (serve)
import Servant.Client (BaseUrl (BaseUrl, baseUrlHost, baseUrlPath, baseUrlPort, baseUrlScheme), Scheme (Http),
                       mkClientEnv)
import Verbosity (Verbosity (..))
import qualified Verbosity

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

data AppConfig = AppConfig { _appWbeConfig :: WBEConfig, _appVerbosity :: Maybe Verbosity }
    deriving (Eq, Generic, Show)

instance Aeson.FromJSON AppConfig where
    parseJSON (Aeson.Object o) = AppConfig
      <$> o .: "wbeConfig"
      <*> o .:? "verbosity"
    parseJSON invalid = Aeson.typeMismatch "Object" invalid

run :: FilePath -> Settings -> Maybe Verbosity -> IO ()
run configPath settings verbosity = do
  mConfig <- Aeson.eitherDecodeFileStrict configPath

  appConfig <- case mConfig of
      Right config -> pure config
      Left err     -> ioError $ userError $ "Config file has invalid format: " <> err
  manager <- liftIO $ newManager defaultManagerSettings
  let
    wbeHost = _wbeHost . _appWbeConfig $ appConfig
    wbePort = _wbePort . _appWbeConfig $ appConfig

    baseUrl = BaseUrl{baseUrlScheme=Http,baseUrlHost=wbeHost,baseUrlPort=wbePort,baseUrlPath=""}
    clientEnv = mkClientEnv manager baseUrl

    logFilter = Verbosity.mkLogFilter (fromMaybe Verbosity.Normal (verbosity <|> _appVerbosity appConfig))
    logger = Colog.cfilter (\(Colog.Msg sev _ _) -> logFilter sev) Colog.richMessageAction

    env = Env clientEnv logger

  Warp.runSettings settings (serve (Proxy @API) (Server.handlers env))
