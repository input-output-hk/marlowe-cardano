{-# LANGUAGE DeriveGeneric #-}

module Marlowe.Run where

import Cardano.Prelude hiding (Handler)
import Data.Aeson as Aeson
import Data.List (stripPrefix)
import Data.String as S
import qualified Data.Text as Text
import Data.Version (showVersion)
import qualified Paths_marlowe_dashboard_server as Package.Paths
import Prelude (userError)

data WBEConfig = WBEConfig { _wbeHost :: String, _wbePort :: Int }
    deriving (Eq, Generic, Show)

normalizeFieldLabel prefix label =
  maybe label lower1 $ stripPrefix ('_' : prefix) label

lower1 (c:cs) = toLower c : cs
lower1 []     = []

instance FromJSON WBEConfig where
    parseJSON = Aeson.genericParseJSON $ Aeson.defaultOptions
        { fieldLabelModifier = normalizeFieldLabel "wbe" }

data AppConfig = AppConfig { _appWbeConfig :: WBEConfig, _appStaticPath :: FilePath }
    deriving (Eq, Generic, Show)

instance FromJSON AppConfig where
    parseJSON = Aeson.genericParseJSON $ Aeson.defaultOptions
        { fieldLabelModifier = normalizeFieldLabel "app" }

getVersion :: Applicative m => m Text
getVersion = pure $ Text.pack $ showVersion Package.Paths.version

initializeServerContext :: FilePath -> IO AppConfig
initializeServerContext configPath = do
    mConfig <- decodeFileStrict configPath
    case mConfig of
        Just config -> pure config
        Nothing     -> ioError $ userError "Config file has invalid format"
