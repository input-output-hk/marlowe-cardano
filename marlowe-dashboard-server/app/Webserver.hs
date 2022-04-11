{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Webserver where

import Prelude

import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Colog (cmapM, defaultFieldMap, fmtRichMessageDefault, logInfo, logTextStderr, logWarning, upgradeMessageAction,
              usingLoggerT)
import qualified Colog
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (parseJSON, (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Char (toLower)
import Data.Function ((&))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Marlowe.Run.API (API)
import Marlowe.Run.Env (Env (..))
import qualified Marlowe.Run.Server as Server
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status (statusCode, statusMessage))
import Network.Wai (pathInfo, requestMethod)
import Network.Wai.Handler.Warp as Warp
import Servant (serve)
import Servant.Client (BaseUrl (BaseUrl, baseUrlHost, baseUrlPath, baseUrlPort, baseUrlScheme), Scheme (Http),
                       mkClientEnv)
import Text.Printf (printf)
import Verbosity (Verbosity (..))
import qualified Verbosity

normalizeFieldLabel :: String -> String -> String
normalizeFieldLabel prefix label =
  maybe label lower1 $ stripPrefix ('_' : prefix) label

lower1 :: String -> String
lower1 (c:cs) = toLower c : cs
lower1 []     = []

data ChainIndexConfig = ChainIndexConfig { _chainIndexHost :: String, _chainIndexPort :: Int }
    deriving (Eq, Generic, Show)

instance Aeson.FromJSON ChainIndexConfig where
    parseJSON = Aeson.genericParseJSON $ Aeson.defaultOptions
        { Aeson.fieldLabelModifier = normalizeFieldLabel "chainIndex" }

data WBEConfig = WBEConfig { _wbeHost :: String, _wbePort :: Int }
    deriving (Eq, Generic, Show)

instance Aeson.FromJSON WBEConfig where
    parseJSON = Aeson.genericParseJSON $ Aeson.defaultOptions
        { Aeson.fieldLabelModifier = normalizeFieldLabel "wbe" }

data AppConfig = AppConfig
  { _appChainIndexConfig :: ChainIndexConfig
  , _appWbeConfig        :: WBEConfig
  , _appVerbosity        :: Maybe Verbosity
  }
    deriving (Eq, Generic, Show)

instance Aeson.FromJSON AppConfig where
    parseJSON (Aeson.Object o) = AppConfig
      <$> o .: "chainIndexConfig"
      <*> o .: "wbeConfig"
      <*> o .:? "verbosity"
    parseJSON invalid = Aeson.typeMismatch "Object" invalid

run :: FilePath -> Settings -> Maybe Verbosity -> NetworkId -> IO ()
run configPath settings verbosity networkId = do
  mConfig <- Aeson.eitherDecodeFileStrict configPath

  appConfig <- case mConfig of
      Right config -> pure config
      Left err     -> ioError $ userError $ "Config file has invalid format: " <> err
  manager <- liftIO $ newManager defaultManagerSettings
  let
    wbeHost = _wbeHost . _appWbeConfig $ appConfig
    wbePort = _wbePort . _appWbeConfig $ appConfig

    wbeBaseUrl = BaseUrl{baseUrlScheme=Http,baseUrlHost=wbeHost,baseUrlPort=wbePort,baseUrlPath=""}
    wbeClientEnv = mkClientEnv manager wbeBaseUrl

    chainIndexHost = _chainIndexHost . _appChainIndexConfig $ appConfig
    chainIndexPort = _chainIndexPort . _appChainIndexConfig $ appConfig

    chainIndexBaseUrl = BaseUrl{baseUrlScheme=Http,baseUrlHost=chainIndexHost,baseUrlPort=chainIndexPort,baseUrlPath=""}
    chainIndexClientEnv = mkClientEnv manager chainIndexBaseUrl

    verbosity' = fromMaybe Verbosity.Normal (verbosity <|> _appVerbosity appConfig)
    logFilter = Verbosity.mkLogFilter verbosity'
      -- Copy of https://hackage.haskell.org/package/co-log-0.4.0.1/docs/src/Colog.Actions.html#richMessageAction
      -- That logs to stderr instead of stdout
    richMessageActionStdErr =upgradeMessageAction defaultFieldMap
      $ cmapM fmtRichMessageDefault logTextStderr
    logger = Colog.cfilter (\(Colog.Msg sev _ _) -> logFilter sev) richMessageActionStdErr
    settings' = settings
      & setLogger
        (\request status _ -> usingLoggerT logger do
          let method = TE.decodeUtf8 $ requestMethod request
              path = T.intercalate "/" $ pathInfo request
              code = statusCode status
              message = TE.decodeUtf8 $ statusMessage  status
          logInfo $ T.pack $ printf "%s %s %d %s" method path code message
        )
      & setBeforeMainLoop
        (usingLoggerT logger do
          logInfo $ "Running on port: " <> T.pack (show $ getPort settings)
          logInfo $ "Verbosity: " <> T.pack (show verbosity')
          case networkId of
            Mainnet       -> logWarning "Running on Mainnet"
            Testnet magic -> logInfo $ "Network magic: " <> T.pack (show $ unNetworkMagic magic)
          logInfo $ "Config: " <> T.pack (show appConfig)
        )


    env = Env wbeClientEnv chainIndexClientEnv networkId logger
  Warp.runSettings settings' (serve (Proxy @API) (Server.handlers env))
