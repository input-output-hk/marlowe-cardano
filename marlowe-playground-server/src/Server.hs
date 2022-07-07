{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Server where

import API
import Actus.Marlowe
import qualified Auth
import Auth.Types (OAuthClientId (OAuthClientId), OAuthClientSecret (OAuthClientSecret))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, logInfoN, runStderrLoggingT)
import Control.Monad.Now (MonadNow (getCurrentTime, getPOSIXTime))
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Aeson as Aeson
import qualified Data.Aeson as A
import Data.Bits (toIntegralSized)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HM
import Data.Proxy (Proxy (Proxy))
import Data.String as S
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Time.LocalTime (LocalTime)
import Data.Time.Units (Second, toMicroseconds)
import qualified Data.Validation as Validation
import GHC.Generics (Generic)
import Language.Haskell.Interpreter (InterpreterError (CompilationErrors), InterpreterResult)
import Language.Marlowe.Pretty (pretty)
import Network.HTTP.Client.Conduit (defaultManagerSettings, managerResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Conduit (newManager)
import Network.HTTP.Simple (getResponseBody, httpJSON)
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Servant (Application, Handler (Handler), Header, Headers, NoContent (NoContent), Server, ServerError,
                ToHttpApiData, addHeader, err400, errBody, hoistServer, serve, (:<|>) ((:<|>)), (:>))
import Servant.Client (ClientEnv, mkClientEnv, parseBaseUrl)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Web.Cookie (SetCookie (setCookieExpires, setCookieHttpOnly, setCookieMaxAge, setCookieName, setCookiePath, setCookieSecure, setCookieValue),
                   defaultSetCookie)
import qualified Web.JWT as JWT
import Webghc.Client (runscript)
import Webghc.Server (CompileRequest)

genActusContract :: ContractTerms Double -> Handler String
genActusContract terms =
    case genContract defaultRiskFactors (toMarlowe terms) of
        -- Should probably send this as a server error and handle it properly on the front end
        Validation.Failure errs -> pure (unlines . (:) "ACTUS Term Validation Failed:" . map ((++) "    " . show) $ errs)
        Validation.Success c -> pure . show . pretty $ c

genActusCashflows :: ContractTerms Double -> Handler [CashFlow Double]
genActusCashflows terms = pure $ genProjectedCashflows defaultRiskFactors terms []

defaultRiskFactors :: ActusOps a => EventType -> LocalTime -> RiskFactors a
defaultRiskFactors _ _ =
    RiskFactors
        { o_rf_CURS = _one,
          o_rf_RRMO = _one,
          o_rf_SCMO = _one,
          pp_payoff = _zero,
          xd_payoff = _zero,
          dv_payoff = _zero
        }

oracle :: MonadIO m => String -> String -> m Value
oracle exchange pair = do
    response <- liftIO (httpJSON (fromString $ "GET https://api.cryptowat.ch/markets/" <> exchange <> "/" <> pair <> "/price"))
    let result = getResponseBody response :: Value
    pure result


hSessionIdCookie :: Text
hSessionIdCookie = "sessionId"

logout :: MonadIO m => m (Headers '[ Header "Set-Cookie" SetCookie ] Value)
logout = do
  now <- liftIO getCurrentTime
  let
    cookie = defaultSetCookie
      { setCookieName = encodeUtf8 hSessionIdCookie
      , setCookieExpires = Just now
      , setCookiePath = Just "/"
      }
  -- We are forced to return something from here
  -- because generated PureScript expects JSON.
  pure . addHeader cookie $ A.object []

compile ::
       ClientEnv
    -> CompileRequest
    -> Handler (Either InterpreterError (InterpreterResult String))
compile clientEnv req = do
    r <- liftIO . runExceptT $ runscript clientEnv req
    case r of
        Right vs -> pure . Right $ vs
        Left (CompilationErrors errors) ->
            pure . Left $ CompilationErrors errors
        Left e -> throwError $ err400 {errBody = BSL.pack . show $ e}

liftedAuthServer :: Auth.GithubEndpoints -> Auth.Config -> Server Auth.API
liftedAuthServer githubEndpoints config =
  hoistServer (Proxy @Auth.API) liftAuthToHandler Auth.server
  where
    liftAuthToHandler ::
      ReaderT (Auth.GithubEndpoints, Auth.Config) (LoggingT (ExceptT ServerError IO)) a ->
      Handler a
    liftAuthToHandler =
      Handler . runStderrLoggingT . flip runReaderT (githubEndpoints, config)

type Web = "api" :> (API :<|> Auth.API)

mkHandlers :: (MonadIO m) => AppConfig -> m (Server Web)
mkHandlers AppConfig {..} = do
  githubEndpoints <- liftIO Auth.mkGithubEndpoints
  pure (mhandlers webghcClientEnv :<|> liftedAuthServer githubEndpoints authConfig)

mhandlers :: ClientEnv -> Server API
mhandlers webghcClientEnv = oracle :<|> (genActusContract :<|> genActusCashflows) :<|> compile webghcClientEnv :<|> logout

app :: Server Web -> Application
app handlers =
  cors (const $ Just policy) $ serve (Proxy @Web) handlers
  where
    policy =
      simpleCorsResourcePolicy

data AppConfig = AppConfig { authConfig :: !Auth.Config, webghcClientEnv :: !ClientEnv }

initializeServerContext :: Second -> Maybe FilePath -> IO AppConfig
initializeServerContext maxInterpretationTime secrets = do
  putStrLn "Initializing Context"
  authConfig <- mkAuthConfig secrets
  mWebghcURL <- lookupEnv "WEBGHC_URL"
  webghcURL <- case mWebghcURL of
    Just url -> parseBaseUrl url
    Nothing -> do
      let localhost = "http://localhost:8080"
      hPutStrLn stderr $ "WEBGHC_URL not set, using " <> localhost
      parseBaseUrl localhost
  manager <- newManager $ defaultManagerSettings
    { managerResponseTimeout = maybe
      (managerResponseTimeout defaultManagerSettings)
      responseTimeoutMicro . toIntegralSized
      $ toMicroseconds maxInterpretationTime
    }
  pure . AppConfig authConfig $ mkClientEnv manager webghcURL

mkAuthConfig :: MonadIO m => Maybe FilePath -> m Auth.Config
mkAuthConfig (Just path) = do
  mConfig <- liftIO $ decodeFileStrict path
  case mConfig of
    Just config -> pure config
    Nothing -> do
      liftIO $ putStrLn $ "failed to decode " <> path
      mkAuthConfig Nothing
mkAuthConfig Nothing = liftIO $ do
  putStrLn "Initializing Context"
  githubClientId <- getEnvOrEmpty "GITHUB_CLIENT_ID"
  githubClientSecret <- getEnvOrEmpty "GITHUB_CLIENT_SECRET"
  jwtSignature <- getEnvOrEmpty "JWT_SIGNATURE"
  frontendURL <- getEnvOrEmpty "FRONTEND_URL"
  cbPath <- getEnvOrEmpty "GITHUB_CALLBACK_PATH"
  pure Auth.Config
          { _configJWTSignature = JWT.hmacSecret jwtSignature,
            _configFrontendUrl = frontendURL,
            _configGithubCbPath = cbPath,
            _configGithubClientId = OAuthClientId githubClientId,
            _configGithubClientSecret = OAuthClientSecret githubClientSecret
          }

getEnvOrEmpty :: String -> IO Text
getEnvOrEmpty name = do
  mEnv <- lookupEnv name
  case mEnv of
    Just env -> pure $ Text.pack env
    Nothing -> do
      putStrLn $ "Warning: " <> name <> " not set"
      pure mempty

initializeApplication :: AppConfig -> IO Application
initializeApplication config = do
  handlers <- mkHandlers config
  pure $ app handlers
