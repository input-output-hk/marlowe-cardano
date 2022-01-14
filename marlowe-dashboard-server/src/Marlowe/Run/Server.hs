{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications   #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marlowe.Run.Server
 ( handlers
 , initializeServerContext
 , WBEConfig(..)
 , AppConfig(..)
 )
 where

import Cardano.Prelude hiding (Handler)
import Marlowe.Run.API (API)
import Prelude (userError)


import Data.Aeson as Aeson


import Data.String as S
import qualified Data.Text as Text
import Data.Version (showVersion)
import Marlowe.Run.Types (Env)
import qualified Marlowe.Run.Wallet.Server as Wallet
import qualified Marlowe.Run.WebSocket as WS
import qualified Paths_marlowe_dashboard_server as Package.Paths
import Servant (Handler (Handler), Server, ServerError, hoistServer, serveDirectoryFileServer, (:<|>) ((:<|>)))

handlers :: FilePath -> Env -> Server API
handlers staticPath env =
    hoistServer (Proxy @API) liftHandler
        ( WS.handle
            :<|> (handleVersion
                   :<|> Wallet.handlers
                 )
            :<|> serveDirectoryFileServer staticPath
        )
    where
    liftHandler :: ReaderT Env (ExceptT ServerError IO) a -> Handler a
    liftHandler = Handler . flip runReaderT env

handleVersion :: Applicative m => m Text
handleVersion = pure $ Text.pack $ showVersion Package.Paths.version

data WBEConfig = WBEConfig { _wbeHost :: String, _wbePort :: Int }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON)

data AppConfig = AppConfig { getWbeConfig :: WBEConfig, getStaticPath :: FilePath }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON)

initializeServerContext :: FilePath -> IO AppConfig
initializeServerContext configPath = do
    mConfig <- decodeFileStrict configPath
    case mConfig of
        Just config -> pure config
        Nothing     -> ioError $ userError "Config file has invalid format"
