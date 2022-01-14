{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications   #-}

module Marlowe.Run.Server where

import Cardano.Prelude hiding (Handler)
import Marlowe.Run (getVersion)
import Marlowe.Run.API (API)
import Marlowe.Run.Types (Env)
import qualified Marlowe.Run.Wallet.Server as Wallet
import qualified Marlowe.Run.WebSocket as WS
import Servant (Handler (Handler), Server, ServerError, hoistServer, serveDirectoryFileServer, (:<|>) ((:<|>)))

handlers :: FilePath -> Env -> Server API
handlers staticPath env =
    hoistServer (Proxy @API) liftHandler
        ( WS.handle
            :<|> (getVersion
                   :<|> Wallet.handlers
                 )
            :<|> serveDirectoryFileServer staticPath
        )
    where
    liftHandler :: ReaderT Env (ExceptT ServerError IO) a -> Handler a
    liftHandler = Handler . flip runReaderT env
