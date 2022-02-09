{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications   #-}

module Marlowe.Run.Server where

import Cardano.Prelude hiding (Handler)
import Marlowe.Run (getVersion)
import Marlowe.Run.API (API)
import qualified Marlowe.Run.Wallet.V1.Server as Wallet
import qualified Marlowe.Run.WebSocket as WS
import Servant (Handler (Handler), Server, ServerError, hoistServer, (:<|>) ((:<|>)))
import Servant.Client (ClientEnv)

handlers :: ClientEnv -> Server API
handlers env =
    hoistServer (Proxy @API) liftHandler
        ( WS.handle
            :<|> (getVersion
                   :<|> Wallet.handlers
                 )
        )
    where
    liftHandler :: ReaderT ClientEnv (ExceptT ServerError IO) a -> Handler a
    liftHandler = Handler . flip runReaderT env
