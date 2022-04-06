{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

module Marlowe.Run.Server where

import Prelude

import Cardano.Prelude hiding (Handler)
import Colog.Core.Action (hoistLogAction)
import Marlowe.Run (getVersion)
import Marlowe.Run.API (API)
import qualified Marlowe.Run.Contract.V1.Server as Contract
import Marlowe.Run.Env (Env, envLogAction, withNamespace)
import qualified Marlowe.Run.Wallet.V1.Server as Wallet
import qualified Marlowe.Run.WebSocket as WS
import Servant (Handler (Handler), Server, ServerError, hoistServer, (:<|>) ((:<|>)))

newtype AppM a = AppM { runAppM :: ReaderT (Env AppM) (ExceptT ServerError IO) a }
  deriving newtype (Applicative, Functor, Monad, MonadReader (Env AppM), MonadError ServerError, MonadIO)

handlers :: Env IO -> Server API
handlers env =
    hoistServer (Proxy @API) (liftHandler <<< withNamespace ["web"])
        ( WS.handle logAction
        :<|>
          ( getVersion
          :<|> Wallet.handlers
          :<|> Contract.handlers
          )
        )
    where
    logAction = envLogAction env
    logAction' = hoistLogAction (AppM <<< lift <<< lift) logAction
    env' = env{ envLogAction = logAction' }

    liftHandler :: AppM a -> Handler a
    liftHandler = Handler <<< flip runReaderT env' <<< runAppM
