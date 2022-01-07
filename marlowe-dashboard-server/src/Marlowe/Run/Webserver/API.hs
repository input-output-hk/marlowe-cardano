{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Marlowe.Run.Webserver.API where

import Cardano.Prelude
import qualified Marlowe.Run.Webserver.Wallet.API as Wallet
import Servant.API (Get, JSON, PlainText, Raw, (:<|>), (:>))
import Servant.API.WebSocket (WebSocketPending)

type API = WebSocketAPI
    :<|> HTTPAPI
    :<|> Raw

type HTTPAPI = "api" :>
    ("version" :> Get '[PlainText, JSON] Text
    :<|> "wallet" :> Wallet.API
    )

type WebSocketAPI = "ws" :> WebSocketPending
