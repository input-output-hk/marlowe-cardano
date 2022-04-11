{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Marlowe.Run.API where

import Cardano.Prelude
import qualified Marlowe.Run.Contract.V1.API as Contract
import qualified Marlowe.Run.Wallet.V1.API as Wallet
import Servant.API (Get, JSON, PlainText, (:<|>), (:>))
import Servant.API.WebSocket (WebSocketPending)

type API = WebSocketAPI
    :<|> HTTPAPI

type HTTPAPI = "api" :>
    ("version" :> Get '[PlainText, JSON] Text
    :<|> "wallet" :> "v1" :> Wallet.API
    :<|> "contracts" :> "v1" :> Contract.API
    )

type WebSocketAPI = "ws" :> WebSocketPending
