{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}

module Marlowe.Run.Webserver.API where

import           Cardano.Prelude
import           Data.Text                   (Text)
import           Marlowe.Run.Webserver.Types (RestoreError, RestorePostData)
import           Servant.API                 (Get, JSON, PlainText, Post, Raw, ReqBody, (:<|>), (:>))
import           Servant.API.WebSocket       (WebSocketPending)
-- FIXME: I don't like to use a Mock type here, but we'd need to publish some changes upstream to the PAB to fix this
import           Cardano.Wallet.Mock.Types   (WalletInfo)

type API = WebSocketAPI
    :<|> HTTPAPI
    :<|> Raw

type HTTPAPI = "api" :>
    ("version" :> Get '[PlainText, JSON] Text
    :<|> "wallet" :>
        ("restore" :> ReqBody '[ JSON] RestorePostData :> Post '[JSON] (Either RestoreError WalletInfo)
        )
    )

type WebSocketAPI = "ws" :> WebSocketPending
