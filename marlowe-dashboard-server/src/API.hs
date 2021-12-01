{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}

module API where

import           Data.Aeson                (FromJSON, ToJSON, Value)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Servant.API               (Capture, Get, Header, JSON, NoContent, PlainText, Post, Raw, ReqBody,
                                            (:<|>), (:>))
import           Servant.API.WebSocket     (WebSocketPending)
import           Types                     (RestoreError, RestorePostData)
-- FIXME: I don't like to use a Mock type here, but we'd need to publish some changes upstream to the PAB to fix this
import           Cardano.Wallet.Mock.Types (WalletInfo)

type API = WebSocketAPI
    :<|> HTTPAPI
    :<|> Raw

type HTTPAPI = "api" :>
    ("version" :> Get '[PlainText, JSON] Text
    :<|> "wallet" :>
        -- FIXME: WalletId does not implement toJSON
        ("restore" :> ReqBody '[ JSON] RestorePostData :> Post '[JSON] (Either RestoreError WalletInfo)
        )
    )

type WebSocketAPI = "ws" :> WebSocketPending
