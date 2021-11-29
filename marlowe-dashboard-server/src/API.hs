{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}

module API where

import           Data.Aeson            (FromJSON, ToJSON, Value)
import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import           Servant.API           (Capture, Get, Header, JSON, NoContent, PlainText, Post, Raw, ReqBody, (:<|>),
                                        (:>))
import           Servant.API.WebSocket (WebSocketPending)
import           Types                 (RestoreError, RestorePostData)
-- import           Cardano.Wallet.Primitive.Types (WalletId(..))

type API = WebSocketAPI
    :<|> HTTPAPI
    :<|> Raw

type HTTPAPI = "api" :>
    ("version" :> Get '[PlainText, JSON] Text
    :<|> "wallet" :>
        -- FIXME: WalletId does not implement toJSON
        ("restore" :> ReqBody '[ JSON] RestorePostData :> Post '[JSON] (Either RestoreError Text)
        )
    )

type WebSocketAPI = "ws" :> WebSocketPending
