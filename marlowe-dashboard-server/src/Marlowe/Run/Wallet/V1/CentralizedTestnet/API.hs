{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | This module holds the API to access the centralized testnet
module Marlowe.Run.Wallet.V1.CentralizedTestnet.API where

import Cardano.Prelude
import Marlowe.Run.Wallet.V1.CentralizedTestnet.Types (CreatePostData, CreateResponse, RestoreError, RestorePostData)
import Marlowe.Run.Wallet.V1.Types (WalletInfo)
import Servant.API (JSON, Post, ReqBody, (:<|>), (:>))

type API =
        ("restore" :> ReqBody '[ JSON] RestorePostData :> Post '[JSON] (Either RestoreError WalletInfo)
        :<|> "create" :> ReqBody '[ JSON] CreatePostData :> Post '[JSON] (Maybe CreateResponse)
        )
