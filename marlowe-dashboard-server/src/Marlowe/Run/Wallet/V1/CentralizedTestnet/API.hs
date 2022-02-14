{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | This module holds the API to access the centralized testnet
module Marlowe.Run.Wallet.V1.CentralizedTestnet.API where

import Marlowe.Run.Wallet.V1.CentralizedTestnet.Types (CreatePostData, CreateResponse, RestorePostData)
import Marlowe.Run.Wallet.V1.Types (WalletInfo)
import Servant.API (JSON, Post, ReqBody, (:<|>), (:>))

type API =
  ("restore" :> ReqBody '[ JSON] RestorePostData :> Post '[JSON] WalletInfo
  :<|> "create" :> ReqBody '[ JSON] CreatePostData :> Post '[JSON] CreateResponse
  )
