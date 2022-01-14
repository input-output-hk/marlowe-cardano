{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | This module holds the API to access the centralized testnet
module Marlowe.Run.Wallet.CentralizedTestnet.API where

import Cardano.Prelude
import Marlowe.Run.Wallet.CentralizedTestnet.Types (CheckPostData, RestoreError, RestorePostData)
import Servant.API (JSON, Post, ReqBody, (:<|>), (:>))
-- FIXME: I don't like to use a Mock type here, but we'd need to publish some changes upstream to the PAB to fix this
import Cardano.Wallet.Mock.Types (WalletInfo)

type API =
        ("restore" :> ReqBody '[ JSON] RestorePostData :> Post '[JSON] (Either RestoreError WalletInfo)
        :<|> "check-mnemonic" :> ReqBody '[ JSON] CheckPostData  :> Post '[JSON] Bool
        )
