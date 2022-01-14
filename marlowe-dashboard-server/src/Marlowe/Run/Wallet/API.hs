{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Marlowe.Run.Wallet.API where

import qualified Marlowe.Run.Wallet.CentralizedTestnet.API as CentralizedTestnet
import Marlowe.Run.Wallet.Types (GetTotalFunds)
import Servant.API (Capture, Get, JSON, (:<|>), (:>))
-- FIXME: I don't like to use a Emulator type here, but we'd need to publish some changes upstream to the PAB to fix this
import Wallet.Emulator (WalletId)



type API =
    (Capture "wallet-id" WalletId :> "get-total-funds" :> Get '[JSON] GetTotalFunds)
    :<|> ("centralized-testnet" :> CentralizedTestnet.API)
