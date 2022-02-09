{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Marlowe.Run.Wallet.V1.API
    ( API
    , GetTotalFundsResponse
    ) where


import Marlowe.Run.Wallet.V1 (GetTotalFundsResponse)
import qualified Marlowe.Run.Wallet.V1.CentralizedTestnet.API as CentralizedTestnet
import Marlowe.Run.Wallet.V1.Types (WalletId)
import Servant.API (Capture, Get, JSON, (:<|>), (:>))

type API =
    (Capture "wallet-id" WalletId :> "total-funds" :> Get '[JSON] GetTotalFundsResponse)
    :<|> ("centralized-testnet" :> CentralizedTestnet.API)
