{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}

module Marlowe.Run.Wallet.API where

import Cardano.Prelude
import Data.Aeson (ToJSON)
import Marlowe.Run.Types (ValueDto)
import qualified Marlowe.Run.Wallet.CentralizedTestnet.API as CentralizedTestnet
import Servant.API (Capture, Get, JSON, (:<|>), (:>))

data GetTotalFundsResponse =
    GetTotalFundsResponse
        { assets :: !ValueDto
        , sync   :: !Double
        }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (ToJSON)

type API =
    (Capture "wallet-id" Text :> "get-total-funds" :> Get '[JSON] GetTotalFundsResponse)
    :<|> ("centralized-testnet" :> CentralizedTestnet.API)
