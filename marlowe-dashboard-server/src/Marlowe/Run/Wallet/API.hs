{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators      #-}

module Marlowe.Run.Wallet.API where

import Cardano.Prelude
import Data.Aeson (ToJSON)
import Marlowe.Run.Types (ValueDto, valueToDto)
import Marlowe.Run.Wallet (GetTotalFunds (..))
import qualified Marlowe.Run.Wallet.CentralizedTestnet.API as CentralizedTestnet
import Servant.API (Capture, Get, JSON, (:<|>), (:>))

data GetTotalFundsDto =
    GetTotalFundsDto
        { assets :: !ValueDto
        , sync   :: !Double
        }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (ToJSON)

getTotalFundsToDto :: GetTotalFunds -> GetTotalFundsDto
getTotalFundsToDto GetTotalFunds{..} = GetTotalFundsDto (valueToDto assets) sync

type API =
    (Capture "wallet-id" Text :> "get-total-funds" :> Get '[JSON] GetTotalFundsDto)
    :<|> ("centralized-testnet" :> CentralizedTestnet.API)
