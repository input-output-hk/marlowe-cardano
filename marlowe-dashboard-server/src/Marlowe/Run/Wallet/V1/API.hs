{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Marlowe.Run.Wallet.V1.API where

import Cardano.Prelude
import Data.Aeson (ToJSON)
import Marlowe.Run.Dto (AssetsDto, ToDto (..), WalletIdDto)
import qualified Marlowe.Run.Wallet.V1 as Domain
import qualified Marlowe.Run.Wallet.V1.CentralizedTestnet.API as CentralizedTestnet
import Servant.API (Capture, Get, JSON, (:<|>), (:>))

data GetTotalFundsResponse =
    GetTotalFundsResponse
        { assets :: !AssetsDto
        , sync   :: !Double
        }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (ToJSON)

instance ToDto Domain.GetTotalFundsResponse GetTotalFundsResponse where
    toDto Domain.GetTotalFundsResponse{..} =
        GetTotalFundsResponse (toDto assets) sync

type API =
    (Capture "wallet-id" WalletIdDto :> "get-total-funds" :> Get '[JSON] GetTotalFundsResponse)
    :<|> ("centralized-testnet" :> CentralizedTestnet.API)
