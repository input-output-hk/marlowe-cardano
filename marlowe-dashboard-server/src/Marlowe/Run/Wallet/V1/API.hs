{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Marlowe.Run.Wallet.V1.API
    ( HttpWalletId(..)
    , API
    , GetTotalFundsResponse
    ) where

import Cardano.Prelude

import Cardano.Wallet.Primitive.Types (WalletId)
import qualified Data.Text as T
import Data.Text.Class (FromText (fromText), TextDecodingError (getTextDecodingError))
import Marlowe.Run.Wallet.V1 (GetTotalFundsResponse)
import qualified Marlowe.Run.Wallet.V1.CentralizedTestnet.API as CentralizedTestnet
import Servant (FromHttpApiData (..))
import Servant.API (Capture, Get, JSON, (:<|>), (:>))

newtype HttpWalletId = HttpWalletId WalletId

instance FromHttpApiData HttpWalletId where
    parseUrlPiece = bimap (T.pack . getTextDecodingError) HttpWalletId . fromText

type API =
    (Capture "wallet-id" HttpWalletId :> "total-funds" :> Get '[JSON] GetTotalFundsResponse)
    :<|> ("centralized-testnet" :> CentralizedTestnet.API)
