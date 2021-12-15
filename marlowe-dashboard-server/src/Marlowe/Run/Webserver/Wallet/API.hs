{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Marlowe.Run.Webserver.Wallet.API where

import qualified Marlowe.Run.Webserver.Wallet.CentralizedTestnet.API as CentralizedTestnet
import           Servant.API                                         ((:>))

type API = "centralized-testnet" :>
        CentralizedTestnet.API

