module Wallet.Yoroi where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Wallet.Yoroi.Internal (DappConector, WalletAPI)
import Wallet.Yoroi.Internal as Internal

data WalletConnectionError
  = WalletNotInjected
  | AccessDenied

connect :: Aff (Either WalletConnectionError WalletAPI)
connect = do
  mYoroiConnector <- liftEffect $ Internal.getDappConector "yoroi"
  case mYoroiConnector of
    Nothing -> pure $ Left WalletNotInjected
    Just connector -> Right <$> Internal.enable connector

-- <#> \enabled -> if enabled then Right api else Left AccessDenied
