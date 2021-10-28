module Wallet.Nami where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Wallet.Nami.Internal (NamiAPI)
import Wallet.Nami.Internal as Internal

data WalletConnectionError
  = WalletNotInjected
  | AccessDenied

connect :: Aff (Either WalletConnectionError NamiAPI)
connect = do
  mNamiAPI <- liftEffect Internal.getNamiAPI
  case mNamiAPI of
    Nothing -> pure $ Left WalletNotInjected
    Just namiAPI ->
      Internal.enable namiAPI
        <#> \enabled -> if enabled then Right namiAPI else Left AccessDenied
