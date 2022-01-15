module API.Marlowe.Run.Wallet
  ( getTotalFunds
  ) where

import Prologue

import API.Request (doGetRequest)
import Component.Contacts.Types (WalletId)
import Control.Monad.Error.Class (class MonadError)
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Marlowe.Run.Dto (WalletIdDto)
import Marlowe.Run.Wallet.V1.API (GetTotalFundsResponse)
import Servant.PureScript (AjaxError)

getTotalFunds
  :: forall m
   . MonadAff m
  => MonadError AjaxError m
  => WalletIdDto
  -> m GetTotalFundsResponse
getTotalFunds wallet =
  doGetRequest $ "/api/wallet/v1/" <> unwrap wallet <> "/total-funds"
