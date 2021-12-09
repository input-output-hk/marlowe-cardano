-- This module provides the API calls to access the MockWallet features of the PAB. It was developed
-- for an initial integration point and could be reused for fast integration testing, but is not
-- currently being used.
module API.MockWallet
  ( createWallet
  , submitWalletTransaction
  , getWalletInfo
  , getWalletTotalFunds
  , signTransaction
  ) where

import Prologue
import API.Request (doGetRequest, doEmptyPostRequest, doPostRequest)
import API.Url (toUrlPiece)
import Cardano.Wallet.Mock.Types (WalletInfo)
import Control.Monad.Error.Class (class MonadError)
import Effect.Aff.Class (class MonadAff)
import Plutus.V1.Ledger.Tx (Tx)
import Plutus.V1.Ledger.Value (Value)
import Servant.PureScript (AjaxError)
import Wallet.Emulator.Wallet (Wallet)

createWallet ::
  forall m.
  MonadError AjaxError m =>
  MonadAff m =>
  m WalletInfo
createWallet = doEmptyPostRequest "/pab/wallet/create"

submitWalletTransaction ::
  forall m.
  MonadError AjaxError m =>
  MonadAff m =>
  Wallet -> Tx -> m Unit
submitWalletTransaction wallet tx = doPostRequest ("/pab/wallet/" <> toUrlPiece wallet) tx

getWalletInfo ::
  forall m.
  MonadError AjaxError m =>
  MonadAff m =>
  Wallet -> m WalletInfo
getWalletInfo wallet = doGetRequest ("/pab/wallet/" <> toUrlPiece wallet <> "/own-public-key")

getWalletTotalFunds ::
  forall m.
  MonadError AjaxError m =>
  MonadAff m =>
  Wallet -> m Value
getWalletTotalFunds wallet = doGetRequest $ "/pab/wallet/" <> toUrlPiece wallet <> "/total-funds"

signTransaction ::
  forall m.
  MonadError AjaxError m =>
  MonadAff m =>
  Wallet -> Tx -> m Tx
signTransaction wallet tx = doPostRequest ("/pab/wallet/" <> toUrlPiece wallet <> "/sign") tx
