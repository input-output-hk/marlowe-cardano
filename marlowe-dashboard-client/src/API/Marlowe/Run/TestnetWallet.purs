module API.Marlowe.Run.TestnetWallet
  ( restoreWallet
  , RestoreWalletOptions
  , RestoreError
  ) where

import Prologue
import API.Request (doPostRequestWith)
import Cardano.Wallet.Mock.Types (WalletInfo)
import Control.Monad.Except (runExceptT)
import Data.Argonaut (encodeJson)
import Data.Argonaut.Decode.Aeson as D
import Effect.Aff.Class (class MonadAff)
import Marlowe.Run.Webserver.Types (RestoreError(..), RestorePostData(..)) as BE
import Servant.PureScript (AjaxError)

type RestoreWalletOptions
  = { walletName :: String, mnemonicPhrase :: Array String, passphrase :: String }

data RestoreError
  = InvalidMnemonic
  | ClientServerError AjaxError
  | ServerError BE.RestoreError

restoreWallet ::
  forall m.
  MonadAff m =>
  RestoreWalletOptions ->
  m (Either RestoreError WalletInfo)
restoreWallet { walletName, mnemonicPhrase } = do
  let
    body =
      BE.RestorePostData
        { "getWalletName": walletName
        , "getMnemonicPhrase": mnemonicPhrase
        , "getPassphrase": "fixme-allow-pass-per-wallet"
        }

    mapResponse =
      map case _ of
        Left ajaxError -> Left $ ClientServerError ajaxError
        Right (Left BE.InvalidMnemonic) -> Left InvalidMnemonic
        Right (Left serverError) -> Left $ ServerError serverError
        Right (Right result) -> Right result
  mapResponse
    $ runExceptT
    $ doPostRequestWith
        { encode: encodeJson, decode: D.decode (D.either D.value D.value) }
        "/fixme/api/wallet/restore"
        body
