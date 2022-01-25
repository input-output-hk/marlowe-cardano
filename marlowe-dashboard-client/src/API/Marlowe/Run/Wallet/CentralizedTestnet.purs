module API.Marlowe.Run.Wallet.CentralizedTestnet
  ( createWallet
  , clientServerError
  , ClientServerErrorRow
  , CreateWalletError
  , CreateWalletResponse
  , mkRestoreWalletError
  , ServerErrorRow
  , restoreWallet
  , RestoreWalletOptions
  , RestoreWalletError(..)
  ) where

import Prologue

import Bridge (toFront)
import Cardano.Wallet.Mock.Types (WalletInfo) as Mock.Types
import Component.Contacts.Types (WalletInfo)
import Control.Error.Util (exceptNoteM)
import Control.Monad.Except (ExceptT(..), except, runExceptT, withExceptT)
import Data.Bifunctor (bimap, lmap)
import Data.MnemonicPhrase (MnemonicPhrase, MnenonicPhraseErrorRow)
import Data.MnemonicPhrase (fromStrings) as MnemonicPhrase
import Data.MnemonicPhrase (injErr) as MnenonicPhrase
import Data.Passpharse (Passphrase)
import Data.Passpharse (toString) as Passphrase
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Data.Variant.Generic
  ( class Constructors
  , mkConstructors
  , mkConstructors'
  )
import Data.WalletNickname (WalletNickname)
import Data.WalletNickname (toString) as WalletNickname
import Marlowe.Run.Server as MarloweRun
import Marlowe.Run.Wallet.V1.CentralizedTestnet.Types
  ( CreatePostData(..)
  , RestoreError(..)
  , RestorePostData(..)
  ) as BE
import Marlowe.Run.Wallet.V1.CentralizedTestnet.Types (CreateResponse(..))
import Servant.PureScript (class MonadAjax)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import Types (JsonAjaxError)

type RestoreWalletOptions =
  { walletName :: String
  , mnemonicPhrase :: Array String
  , passphrase :: Passphrase
  }

type ClientServerErrorRow r = (clientServerError :: JsonAjaxError | r)

clientServerError :: forall r. JsonAjaxError -> Variant (ClientServerErrorRow r)
clientServerError = Variant.inj (Proxy :: Proxy "clientServerError")

type ServerErrorRow :: Type -> Row Type -> Row Type
type ServerErrorRow a r = (serverError :: a | r)

type RestoreWalletError = Variant
  ( ClientServerErrorRow + ServerErrorRow BE.RestoreError +
      (invalidMnemonic :: Unit)
  )

mkRestoreWalletError :: forall c. Constructors RestoreWalletError c => c
mkRestoreWalletError = mkConstructors' (Proxy :: Proxy RestoreWalletError)

restoreWallet
  :: forall m
   . MonadAjax MarloweRun.Api m
  => RestoreWalletOptions
  -> m (Either RestoreWalletError WalletInfo)
restoreWallet { passphrase, walletName, mnemonicPhrase } = runExceptT do
  let
    body =
      BE.RestorePostData
        { "getRestoreWalletName": walletName
        , "getRestoreMnemonicPhrase": mnemonicPhrase
        , "getRestorePassphrase": Passphrase.toString passphrase
        }
    fromServerErr BE.InvalidMnemonic = mkRestoreWalletError.invalidMnemonic
    fromServerErr err = mkRestoreWalletError.serverError err

  (res :: Either _ Mock.Types.WalletInfo) <-
    withExceptT mkRestoreWalletError.clientServerError $ ExceptT $
      MarloweRun.postApiWalletV1CentralizedtestnetRestore body

  except $ bimap fromServerErr toFront res

type CreateWalletError = Variant
  (ClientServerErrorRow + ServerErrorRow Unit + MnenonicPhraseErrorRow + ())

type CreateWalletResponse =
  { walletInfo :: WalletInfo, mnemonic :: MnemonicPhrase }

createWallet
  :: forall m
   . MonadAjax MarloweRun.Api m
  => WalletNickname
  -> Passphrase
  -> m (Either CreateWalletError CreateWalletResponse)
createWallet walletName passphrase = runExceptT do
  let
    body =
      BE.CreatePostData
        { "getCreateWalletName": WalletNickname.toString walletName
        , "getCreatePassphrase": Passphrase.toString passphrase
        }
    mkError = mkConstructors (Proxy :: Proxy CreateWalletError)

  res <- withExceptT mkError.clientServerError $ ExceptT $
    MarloweRun.postApiWalletV1CentralizedtestnetCreate body
  CreateResponse { mnemonic, walletInfo } <- res `exceptNoteM`
    mkError.serverError
  mnemonic' <- except $ lmap MnenonicPhrase.injErr $ MnemonicPhrase.fromStrings
    mnemonic
  pure
    { mnemonic: mnemonic'
    , walletInfo: toFront walletInfo
    }
