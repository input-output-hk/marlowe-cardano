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

import Control.Error.Util (exceptNoteM)
import Control.Logger.Capability (class MonadLogger, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), except, runExceptT, withExceptT)
import Data.Bifunctor (lmap)
import Data.MnemonicPhrase (MnemonicPhrase, MnenonicPhraseErrorRow)
import Data.MnemonicPhrase (fromStrings) as MnemonicPhrase
import Data.Passpharse (Passphrase)
import Data.Passpharse (toString) as Passphrase
import Data.String (joinWith) as String
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Data.Variant.Generic
  ( class Constructors
  , mkConstructors
  , mkConstructors'
  )
import Data.WalletNickname (WalletNickname)
import Marlowe.Run.Server as MarloweRun
import Marlowe.Run.Wallet.V1.CentralizedTestnet.Types
  ( CreatePostData(..)
  , RestoreError(..)
  , RestorePostData(..)
  ) as BE
import Marlowe.Run.Wallet.V1.CentralizedTestnet.Types (CreateResponse(..))
import Marlowe.Run.Wallet.V1.Types (WalletInfo)
import Servant.PureScript (class MonadAjax)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import Types (JsonAjaxError)

type RestoreWalletOptions =
  { walletName :: WalletNickname
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
    -- TODO: provide more fine grained info about mnemonic validation error
    -- (from the server side) so we can handle it.
    -- Validation inconsistency should be logged here too.
    fromServerErr BE.InvalidMnemonic = mkRestoreWalletError.invalidMnemonic
    fromServerErr err = mkRestoreWalletError.serverError err

  res <- withExceptT mkRestoreWalletError.clientServerError
    $ ExceptT
    $ MarloweRun.postApiWalletV1CentralizedtestnetRestore body

  except $ lmap fromServerErr res

type CreateWalletError = Variant
  (ClientServerErrorRow + ServerErrorRow Unit + MnenonicPhraseErrorRow + ())

type CreateWalletResponse =
  { walletInfo :: WalletInfo, mnemonic :: MnemonicPhrase }

createWallet
  :: forall m
   . MonadAjax MarloweRun.Api m
  => MonadLogger String m
  => WalletNickname
  -> Passphrase
  -> m (Either CreateWalletError CreateWalletResponse)
createWallet walletName passphrase = runExceptT do
  let
    body =
      BE.CreatePostData
        { "getCreateWalletName": walletName
        , "getCreatePassphrase": Passphrase.toString passphrase
        }
    mkError = mkConstructors (Proxy :: Proxy CreateWalletError)

  res <- withExceptT mkError.clientServerError $ ExceptT $
    MarloweRun.postApiWalletV1CentralizedtestnetCreate body
  CreateResponse { mnemonic, walletInfo } <- res `exceptNoteM`
    mkError.serverError
  mnemonic' <- case MnemonicPhrase.fromStrings mnemonic of
    Left _ -> do
      error $
        "Invalid mnemonic from server: " <> String.joinWith " " mnemonic
      throwError mkError.serverError
    Right m -> pure m
  pure
    { mnemonic: mnemonic'
    , walletInfo
    }
