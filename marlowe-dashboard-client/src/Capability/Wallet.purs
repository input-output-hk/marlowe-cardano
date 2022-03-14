module Capability.Wallet
  ( class ManageWallet
  , createWallet
  , restoreWallet
  , submitWalletTransaction
  , getWalletInfo
  , getWalletTotalFunds
  , signTransaction
  ) where

import Prologue

import AppM (AppM)
import Control.Monad.Except (lift)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader (ReaderT)
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.Passphrase (Passphrase)
import Data.WalletId (WalletId)
import Data.WalletNickname (WalletNickname)
import Effect.Exception.Unsafe (unsafeThrow)
import Halogen (HalogenM)
import Marlowe.Run.Server as MarloweRun
import Marlowe.Run.Wallet.V1 (GetTotalFundsResponse)
import Marlowe.Run.Wallet.V1.CentralizedTestnet.Types
  ( CreatePostData(..)
  , CreateResponse
  , RestorePostData(..)
  )
import Marlowe.Run.Wallet.V1.Types (WalletInfo)
import Plutus.V1.Ledger.Tx (Tx)
import Servant.PureScript (class MonadAjax)
import Types (AjaxResponse)

class Monad m <= ManageWallet m where
  createWallet
    :: WalletNickname
    -> Passphrase
    -> m (AjaxResponse CreateResponse)
  restoreWallet
    :: MnemonicPhrase
    -> WalletNickname
    -> Passphrase
    -> m (AjaxResponse WalletInfo)
  submitWalletTransaction :: WalletId -> Tx -> m (AjaxResponse Unit)
  getWalletInfo :: WalletId -> m (AjaxResponse WalletInfo)
  getWalletTotalFunds :: WalletId -> m (AjaxResponse GetTotalFundsResponse)
  signTransaction :: WalletId -> Tx -> m (AjaxResponse Tx)

instance MonadAjax MarloweRun.Api m => ManageWallet (AppM m) where
  createWallet wn p = MarloweRun.postApiWalletV1CentralizedtestnetCreate
    $ CreatePostData { getCreateWalletName: wn, getCreatePassphrase: p }
  restoreWallet mp wn p = MarloweRun.postApiWalletV1CentralizedtestnetRestore
    $ RestorePostData
        { getRestoreMnemonicPhrase: mp
        , getRestoreWalletName: wn
        , getRestorePassphrase: p
        }
  submitWalletTransaction _wallet _tx = unsafeThrow "Not implemented"
  getWalletInfo _wallet = unsafeThrow "Not implemented"
  getWalletTotalFunds = MarloweRun.getApiWalletV1ByWalletidTotalfunds
  signTransaction _wallet _tx = unsafeThrow "Not implemented"

instance ManageWallet m => ManageWallet (HalogenM state action slots msg m) where
  createWallet wn p = lift $ createWallet wn p
  restoreWallet mp wn p = lift $ restoreWallet mp wn p
  submitWalletTransaction tx wallet = lift $ submitWalletTransaction tx wallet
  getWalletInfo = lift <<< getWalletInfo
  getWalletTotalFunds = lift <<< getWalletTotalFunds
  signTransaction tx wallet = lift $ signTransaction tx wallet

instance ManageWallet m => ManageWallet (MaybeT m) where
  createWallet wn p = lift $ createWallet wn p
  restoreWallet mp wn p = lift $ restoreWallet mp wn p
  submitWalletTransaction tx wallet = lift $ submitWalletTransaction tx wallet
  getWalletInfo = lift <<< getWalletInfo
  getWalletTotalFunds = lift <<< getWalletTotalFunds
  signTransaction tx wallet = lift $ signTransaction tx wallet

instance ManageWallet m => ManageWallet (ReaderT r m) where
  createWallet wn p = lift $ createWallet wn p
  restoreWallet mp wn p = lift $ restoreWallet mp wn p
  submitWalletTransaction tx wallet = lift $ submitWalletTransaction tx wallet
  getWalletInfo = lift <<< getWalletInfo
  getWalletTotalFunds = lift <<< getWalletTotalFunds
  signTransaction tx wallet = lift $ signTransaction tx wallet
