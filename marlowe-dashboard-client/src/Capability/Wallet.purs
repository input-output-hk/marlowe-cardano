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

import API.Marlowe.Run.Wallet.CentralizedTestnet
  ( CreateWalletError
  , CreateWalletResponse
  , RestoreWalletError
  , RestoreWalletOptions
  )
import API.Marlowe.Run.Wallet.CentralizedTestnet as TestnetAPI
import AppM (AppM)
import Component.Contacts.Types (WalletId, WalletInfo)
import Control.Monad.Except (lift)
import Data.Passpharse (Passphrase)
import Data.WalletNickname (WalletNickname)
import Effect.Exception.Unsafe (unsafeThrow)
import Halogen (HalogenM)
import Marlowe.Run.Server as MarloweRun
import Marlowe.Run.Wallet.V1 (GetTotalFundsResponse)
import Plutus.V1.Ledger.Tx (Tx)
import Types (AjaxResponse)

-- FIXME: Abstract away AjaxResponse (just return an `m ResponseType` and
-- handle API failures in the concrete Monad instance).
class Monad m <= ManageWallet m where
  createWallet
    :: WalletNickname
    -> Passphrase
    -> m (Either CreateWalletError CreateWalletResponse)
  restoreWallet
    :: RestoreWalletOptions -> m (Either RestoreWalletError WalletInfo)
  submitWalletTransaction :: WalletId -> Tx -> m (AjaxResponse Unit)
  getWalletInfo :: WalletId -> m (AjaxResponse WalletInfo)
  getWalletTotalFunds :: WalletId -> m (AjaxResponse GetTotalFundsResponse)
  signTransaction :: WalletId -> Tx -> m (AjaxResponse Tx)

instance monadWalletAppM :: ManageWallet AppM where
  createWallet wn p = TestnetAPI.createWallet wn p
  restoreWallet = TestnetAPI.restoreWallet
  submitWalletTransaction _wallet _tx = unsafeThrow "Not implemented"
  getWalletInfo _wallet = unsafeThrow "Not implemented"
  getWalletTotalFunds = MarloweRun.getApiWalletV1ByWalletidTotalfunds
  signTransaction _wallet _tx = unsafeThrow "Not implemented"

instance monadWalletHalogenM ::
  ManageWallet m =>
  ManageWallet (HalogenM state action slots msg m) where
  createWallet wn p = lift $ createWallet wn p
  restoreWallet options = lift $ restoreWallet options
  submitWalletTransaction tx wallet = lift $ submitWalletTransaction tx wallet
  getWalletInfo = lift <<< getWalletInfo
  getWalletTotalFunds = lift <<< getWalletTotalFunds
  signTransaction tx wallet = lift $ signTransaction tx wallet
