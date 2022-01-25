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

import API.Marlowe.Run.Wallet as WBE
import API.Marlowe.Run.Wallet.CentralizedTestnet
  ( CreateWalletResponse
  , CreateWalletError
  , RestoreWalletError
  , RestoreWalletOptions
  )
import API.Marlowe.Run.Wallet.CentralizedTestnet as TestnetAPI
import API.MockWallet as MockAPI
import AppM (AppM)
import Bridge (toBack, toFront)
import Component.Contacts.Types (WalletId, WalletInfo)
import Control.Monad.Except (lift, runExceptT)
import Data.Passpharse (Passphrase)
import Data.WalletNickname (WalletNickname)
import Halogen (HalogenM)
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
  restoreWallet options = TestnetAPI.restoreWallet options
  submitWalletTransaction wallet tx = runExceptT $
    MockAPI.submitWalletTransaction (toBack wallet) tx
  getWalletInfo wallet = map (map toFront) $ runExceptT $ MockAPI.getWalletInfo
    (toBack wallet)
  -- TODO: we use a manual getTotalFunds because a problem with the toUrlPiece from
  --       PureScript servant.
  -- getWalletTotalFunds = runExceptT <<< WBE.getApiWalletV1ByWalletidTotalfunds
  getWalletTotalFunds = runExceptT <<< WBE.getTotalFunds
  signTransaction wallet tx = runExceptT $ MockAPI.signTransaction
    (toBack wallet)
    tx

instance monadWalletHalogenM ::
  ManageWallet m =>
  ManageWallet (HalogenM state action slots msg m) where
  createWallet wn p = lift $ createWallet wn p
  restoreWallet options = lift $ restoreWallet options
  submitWalletTransaction tx wallet = lift $ submitWalletTransaction tx wallet
  getWalletInfo = lift <<< getWalletInfo
  getWalletTotalFunds = lift <<< getWalletTotalFunds
  signTransaction tx wallet = lift $ signTransaction tx wallet
