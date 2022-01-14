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
  ( RestoreError
  , RestoreWalletOptions
  )
import API.Marlowe.Run.Wallet.CentralizedTestnet as TestnetAPI
import API.MockWallet as MockAPI
import AppM (AppM)
import Bridge (toBack, toFront)
import Component.Contacts.Types (WalletId, WalletInfo)
import Control.Monad.Except (lift, runExceptT)
import Halogen (HalogenM)
import Marlowe.Run.Wallet.Types (GetTotalFunds)
import Plutus.V1.Ledger.Tx (Tx)
import Types (AjaxResponse)

class
  Monad m <=
  ManageWallet m where
  -- FIXME: Abstract from AjaxResponse
  createWallet :: m (AjaxResponse WalletInfo)
  restoreWallet :: RestoreWalletOptions -> m (Either RestoreError WalletInfo)
  submitWalletTransaction :: WalletId -> Tx -> m (AjaxResponse Unit)
  getWalletInfo :: WalletId -> m (AjaxResponse WalletInfo)
  getWalletTotalFunds :: WalletId -> m (AjaxResponse GetTotalFunds)
  signTransaction :: WalletId -> Tx -> m (AjaxResponse Tx)

instance monadWalletAppM :: ManageWallet AppM where
  createWallet = map (map toFront) $ runExceptT $ MockAPI.createWallet
  restoreWallet options = map (map toFront) $ TestnetAPI.restoreWallet options
  submitWalletTransaction wallet tx = runExceptT $
    MockAPI.submitWalletTransaction (toBack wallet) tx
  getWalletInfo wallet = map (map toFront) $ runExceptT $ MockAPI.getWalletInfo
    (toBack wallet)
  getWalletTotalFunds walletId = runExceptT $ WBE.getTotalFunds walletId
  signTransaction wallet tx = runExceptT $ MockAPI.signTransaction
    (toBack wallet)
    tx

instance monadWalletHalogenM ::
  ManageWallet m =>
  ManageWallet (HalogenM state action slots msg m) where
  createWallet = lift createWallet
  restoreWallet options = lift $ restoreWallet options
  submitWalletTransaction tx wallet = lift $ submitWalletTransaction tx wallet
  getWalletInfo = lift <<< getWalletInfo
  getWalletTotalFunds = lift <<< getWalletTotalFunds
  signTransaction tx wallet = lift $ signTransaction tx wallet
