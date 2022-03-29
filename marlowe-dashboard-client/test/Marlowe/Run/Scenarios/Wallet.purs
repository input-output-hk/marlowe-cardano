module Test.Marlowe.Run.Action.Scenarios.Wallet where

import Prologue

import Data.BigInt.Argonaut as BigInt
import Data.Newtype (unwrap)
import Data.WalletNickname as WN
import Test.Data.Marlowe (newMnemonicPhrase, newWalletInfo, walletNickname)
import Test.Marlowe.Run (fundWallet, marloweRunTest)
import Test.Marlowe.Run.Action.Flows (createWallet, dropWallet, restoreWallet)
import Test.Marlowe.Run.Commands (openMyWalletDialog)
import Test.Marlowe.Run.Queries
  ( getWalletBalance
  , getWalletName
  , getWalletStatus
  )
import Test.Spec (Spec)
import Test.Web.DOM.Assertions (shouldHaveText)

createAndRestoreWallet :: Spec Unit
createAndRestoreWallet = marloweRunTest "Create and Restore Wallet" do
  -- Arrange
  name <- walletNickname "Wallet1"
  mnemonic <- newMnemonicPhrase
  walletInfo <- newWalletInfo

  -- Act
  _ <- createWallet name mnemonic walletInfo

  -- Assert
  openMyWalletDialog do
    heading <- getWalletName
    balance <- getWalletBalance
    status <- getWalletStatus

    heading `shouldHaveText` WN.toString name
    balance `shouldHaveText` "₳ 0.000000"
    status `shouldHaveText` "Out of sync"

    fundWallet name "" "" $ BigInt.fromInt 1000000000

    balance `shouldHaveText` "₳ 1,000.000000"
    status `shouldHaveText` "Synchronized"

  -- Act
  dropWallet (unwrap walletInfo).walletId
  _ <- restoreWallet name []

  -- Assert
  openMyWalletDialog do
    heading <- getWalletName
    balance <- getWalletBalance
    status <- getWalletStatus

    heading `shouldHaveText` WN.toString name
    balance `shouldHaveText` "₳ 1,000.000000"
    status `shouldHaveText` "Synchronized"

  dropWallet (unwrap walletInfo).walletId
