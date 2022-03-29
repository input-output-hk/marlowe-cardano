module Test.Marlowe.Run.Action.Scenarios.Contract where

import Prologue

import Data.AddressBook (AddressBook(..))
import Data.Bimap as Bimap
import Test.Data.Marlowe
  ( makeTestContractNickname
  , makeTestWalletNickname
  , newAddress
  , newMarloweParams
  )
import Test.Marlowe.Run (defaultTestParameters, getWallet, marloweRunTestWith)
import Test.Marlowe.Run.Action.Flows (createLoan, restoreWallet)
import Test.Spec (Spec)
import Test.Web.DOM.Query (getBy, nameRegexi, role)
import Web.ARIA (ARIARole(..))

loanContract :: Spec Unit
loanContract = marloweRunTestWith "Run a loan contract" setupWallets do
  -- Arrange
  lenderNickname <- makeTestWalletNickname "Lender"
  borrowerNickname <- makeTestWalletNickname "Borrower"
  lenderApps <- restoreWallet lenderNickname []
  lenderWallet <- getWallet lenderNickname
  marloweParams <- newMarloweParams
  contractNickname <- makeTestContractNickname "Test loan"
  _followerId <- createLoan
    lenderWallet
    lenderApps
    marloweParams
    contractNickname
    borrowerNickname
    lenderNickname
    1000
    100
  void $ getBy role do
    nameRegexi "Test loan"
    pure Listitem
  -- TODO complete the contract
  where
  setupWallets = do
    borrowerNickname <- makeTestWalletNickname "Borrower"
    lenderNickname <- makeTestWalletNickname "Lender"
    borrowerAddress <- newAddress
    lenderAddress <- newAddress
    pure $ defaultTestParameters
      { addressBook = AddressBook
          $ Bimap.fromFoldable
              [ Tuple borrowerNickname borrowerAddress
              , Tuple lenderNickname lenderAddress
              ]
      }
