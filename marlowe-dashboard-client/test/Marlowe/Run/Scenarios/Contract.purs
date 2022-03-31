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
import Test.Marlowe.Run.Commands (clickButtonRegex, clickConfirm)
import Test.Spec (Spec)
import Test.Web.DOM.Query (findBy, getBy, nameRegexi, role)
import Test.Web.Monad (withContainer)
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
  card <- getBy role do
    nameRegexi "Test loan"
    pure Listitem
  withContainer card $ clickButtonRegex "deposit"
  confirmDialog <- findBy role $ pure Dialog
  withContainer confirmDialog clickConfirm
  pure unit
  -- reqId <- getNextUUID
  -- let
  --   interval = TimeInterval (POSIXTime unixEpoch) (POSIXTime unixEpoch)
  --   depositInput = TransactionInput { inputs: mempty, interval }
  -- handlePostApplyInputs lenderApps.marloweAppId reqId marloweParams depositInput

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
