module Test.Marlowe.Run.Action.Scenarios.CreateAndRestoreWallet where

import Prologue

import Control.Logger.Capability (class MonadLogger)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (class MonadAsk)
import Control.Monad.UUID (class MonadUUID)
import Data.Address as Address
import Data.Newtype (unwrap)
import Data.WalletNickname as WN
import Effect.Aff (Error)
import Effect.Class (class MonadEffect, liftEffect)
import Test.Data.Marlowe (newMnemonicPhrase, newWalletInfo, walletNickname)
import Test.Marlowe.Run (Coenv)
import Test.Marlowe.Run.Action.Flows (createWallet)
import Test.Marlowe.Run.Commands (openMyWalletDialog)
import Test.Marlowe.Run.Queries
  ( getWalletAddress
  , getWalletBalance
  , getWalletName
  , getWalletStatus
  )
import Test.Network.HTTP (class MonadMockHTTP)
import Test.Spec.Assertions (shouldEqual)
import Test.Web.DOM.Assertions (shouldHaveText)
import Test.Web.Event.User.Monad (class MonadUser)
import Test.Web.Monad (class MonadTest)
import Web.HTML.HTMLInputElement as Input

createAndRestoreWallet
  :: forall m
   . Bind m
  => MonadError Error m
  => MonadEffect m
  => MonadTest m
  => MonadLogger String m
  => MonadUser m
  => MonadUUID m
  => MonadAsk Coenv m
  => MonadMockHTTP m
  => m Unit
createAndRestoreWallet = do
  -- Arrange
  name <- walletNickname "Wallet1"
  mnemonic <- newMnemonicPhrase
  walletInfo <- newWalletInfo

  -- Act
  _appIds <- createWallet name mnemonic walletInfo

  -- Assert
  openMyWalletDialog do
    heading <- getWalletName
    address <- getWalletAddress
    balance <- getWalletBalance
    status <- getWalletStatus

    heading `shouldHaveText` WN.toString name
    addressValue <- liftEffect $ Input.value address
    addressValue `shouldEqual` Address.toString (unwrap walletInfo).address
    balance `shouldHaveText` "₳ 0.000000"
    status `shouldHaveText` "Synchronized"
