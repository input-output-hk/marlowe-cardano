-- Queries are reusable DOM queries that fetch specific UI elements.
module Test.Marlowe.Run.Queries where

import Prologue

import Control.Monad.Error.Class (class MonadError)
import Effect.Aff (Error)
import Test.Web.DOM.Assertions (shouldCast)
import Test.Web.DOM.Query (getBy, nameRegexi, role)
import Test.Web.Monad (class MonadTest)
import Web.ARIA (ARIARole(..))
import Web.DOM (Element)
import Web.HTML (HTMLInputElement)

-------------------------------------------------------------------------------
-- Dashboard Page - My Wallet
-------------------------------------------------------------------------------

getWalletName :: forall m. MonadTest m => MonadError Error m => m Element
getWalletName = getBy role do
  nameRegexi "wallet-name"
  pure Heading

getWalletAddress
  :: forall m. MonadTest m => MonadError Error m => m HTMLInputElement
getWalletAddress = shouldCast =<< getBy role do
  nameRegexi "wallet address"
  pure Textbox

getWalletBalance
  :: forall m. MonadTest m => MonadError Error m => m Element
getWalletBalance = getBy role do
  nameRegexi "balance"
  pure Cell

getWalletStatus
  :: forall m. MonadTest m => MonadError Error m => m Element
getWalletStatus = getBy role do
  nameRegexi "status"
  pure Cell
