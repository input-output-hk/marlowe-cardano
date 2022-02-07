module Page.Welcome.State
  ( initialState
  , handleAction
  ) where

import Prologue

import Capability.MainFrameLoop (class MainFrameLoop, callMainFrameAction)
import Capability.Marlowe (class ManageMarlowe)
import Capability.MarloweStorage
  ( class ManageMarloweStorage
  , clearAllLocalStorage
  , modifyAddressBook_
  )
import Capability.Toast (class Toast, addToast)
import Clipboard (class MonadClipboard)
import Clipboard (handleAction) as Clipboard
import Control.Monad.Reader (class MonadAsk)
import Data.Address as A
import Data.AddressBook as AddressBook
import Data.Lens (assign, view)
import Data.PaymentPubKeyHash (_PaymentPubKeyHash)
import Data.Wallet (_pubKeyHash, _walletNickname)
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen (HalogenM, liftEffect, modify_)
import Halogen.Query.HalogenM (mapAction)
import MainFrame.Types (Action(..)) as MainFrame
import MainFrame.Types (ChildSlots, Msg)
import Page.Welcome.Lenses (_enteringDashboardState)
import Page.Welcome.Types (Action(..), State)
import Toast.Types (successToast)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)

initialState :: State
initialState =
  { card: Nothing
  , cardOpen: false
  , enteringDashboardState: false
  }

-- Some actions are handled in `MainFrame.State` because they involve
-- modifications of that state. See Note [State] in MainFrame.State.
handleAction
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => MainFrameLoop m
  => ManageMarlowe m
  => ManageMarloweStorage m
  => Toast m
  => MonadClipboard m
  => Action
  -> HalogenM State Action ChildSlots Msg m Unit
handleAction (OpenCard card) =
  modify_ _ { card = Just card, cardOpen = true }

handleAction CloseCard =
  modify_ _ { enteringDashboardState = false, cardOpen = false }

{- [UC-WALLET-TESTNET-1][0] Create a new testnet wallet
   [UC-WALLET-TESTNET-2][4a] Restore a testnet wallet
This action is triggered either after the restore wallet form or create wallet form submit.
In both cases we receive a fully populated wallet details here so we can add them to the address
book. Then we enter the dashboard.

Both `createWallet` and `restoreWallet` (performed during respective form submition)
functions don't just create or restore a wallet.  They also create and attach or just attach to the
two PAB apps for that wallet: a `WalletCompanion` and a `MarloweApp`.
- The `WalletCompanion` will watch for any new role tokens paid to this wallet, and then update its
  internal state to include the `MarloweParams` and initial `MarloweData` for the corresponding
  contract.
- The `MarloweApp` is a control app, used to create Marlowe contracts, apply inputs, and redeem
  payments to this wallet.
-}
handleAction (ConnectWallet walletDetails) = do
  assign _enteringDashboardState true
  let
    walletNickname = view _walletNickname walletDetails
    pubKeyHash = view
      (_pubKeyHash <<< _PaymentPubKeyHash)
      walletDetails

  modifyAddressBook_
    (AddressBook.insert walletNickname $ A.fromPubKeyHash pubKeyHash)
  callMainFrameAction $ MainFrame.EnterDashboardState walletDetails

handleAction ClearLocalStorage = do
  clearAllLocalStorage
  liftEffect do
    location_ <- location =<< window
    reload location_

handleAction (ClipboardAction clipboardAction) = do
  mapAction ClipboardAction $ Clipboard.handleAction clipboardAction
  addToast $ successToast "Copied to clipboard"
