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
import Component.Contacts.Lenses (_pubKeyHash, _walletInfo, _walletNickname)
import Control.Monad.Reader (class MonadAsk)
import Data.AddressBook as AddressBook
import Data.Lens (assign, set, view)
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
Here we attempt to create a new demo wallet (with everything that entails), and - if successful -
open up the UseNewWalletCard for connecting the wallet just created.
Note the `createWallet` function doesn't just create a wallet. It also creates two PAB apps for
that wallet: a `WalletCompanion` and a `MarloweApp`.
- The `WalletCompanion` will watch for any new role tokens paid to this wallet, and then update its
  internal state to include the `MarloweParams` and initial `MarloweData` for the corresponding
  contract.
- The `MarloweApp` is a control app, used to create Marlowe contracts, apply inputs, and redeem
  payments to this wallet.
-}
-- TODO: This functionality is disabled, I'll re-enable it as part of SCP-3170.
handleAction GenerateWallet = pure unit

{- [Workflow 2][2] Connect a wallet
This action is triggered by clicking the confirmation button on the UseWalletCard or
UseNewWalletCard. It saves the wallet nickname to LocalStorage, and then calls the
`MainFrame.EnterDashboardState` action.
-}
handleAction (ConnectWallet walletNickname walletDetails) = do
  assign _enteringDashboardState true
  let
    walletDetailsWithNickname = set _walletNickname walletNickname walletDetails

    address = view (_walletInfo <<< _pubKeyHash) walletDetailsWithNickname
  modifyAddressBook_ (AddressBook.insert walletNickname address)
  callMainFrameAction $ MainFrame.EnterDashboardState walletDetailsWithNickname

handleAction ClearLocalStorage = do
  clearAllLocalStorage
  liftEffect do
    location_ <- location =<< window
    reload location_

handleAction (ClipboardAction clipboardAction) = do
  mapAction ClipboardAction $ Clipboard.handleAction clipboardAction
  addToast $ successToast "Copied to clipboard"
