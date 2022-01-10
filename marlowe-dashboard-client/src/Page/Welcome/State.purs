module Page.Welcome.State
  ( dummyState
  , mkInitialState
  , handleAction
  ) where

import Prologue

import Capability.MainFrameLoop (class MainFrameLoop, callMainFrameAction)
import Capability.Marlowe (class ManageMarlowe)
import Capability.MarloweStorage
  ( class ManageMarloweStorage
  , clearAllLocalStorage
  , insertIntoAddressBook
  )
import Capability.Toast (class Toast, addToast)
import Clipboard (class MonadClipboard)
import Clipboard (handleAction) as Clipboard
import Component.Contacts.Lenses (_pubKeyHash, _walletInfo, _walletNickname)
import Component.Contacts.Types (AddressBook)
import Control.Monad.Reader (class MonadAsk)
import Data.Lens (assign, modifying, set, use, view)
import Data.Map (insert)
import Data.Map as Map
import Data.UUID.Argonaut (emptyUUID) as UUID
import Data.WalletNickname as WN
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen (HalogenM, liftEffect, modify_)
import Halogen.Query.HalogenM (mapAction)
import MainFrame.Types (Action(..)) as MainFrame
import MainFrame.Types (ChildSlots, Msg)
import Marlowe.PAB (PlutusAppId(..))
import Page.Welcome.Lenses
  ( _addressBook
  , _card
  , _cardOpen
  , _enteringDashboardState
  , _walletId
  )
import Page.Welcome.Types (Action(..), State)
import Toast.Types (successToast)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)

-- see note [dummyState] in MainFrame.State
dummyState :: State
dummyState = mkInitialState Map.empty

mkInitialState :: AddressBook -> State
mkInitialState addressBook =
  { addressBook
  , card: Nothing
  , cardOpen: false
  , walletId: PlutusAppId UUID.emptyUUID
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
handleAction (OpenCard card) = do
  modify_ $ set _card (Just card) <<< set _cardOpen true

handleAction CloseCard = do
  modify_
    $ set _enteringDashboardState false
        <<< set _cardOpen false
        <<< set _walletId dummyState.walletId

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

-- TODO: SCP-3218 We'll most likely won't need the [Workflow 2][X] connect wallet features, but I'll remove them
--       once the new flow is fully functional.
{- [Workflow 2][1] Connect a wallet
If we are connecting a wallet that was selected by the user inputting a wallet nickname, then we
will have a cache of it's `WalletDetails` in LocalStorage. But those details may well be out of
date. This intermediate step makes sure we have the current details before proceeding.
This is also factored out into a separate handler so that it can be called directly when the user
selects a wallet nickname from the dropdown menu (as well as indirectly via the previous handler).
-}
-- handleAction (OpenUseWalletCardWithDetails walletDetails) = do
--   assign _remoteWalletDetails Loading
--   ajaxWalletDetails <- lookupWalletDetails $ view _companionAppId walletDetails
--   assign _remoteWalletDetails $ fromEither ajaxWalletDetails
--   case ajaxWalletDetails of
--     Left _ -> handleAction $ OpenCard LocalWalletMissingCard
--     Right _ -> do
--       handleAction $ WalletNicknameInputAction $ InputField.SetValue $ view
--         _walletNickname
--         walletDetails
--       assign _walletId $ walletDetails ^. _companionAppId
--       handleAction $ OpenCard UseWalletCard

{- [Workflow 2][2] Connect a wallet
This action is triggered by clicking the confirmation button on the UseWalletCard or
UseNewWalletCard. It saves the wallet nickname to LocalStorage, and then calls the
`MainFrame.EnterDashboardState` action.
-}
handleAction (ConnectWallet walletNickname walletDetails) = do
  assign _enteringDashboardState true
  let
    walletDetailsWithNickname = set _walletNickname
      (WN.toString walletNickname)
      walletDetails

    address = view (_walletInfo <<< _pubKeyHash) walletDetailsWithNickname
  modifying _addressBook $ insert (WN.toString walletNickname) address
  insertIntoAddressBook (WN.toString walletNickname) address
  addressBook <- use _addressBook
  callMainFrameAction $ MainFrame.EnterDashboardState addressBook
    walletDetailsWithNickname

handleAction ClearLocalStorage = do
  clearAllLocalStorage
  liftEffect do
    location_ <- location =<< window
    reload location_

handleAction (ClipboardAction clipboardAction) = do
  mapAction ClipboardAction $ Clipboard.handleAction clipboardAction
  addToast $ successToast "Copied to clipboard"
