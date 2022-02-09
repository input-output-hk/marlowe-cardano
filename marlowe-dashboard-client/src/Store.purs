module Store
  ( Action(..)
  , Store
  , mkStore
  , reduce
  ) where

import Prologue

import Data.AddressBook (AddressBook)
import Data.ContractNickname (ContractNickname)
import Data.Lens (_Just, (.~))
import Data.Map (Map)
import Data.PABConnectedWallet
  ( PABConnectedWallet
  , _assets
  , _companionAppId
  , _marloweAppId
  , _syncStatus
  )
import Data.Tuple.Nested (type (/\))
import Data.UUID.Argonaut (UUID)
import Data.Wallet (SyncStatus)
import Marlowe.Client (ContractHistory(..))
import Marlowe.Extended.Metadata (MetaData)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (Assets, MarloweData, MarloweParams, Slot)
import MarloweContract (MarloweContract(..))
import Store.Contracts
  ( ContractStore
  , addFollowerContract
  , addStartingContract
  , emptyContractStore
  )
import Toast.Types (ToastMessage)

type Store =
  { -- # Wallet
    addressBook :: AddressBook
  , wallet :: Maybe PABConnectedWallet
  -- # Contracts
  , contracts :: ContractStore
  -- # Backend Notifications
  -- this property shouldn't be necessary, but at the moment we are getting too many update notifications
  -- through the PAB - so until that bug is fixed, we use this to check whether an update notification
  -- really has changed anything
  , previousCompanionAppState :: Maybe (Map MarloweParams MarloweData)
  , currentSlot :: Slot
  -- # System wide components
  -- This is to make sure only one dropdown at a time is open, in order to
  -- overcome a limitation of nselect that prevents it from closing the
  -- dropdown on blur.
  , openDropdown :: Maybe String
  , toast :: Maybe ToastMessage
  }

mkStore :: AddressBook -> Store
mkStore addressBook =
  { -- # Wallet
    addressBook
  , wallet: Nothing
  -- # Contracts
  , contracts: emptyContractStore
  -- # Backend Notifications
  , previousCompanionAppState: Nothing
  , currentSlot: zero
  -- # System wide components
  , openDropdown: Nothing
  , toast: Nothing
  }

data Action
  -- Backend Notifications
  = AdvanceToSlot Slot
  | NewCompanionAppStateObserved (Map MarloweParams MarloweData)
  -- Contract
  | AddStartingContract (UUID /\ ContractNickname /\ MetaData)
  | AddFollowerContract Slot PlutusAppId ContractHistory
  -- Address book
  | ModifyAddressBook (AddressBook -> AddressBook)
  -- Wallet
  | ActivateWallet PABConnectedWallet
  | ChangePlutusScript MarloweContract PlutusAppId
  | UpdateAssets Assets
  | UpdateWalletSyncStatus SyncStatus
  | DeactivateWallet
  -- System wide components
  | ShowToast ToastMessage
  | ClearToast
  | DropdownOpened String
  | DropdownClosed

reduce :: Store -> Action -> Store
reduce store = case _ of
  -- Backend Notifications
  -- FIXME: SCP-3208
  AdvanceToSlot newSlot -> store { currentSlot = newSlot }
  NewCompanionAppStateObserved state ->
    store { previousCompanionAppState = Just state }
  -- Contract
  AddStartingContract startingContractInfo -> store
    { contracts = addStartingContract startingContractInfo
        store.contracts
    }
  AddFollowerContract currentSlot followerId history -> store
    { contracts = addFollowerContract currentSlot followerId history
        store.contracts
    }
  -- Address book
  ModifyAddressBook f -> store { addressBook = f store.addressBook }
  -- Wallet
  ActivateWallet wallet -> store { wallet = Just wallet }
  ChangePlutusScript MarloweApp plutusAppId ->
    store { wallet = store.wallet # _Just <<< _marloweAppId .~ plutusAppId }
  ChangePlutusScript WalletCompanion plutusAppId ->
    store { wallet = store.wallet # _Just <<< _companionAppId .~ plutusAppId }
  ChangePlutusScript MarloweFollower _ -> store
  UpdateAssets assets ->
    store { wallet = store.wallet # _Just <<< _assets .~ assets }
  UpdateWalletSyncStatus sync ->
    store { wallet = store.wallet # _Just <<< _syncStatus .~ sync }
  DeactivateWallet -> store { wallet = Nothing }
  -- Toast
  ShowToast msg -> store { toast = Just msg }
  ClearToast -> store { toast = Nothing }
  -- Dropdown
  DropdownOpened dropdown -> store { openDropdown = Just dropdown }
  DropdownClosed -> store { openDropdown = Nothing }
