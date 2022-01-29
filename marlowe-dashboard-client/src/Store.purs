module Store
  ( Action(..)
  , Store
  , reduce
  ) where

import Prologue

import Component.Contacts.Lenses (_assets)
import Component.Contacts.Types (WalletDetails)
import Data.AddressBook (AddressBook)
import Data.Lens (_Just, (.~))
import Data.Map (Map)
import Marlowe.Semantics (Assets, MarloweData, MarloweParams, Slot)
import Store.Contract as Contract
import Toast.Types (ToastMessage)

type Store =
  { addressBook :: AddressBook
  , currentSlot :: Slot
  , toast :: Maybe ToastMessage
  , wallet :: Maybe WalletDetails
  -- This contains the state of Marlowe contracts as seen by the backend. Most operations should
  -- trigger an API call that modifies the state of the Blockchain and when the PAB detect a change
  -- it notifies us via the ObservableState of the FollowerContract.
  , contracts :: Contract.Store
  -- this property shouldn't be necessary, but at the moment we are getting too many update notifications
  -- through the PAB - so until that bug is fixed, we use this to check whether an update notification
  -- really has changed anything
  , previousCompanionAppState :: Maybe (Map MarloweParams MarloweData)
  }

data Action
  = AdvanceToSlot Slot
  | ShowToast ToastMessage
  | ModifyAddressBook (AddressBook -> AddressBook)
  | ActivateWallet WalletDetails
  | NewCompanionAppStateObserved (Map MarloweParams MarloweData)
  | UpdateAssets Assets
  | DeactivateWallet
  | ClearToast

reduce :: Store -> Action -> Store
reduce store = case _ of
  -- FIXME: SCP-3208
  AdvanceToSlot newSlot -> store { currentSlot = newSlot }
  ShowToast msg -> store { toast = Just msg }
  ClearToast -> store { toast = Nothing }
  ModifyAddressBook f -> store { addressBook = f store.addressBook }
  ActivateWallet wallet -> store { wallet = Just wallet }
  UpdateAssets assets ->
    store { wallet = store.wallet # _Just <<< _assets .~ assets }
  DeactivateWallet -> store { wallet = Nothing }
  NewCompanionAppStateObserved state ->
    store { previousCompanionAppState = Just state }
