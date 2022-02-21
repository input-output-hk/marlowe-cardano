module Store where

import Prologue

import Data.AddressBook (AddressBook)
import Data.ContractNickname (ContractNickname)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.LocalContractNicknames (LocalContractNicknames)
import Data.Map (Map)
import Data.Tuple.Nested (type (/\))
import Data.UUID.Argonaut (UUID)
import Marlowe.Client (ContractHistory)
import Marlowe.Execution.Types as Execution
import Marlowe.Extended.Metadata (MetaData)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (MarloweData, MarloweParams, Slot)
import Store.Contracts
  ( ContractStore
  , addFollowerContract
  , addStartingContract
  , advanceToSlot
  , mkContractStore
  , modifyContract
  , modifyContractNicknames
  )
import Store.Wallet (WalletAction, WalletStore)
import Store.Wallet as Wallet
import Toast.Types (ToastMessage)
import Type.Proxy (Proxy(..))

type Store =
  { addressBook :: AddressBook
  , wallet :: WalletStore
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

_wallet :: forall r a. Lens' { wallet :: a | r } a
_wallet = prop (Proxy :: _ "wallet")

_contracts :: forall r a. Lens' { contracts :: a | r } a
_contracts = prop (Proxy :: _ "contracts")

mkStore :: AddressBook -> LocalContractNicknames -> Store
mkStore addressBook contractNicknames =
  { -- # Wallet
    addressBook
  , wallet: Wallet.Disconnected
  -- # Contracts
  , contracts: mkContractStore contractNicknames
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
  | AddFollowerContract Slot PlutusAppId MetaData ContractHistory
  | ModifyContractNicknames (LocalContractNicknames -> LocalContractNicknames)
  | ModifySyncedContract MarloweParams (Execution.State -> Execution.State)
  -- Address book
  | ModifyAddressBook (AddressBook -> AddressBook)
  -- Wallet
  | Wallet WalletAction
  -- System wide components
  | ShowToast ToastMessage
  | ClearToast
  | DropdownOpened String
  | DropdownClosed

reduce :: Store -> Action -> Store
reduce store = case _ of
  -- Backend Notifications
  AdvanceToSlot newSlot -> store
    { currentSlot = newSlot
    , contracts = advanceToSlot newSlot store.contracts
    }
  NewCompanionAppStateObserved state ->
    store { previousCompanionAppState = Just state }
  -- Contract
  AddStartingContract startingContractInfo -> store
    { contracts =
        addStartingContract
          startingContractInfo
          store.contracts
    }
  AddFollowerContract currentSlot followerId metadata history -> store
    { contracts =
        addFollowerContract
          currentSlot
          followerId
          metadata
          history
          store.contracts
    }
  ModifyContractNicknames f -> store
    { contracts = modifyContractNicknames f store.contracts
    }
  ModifySyncedContract marloweParams f -> store
    { contracts = modifyContract marloweParams f store.contracts
    }
  -- Address book
  ModifyAddressBook f -> store { addressBook = f store.addressBook }
  -- Wallet
  Wallet action -> store { wallet = Wallet.reduce store.wallet action }
  -- Toast
  ShowToast msg -> store { toast = Just msg }
  ClearToast -> store { toast = Nothing }
  -- Dropdown
  DropdownOpened dropdown -> store { openDropdown = Just dropdown }
  DropdownClosed -> store { openDropdown = Nothing }
