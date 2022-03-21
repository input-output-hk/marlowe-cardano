module Store where

import Prologue

import Data.AddressBook (AddressBook)
import Data.DateTime.Instant (Instant)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.LocalContractNicknames (LocalContractNicknames)
import Data.Maybe (maybe)
import Data.NewContract (NewContract)
import Data.Set (Set)
import Data.Wallet (WalletDetails)
import Language.Marlowe.Client (ContractHistory)
import Marlowe.Execution.Types as Execution
import Marlowe.Extended.Metadata (MetaData)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (MarloweParams)
import Store.Contracts
  ( ContractStore
  , contractCreated
  , contractStarted
  , emptyContractStore
  , historyUpdated
  , initialFollowersReceived
  , mkContractStore
  , modifyContract
  , modifyContractNicknames
  , tick
  )
import Store.Wallet (WalletAction, WalletStore)
import Store.Wallet as Wallet
import Toast.Types (ToastMessage, errorToast)
import Type.Proxy (Proxy(..))

type Store =
  { addressBook :: AddressBook
  , wallet :: WalletStore
  -- # Contracts
  , contracts :: ContractStore
  -- # Backend Notifications
  , currentTime :: Instant
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

mkStore
  :: Instant
  -> AddressBook
  -> LocalContractNicknames
  -> Maybe WalletDetails
  -> Store
mkStore currentTime addressBook contractNicknames wallet =
  { -- # Wallet
    addressBook
  , wallet: maybe Wallet.Disconnected Wallet.Connecting wallet
  -- # Contracts
  , contracts: mkContractStore contractNicknames
  -- # Time
  , currentTime
  -- # System wide components
  , openDropdown: Nothing
  , toast: Nothing
  }

data Action
  -- Time
  = Tick Instant
  -- Contract
  | InitialFollowersReceived (Set (Tuple MarloweParams PlutusAppId))
  | ContractCreated NewContract
  | ContractHistoryUpdated PlutusAppId MetaData ContractHistory
  | ModifyContractNicknames (LocalContractNicknames -> LocalContractNicknames)
  | ModifySyncedContract MarloweParams (Execution.State -> Execution.State)
  | ContractStarted NewContract MarloweParams
  -- Address book
  | ModifyAddressBook (AddressBook -> AddressBook)
  -- Wallet
  | Wallet WalletAction
  -- System wide components
  | Disconnect
  | ShowToast ToastMessage
  | ClearToast
  | DropdownOpened String
  | DropdownClosed

reduce :: Store -> Action -> Store
reduce store = case _ of
  -- Time
  Tick currentTime -> case tick currentTime store.contracts of
    Left error -> reduce store
      $ ShowToast
      $ errorToast "Error updating contract state with new time"
      $ Just error
    Right contracts -> store
      { currentTime = currentTime
      , contracts = contracts
      }
  -- Contract
  InitialFollowersReceived followers -> store
    { contracts = initialFollowersReceived followers store.contracts
    }
  ContractCreated startingContractInfo -> store
    { contracts = contractCreated startingContractInfo store.contracts
    }
  ContractHistoryUpdated followerId metadata history ->
    let
      mContracts = historyUpdated
        store.currentTime
        followerId
        metadata
        history
        store.contracts
    in
      case mContracts of
        Left error -> reduce store
          $ ShowToast
          $ errorToast "Error adding follower contract"
          $ Just error
        Right contracts -> store { contracts = contracts }
  ModifyContractNicknames f -> store
    { contracts = modifyContractNicknames f store.contracts
    }
  ModifySyncedContract marloweParams f -> store
    { contracts = modifyContract marloweParams f store.contracts
    }
  ContractStarted newContract marloweParams ->
    store
      { contracts = contractStarted newContract marloweParams store.contracts
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
  Disconnect -> store
    { wallet = Wallet.Disconnected
    , contracts = emptyContractStore
    }
