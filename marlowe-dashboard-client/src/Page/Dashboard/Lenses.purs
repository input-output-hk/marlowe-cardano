module Page.Dashboard.Lenses
  ( _card
  , _cardOpen
  , _closedContracts
  , _contactsState
  , _contractFilter
  , _menuOpen
  , _runningContracts
  , _selectedContractMarloweParams
  , _templateState
  , _walletCompanionStatus
  ) where

import Prologue

import Component.Contacts.Types (State) as Contacts
import Component.Template.Types (State) as Template
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Marlowe.Semantics (MarloweParams)
import Page.Dashboard.Types
  ( Card
  , ContractFilter
  , ContractState
  , State
  , WalletCompanionStatus
  )
import Type.Proxy (Proxy(..))

_contactsState :: Lens' State Contacts.State
_contactsState = prop (Proxy :: _ "contactsState")

_walletCompanionStatus :: Lens' State WalletCompanionStatus
_walletCompanionStatus = prop (Proxy :: _ "walletCompanionStatus")

_menuOpen :: Lens' State Boolean
_menuOpen = prop (Proxy :: _ "menuOpen")

_card :: Lens' State (Maybe Card)
_card = prop (Proxy :: _ "card")

_cardOpen :: Lens' State Boolean
_cardOpen = prop (Proxy :: _ "cardOpen")

_contractFilter :: Lens' State ContractFilter
_contractFilter = prop (Proxy :: _ "contractFilter")

_selectedContractMarloweParams :: Lens' State (Maybe MarloweParams)
_selectedContractMarloweParams = prop
  (Proxy :: _ "selectedContractMarloweParams")

_templateState :: Lens' State Template.State
_templateState = prop (Proxy :: _ "templateState")

_runningContracts :: Lens' State (Array ContractState)
_runningContracts = prop (Proxy :: _ "runningContracts")

_closedContracts :: Lens' State (Array ContractState)
_closedContracts = prop (Proxy :: _ "closedContracts")

