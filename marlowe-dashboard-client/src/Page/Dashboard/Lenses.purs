module Page.Dashboard.Lenses
  ( _contactsState
  , _walletDetails
  , _walletCompanionStatus
  , _menuOpen
  , _card
  , _cardOpen
  , _contracts
  , _contract
  , _contractFilter
  , _selectedContractFollowerAppId
  , _selectedContract
  , _templateState
  ) where

import Prologue
import Component.Contacts.Types (State) as Contacts
import Component.Contacts.Types (WalletDetails)
import Component.Template.Types (State) as Template
import Page.Dashboard.Types (Card, ContractFilter, State, WalletCompanionStatus)
import Data.Lens (Lens', Traversal', set, wander)
import Data.Lens.At (at)
import Data.Lens.Prism.Maybe (_Just)
import Data.Lens.Record (prop)
import Data.Map (Map, insert, lookup)
import Type.Proxy (Proxy(..))
import Marlowe.PAB (PlutusAppId)
import Page.Contract.Types (State) as Contract

_contactsState :: Lens' State Contacts.State
_contactsState = prop (Proxy :: _ "contactsState")

_walletDetails :: Lens' State WalletDetails
_walletDetails = prop (Proxy :: _ "walletDetails")

_walletCompanionStatus :: Lens' State WalletCompanionStatus
_walletCompanionStatus = prop (Proxy :: _ "walletCompanionStatus")

_menuOpen :: Lens' State Boolean
_menuOpen = prop (Proxy :: _ "menuOpen")

_card :: Lens' State (Maybe Card)
_card = prop (Proxy :: _ "card")

_cardOpen :: Lens' State Boolean
_cardOpen = prop (Proxy :: _ "cardOpen")

_contracts :: Lens' State (Map PlutusAppId Contract.State)
_contracts = prop (Proxy :: _ "contracts")

_contract :: PlutusAppId -> Traversal' State Contract.State
_contract followerAppId = _contracts <<< at followerAppId <<< _Just

_contractFilter :: Lens' State ContractFilter
_contractFilter = prop (Proxy :: _ "contractFilter")

_selectedContractFollowerAppId :: Lens' State (Maybe PlutusAppId)
_selectedContractFollowerAppId = prop
  (Proxy :: _ "selectedContractFollowerAppId")

-- This traversal focus on a specific contract indexed by another property of the state
_selectedContract :: Traversal' State Contract.State
_selectedContract =
  wander \f state -> case state.selectedContractFollowerAppId of
    Just ix
      | Just contract <- lookup ix state.contracts ->
          let
            updateContract contract' = insert ix contract' state.contracts
          in
            (\contract' -> set _contracts (updateContract contract') state) <$>
              f contract
    _ -> pure state

_templateState :: Lens' State Template.State
_templateState = prop (Proxy :: _ "templateState")
