module Page.Dashboard.Lenses
  ( _card
  , _cardOpen
  , _contactsState
  , _contract
  , _contractFilter
  , _contracts
  , _menuOpen
  , _selectedContract
  , _selectedContractMarloweParams
  , _templateState
  , _walletCompanionStatus
  ) where

import Prologue

import Component.Contacts.Types (State) as Contacts
import Component.Template.Types (State) as Template
import Data.Lens (Lens', Traversal', set, wander)
import Data.Lens.At (at)
import Data.Lens.Prism.Maybe (_Just)
import Data.Lens.Record (prop)
import Data.Map (Map, insert, lookup)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (MarloweParams(..))
import Page.Contract.Types (State) as Contract
import Page.Dashboard.Types (Card, ContractFilter, State, WalletCompanionStatus)
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

_contracts :: Lens' State (Map MarloweParams Contract.State)
_contracts = prop (Proxy :: _ "contracts")

-- FIXME-3208 remove
_contract :: MarloweParams -> Traversal' State Contract.State
_contract followerAppId = _contracts <<< at followerAppId <<< _Just

_contractFilter :: Lens' State ContractFilter
_contractFilter = prop (Proxy :: _ "contractFilter")

_selectedContractMarloweParams :: Lens' State (Maybe MarloweParams)
_selectedContractMarloweParams = prop
  (Proxy :: _ "selectedContractMarloweParams")

-- This traversal focus on a specific contract indexed by another property of the state
_selectedContract :: Traversal' State Contract.State
_selectedContract =
  wander \f state -> case state.selectedContractMarloweParams of
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
