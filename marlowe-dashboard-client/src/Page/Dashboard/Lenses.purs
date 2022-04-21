module Page.Dashboard.Lenses where

import Prologue

import Component.Template.Types (State) as Template
import Data.ContractStatus (ContractStatusId)
import Data.DateTime.Instant (Instant)
import Data.Lens (Fold', Lens', filtered, folded)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.NewContract (NewContract)
import Data.PABConnectedWallet (PABConnectedWallet)
import Data.Slot (Slot)
import Data.Time.Duration (Minutes)
import Data.UUID.Argonaut (UUID)
import Halogen.Component.Reactive (_derived, _input, _transient)
import Marlowe.Semantics (MarloweParams)
import Page.Dashboard.Types
  ( Card
  , ContractFilter
  , ContractState
  , State
  , WalletCompanionStatus
  )
import Store.Contracts (ContractStore)
import Store.RoleTokens (RoleTokenStore)
import Type.Proxy (Proxy(..))

_walletCompanionStatus :: Lens' State WalletCompanionStatus
_walletCompanionStatus =
  _transient <<< prop (Proxy :: _ "walletCompanionStatus")

_menuOpen :: Lens' State Boolean
_menuOpen = _transient <<< prop (Proxy :: _ "menuOpen")

_card :: Lens' State (Maybe Card)
_card = _transient <<< prop (Proxy :: _ "card")

_cardOpen :: Lens' State Boolean
_cardOpen = _transient <<< prop (Proxy :: _ "cardOpen")

_contractFilter :: Lens' State ContractFilter
_contractFilter = _transient <<< prop (Proxy :: _ "contractFilter")

_selectedContractIndex :: Lens' State (Maybe ContractStatusId)
_selectedContractIndex = _transient <<< prop
  (Proxy :: _ "selectedContractIndex")

_wallet :: Lens' State PABConnectedWallet
_wallet = _input <<< prop (Proxy :: _ "input")

_contractStore :: Lens' State ContractStore
_contractStore =
  _input <<< prop (Proxy :: _ "context") <<< prop (Proxy :: _ "contracts")

_tipSlot :: Lens' State Slot
_tipSlot =
  _input <<< prop (Proxy :: _ "context") <<< prop (Proxy :: _ "tipSlot")

_roleTokens :: Lens' State RoleTokenStore
_roleTokens =
  _input <<< prop (Proxy :: _ "context") <<< prop (Proxy :: _ "roleTokens")

_currentTime :: Lens' State Instant
_currentTime =
  _input <<< prop (Proxy :: _ "context") <<< prop (Proxy :: _ "currentTime")

_tzOffset :: Lens' State Minutes
_tzOffset = _transient <<< prop (Proxy :: _ "tzOffset")

_templateState :: Lens' State Template.State
_templateState = _transient <<< prop (Proxy :: _ "templateState")

_contracts :: Lens' State (Map MarloweParams ContractState)
_contracts = _derived <<< prop (Proxy :: _ "contracts")

_closedContracts :: forall r. Monoid r => Fold' r State ContractState
_closedContracts = _contracts <<< folded <<< filtered _.isClosed

_runningContracts :: forall r. Monoid r => Fold' r State ContractState
_runningContracts = _contracts <<< folded <<< filtered (not <<< _.isClosed)

_newContracts :: Lens' State (Map UUID NewContract)
_newContracts = _derived <<< prop (Proxy :: _ "newContracts")
