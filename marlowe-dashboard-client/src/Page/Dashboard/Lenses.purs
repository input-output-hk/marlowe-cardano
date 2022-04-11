module Page.Dashboard.Lenses where

import Prologue

import Component.Template.Types (State) as Template
import Data.ContractStatus (ContractStatusId)
import Data.DateTime.Instant (Instant)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.NewContract (NewContract)
import Data.PABConnectedWallet (PABConnectedWallet)
import Data.Time.Duration (Minutes)
import Halogen.Component.Reactive (_derived, _input, _transient)
import Page.Dashboard.Types
  ( Card
  , ContractFilter
  , ContractState
  , State
  , WalletCompanionStatus
  )
import Store.Contracts (ContractStore)
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

_contracts :: Lens' State ContractStore
_contracts =
  _input <<< prop (Proxy :: _ "context") <<< prop (Proxy :: _ "contracts")

_currentTime :: Lens' State Instant
_currentTime =
  _input <<< prop (Proxy :: _ "context") <<< prop (Proxy :: _ "currentTime")

_tzOffset :: Lens' State Minutes
_tzOffset = _transient <<< prop (Proxy :: _ "tzOffset")

_templateState :: Lens' State Template.State
_templateState = _transient <<< prop (Proxy :: _ "templateState")

_runningContracts :: Lens' State (Array ContractState)
_runningContracts = _derived <<< prop (Proxy :: _ "runningContracts")

_closedContracts :: Lens' State (Array ContractState)
_closedContracts = _derived <<< prop (Proxy :: _ "closedContracts")

_newContracts :: Lens' State (Array NewContract)
_newContracts = _derived <<< prop (Proxy :: _ "newContracts")
