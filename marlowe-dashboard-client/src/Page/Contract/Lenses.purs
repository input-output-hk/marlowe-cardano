module Page.Contract.Lenses
  ( _Started
  , _Starting
  , _contract
  , _contractUserParties
  , _executionState
  , _expandPayments
  , _marloweParams
  , _metadata
  , _namedActions
  , _nickname
  , _previousSteps
  , _resultingPayments
  , _selectedStep
  , _tab
  ) where

import Prologue

import Data.Lens (Lens', Prism', prism')
import Data.Lens.Record (prop)
import Page.Contract.Types (ContractState(..), StartedState, StartingState)
import Type.Proxy (Proxy(..))

_contract :: forall a r. Lens' { contract :: a | r } a
_contract = prop (Proxy :: _ "contract")

_Starting :: Prism' ContractState StartingState
_Starting =
  prism'
    Starting
    ( case _ of
        Starting s -> Just s
        _ -> Nothing
    )

_Started :: Prism' ContractState StartedState
_Started =
  prism'
    Started
    ( case _ of
        Started s -> Just s
        _ -> Nothing
    )

_nickname :: forall a r. Lens' { nickname :: a | r } a
_nickname = prop (Proxy :: _ "nickname")

_tab :: forall a r. Lens' { tab :: a | r } a
_tab = prop (Proxy :: _ "tab")

_executionState :: forall a r. Lens' { executionState :: a | r } a
_executionState = prop (Proxy :: _ "executionState")

_previousSteps :: forall a r. Lens' { previousSteps :: a | r } a
_previousSteps = prop (Proxy :: _ "previousSteps")

_marloweParams :: forall a r. Lens' { marloweParams :: a | r } a
_marloweParams = prop (Proxy :: _ "marloweParams")

_selectedStep :: forall a r. Lens' { selectedStep :: a | r } a
_selectedStep = prop (Proxy :: _ "selectedStep")

_metadata :: forall a r. Lens' { metadata :: a | r } a
_metadata = prop (Proxy :: _ "metadata")

_contractUserParties :: forall a r. Lens' { contractUserParties :: a | r } a
_contractUserParties = prop (Proxy :: _ "contractUserParties")

_namedActions :: forall a r. Lens' { namedActions :: a | r } a
_namedActions = prop (Proxy :: _ "namedActions")

_expandPayments :: forall a r. Lens' { expandPayments :: a | r } a
_expandPayments = prop (Proxy :: _ "expandPayments")

_resultingPayments :: forall a r. Lens' { resultingPayments :: a | r } a
_resultingPayments = prop (Proxy :: _ "resultingPayments")
