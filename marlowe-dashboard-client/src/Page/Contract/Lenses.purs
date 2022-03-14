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

import Data.ContractStatus (ContractStatus(..))
import Data.Lens (Lens', Prism', lens, prism')
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Maybe (fromMaybe)
import Data.NewContract (NewContract)
import Page.Contract.Types (ContractState, StartedState, Tab(..))
import Type.Proxy (Proxy(..))

_contract :: forall a r. Lens' { contract :: a | r } a
_contract = prop (Proxy :: _ "contract")

_Starting :: Prism' ContractState NewContract
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

_expandPayments :: Int -> Lens' StartedState Boolean
_expandPayments index = prop (Proxy :: _ "expandPayments")
  <<< at index
  <<< lens (fromMaybe false) (const Just)

_tab :: Int -> Lens' StartedState Tab
_tab index = prop (Proxy :: _ "tabs")
  <<< at index
  <<< lens (fromMaybe Tasks) (const Just)

_resultingPayments :: forall a r. Lens' { resultingPayments :: a | r } a
_resultingPayments = prop (Proxy :: _ "resultingPayments")
