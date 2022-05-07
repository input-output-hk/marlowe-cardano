module Simulator.Lenses where

import Prologue

import Data.Lens (Lens', Optic', Prism', Traversal', _Just, prism)
import Data.Lens.Index (ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.NonEmptyList (_Head)
import Data.Lens.Record (prop)
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Marlowe.Holes (Contract, Term)
import Marlowe.Semantics (Party)
import Simulator.Types
  ( ActionInput
  , ActionInputId
  , ExecutionState(..)
  , ExecutionStateRecord
  , InitialConditionsRecord
  , MarloweState
  , PartiesAction
  , otherActionsParty
  )
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------
-- ActionInput and ActionInputId Lenses
--
_actionInput :: Party -> ActionInputId -> Traversal' PartiesAction ActionInput
_actionInput party id = _Newtype <<< ix party <<< ix id

_otherActions :: Traversal' PartiesAction (Map ActionInputId ActionInput)
_otherActions = _Newtype <<< ix otherActionsParty

--------------------------------------------------------------------------
-- ExecutionStateRecord Lenses
--
_possibleActions :: forall s a. Lens' { possibleActions :: a | s } a
_possibleActions = prop (Proxy :: _ "possibleActions")

_pendingInputs :: forall s a. Lens' { pendingInputs :: a | s } a
_pendingInputs = prop (Proxy :: _ "pendingInputs")

_state :: forall s a. Lens' { state :: a | s } a
_state = prop (Proxy :: _ "state")

_transactionError :: forall s a. Lens' { transactionError :: a | s } a
_transactionError = prop (Proxy :: _ "transactionError")

_transactionWarnings :: forall s a. Lens' { transactionWarnings :: a | s } a
_transactionWarnings = prop (Proxy :: _ "transactionWarnings")

_time :: forall s a. Lens' { time :: a | s } a
_time = prop (Proxy :: _ "time")

_moneyInContract :: forall s a. Lens' { moneyInContract :: a | s } a
_moneyInContract = prop (Proxy :: _ "moneyInContract")

_contract :: forall s a. Lens' { contract :: a | s } a
_contract = prop (Proxy :: _ "contract")

_log :: forall s a. Lens' { log :: a | s } a
_log = prop (Proxy :: _ "log")

--------------------------------------------------------------------------
-- InitialConditionsRecord Lenses
--
_initialTime :: forall s a. Lens' { initialTime :: a | s } a
_initialTime = prop (Proxy :: _ "initialTime")

_termContract :: forall s a. Lens' { termContract :: a | s } a
_termContract = prop (Proxy :: _ "termContract")

_templateContent :: forall s a. Lens' { templateContent :: a | s } a
_templateContent = prop (Proxy :: _ "templateContent")

--------------------------------------------------------------------------
-- ExecutionState Lenses
--
-- | Prism for the `ExecutionState` constructor of `SimulationRunning`.
_SimulationRunning :: Prism' ExecutionState ExecutionStateRecord
_SimulationRunning =
  prism SimulationRunning
    $
      ( \x -> case x of
          SimulationRunning record -> Right record
          anotherCase -> Left anotherCase
      )

-- | Prism for the `ExecutionState` constructor of `SimulationNotStarted`.
_SimulationNotStarted :: Prism' ExecutionState InitialConditionsRecord
_SimulationNotStarted =
  prism SimulationNotStarted
    $
      ( \x -> case x of
          SimulationNotStarted record -> Right record
          anotherCase -> Left anotherCase
      )

--------------------------------------------------------------------------
-- MarloweState Lenses
--
_executionState
  :: forall s p a
   . Strong p
  => Choice p
  => Optic' p { executionState :: Maybe a | s } a
_executionState = prop (Proxy :: _ "executionState") <<< _Just

_editorErrors :: forall s a. Lens' { editorErrors :: a | s } a
_editorErrors = prop (Proxy :: _ "editorErrors")

_editorWarnings :: forall s a. Lens' { editorWarnings :: a | s } a
_editorWarnings = prop (Proxy :: _ "editorWarnings")

_holes :: forall s a. Lens' { holes :: a | s } a
_holes = prop (Proxy :: _ "holes")

--- Language.Haskell.Interpreter ---
_result :: forall s a. Lens' { result :: a | s } a
_result = prop (Proxy :: _ "result")

_marloweState
  :: forall s
   . Lens' { marloweState :: NonEmptyList MarloweState | s }
       (NonEmptyList MarloweState)
_marloweState = prop (Proxy :: _ "marloweState")

_currentMarloweState
  :: forall s
   . Lens' { marloweState :: NonEmptyList MarloweState | s } MarloweState
_currentMarloweState = _marloweState <<< _Head

_currentContract
  :: forall s p
   . Strong p
  => Choice p
  => Optic' p { marloweState :: NonEmptyList MarloweState | s } (Term Contract)
_currentContract = _currentMarloweState
  <<< _executionState
  <<< _SimulationRunning
  <<< _contract

_currentPossibleActions
  :: forall s p
   . Strong p
  => Choice p
  => Optic' p { marloweState :: NonEmptyList MarloweState | s } PartiesAction
_currentPossibleActions = _currentMarloweState
  <<< _executionState
  <<< _SimulationRunning
  <<< _possibleActions
