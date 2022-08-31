-- A separate module for types that are shared between Simulation and Simulation.BottomPanel
module Page.Simulation.Types where

import Prologue

import Analytics (class IsEvent, Event)
import Analytics as A
import Component.BottomPanel.Types as BottomPanel
import Data.BigInt.Argonaut (BigInt)
import Data.DateTime.Instant (Instant)
import Data.Generic.Rep (class Generic)
import Data.List.Types (NonEmptyList)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Minutes)
import Halogen.Monaco as Monaco
import Help (HelpContext)
import Language.Marlowe.Core.V1.Semantics.Types
  ( Bound
  , ChoiceId
  , ChosenNum
  , Input
  )
import Marlowe.Symbolic.Types.Response (Result)
import Network.RemoteData (RemoteData)
import Simulator.Types (MarloweState)

type StateBase r =
  { showRightPanel :: Boolean
  , bottomPanelState :: BottomPanel.State BottomPanelView
  , marloweState :: NonEmptyList MarloweState
  , tzOffset :: Minutes
  , helpContext :: HelpContext
  -- List of decoration ids used by the monaco editor to track the running contract
  , decorationIds :: Array String
  | r
  }

type State = StateBase (projectName :: String)

data Action
  = HandleEditorMessage Monaco.Message
  -- marlowe actions
  | SetInitialTime Instant
  | SetTimeTemplateParam String Instant
  | SetValueTemplateParam String BigInt
  | StartSimulation
  | DownloadAsJson
  | MoveTime Instant
  | AddInput Input (Array Bound)
  | SetChoice ChoiceId ChosenNum
  | ResetSimulator
  | Undo
  | LoadContract String
  -- simulation view
  -- FIXME: We are not showing a help context. See if we want to bring back this
  --       functionality or delete this code
  | ChangeHelpContext HelpContext
  -- FIXME: This action is not triggerable at the moment. Check if we want to bring
  --        back this functionality or delete this code
  | ShowRightPanel Boolean
  | BottomPanelAction (BottomPanel.Action BottomPanelView Action)
  | EditSource

defaultEvent :: String -> Event
defaultEvent s = A.defaultEvent $ "Simulation." <> s

instance isEventAction :: IsEvent Action where
  toEvent (SetInitialTime _) = Just $ defaultEvent "SetInitialTime"
  toEvent (SetTimeTemplateParam _ _) = Nothing
  toEvent (SetValueTemplateParam _ _) = Nothing
  toEvent StartSimulation = Just $ defaultEvent "StartSimulation"
  toEvent DownloadAsJson = Just $ defaultEvent "DownloadAsJson"
  toEvent (MoveTime _) = Just $ defaultEvent "MoveTime"
  toEvent (AddInput _ _) = Just $ defaultEvent "AddInput"
  toEvent (SetChoice _ _) = Just $ defaultEvent "SetChoice"
  toEvent ResetSimulator = Just $ defaultEvent "ResetSimulator"
  toEvent Undo = Just $ defaultEvent "Undo"
  toEvent (LoadContract _) = Just $ defaultEvent "LoadContract"
  toEvent (ChangeHelpContext help) = Just $ (defaultEvent "ChangeHelpContext")
    { label = Just $ show help }
  toEvent (ShowRightPanel _) = Just $ defaultEvent "ShowRightPanel"
  toEvent (BottomPanelAction action) = A.toEvent action
  toEvent EditSource = Just $ defaultEvent "EditSource"
  toEvent (HandleEditorMessage _) = Just $ defaultEvent "HandleEditorMessage"

data Query a = WebsocketResponse (RemoteData String Result) a

data BottomPanelView
  = CurrentStateView
  | WarningsAndErrorsView

derive instance eqBottomPanelView :: Eq BottomPanelView

derive instance genericBottomPanelView :: Generic BottomPanelView _

instance showBottomPanelView :: Show BottomPanelView where
  show = genericShow
