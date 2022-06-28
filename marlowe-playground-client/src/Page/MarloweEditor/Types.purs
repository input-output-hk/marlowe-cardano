module Page.MarloweEditor.Types where

import Prologue

import Analytics (class IsEvent, Event)
import Analytics as A
import Component.BottomPanel.Types as BottomPanel
import Component.MetadataTab.Types (MetadataAction, showConstructor)
import Data.Array as Array
import Data.BigInt.Argonaut (BigInt)
import Data.DateTime.Instant (Instant)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', to, view)
import Data.Lens.Record (prop)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Minutes)
import Halogen.Monaco (KeyBindings(..))
import Halogen.Monaco as Monaco
import Language.Marlowe.Extended.V1.Metadata (MetadataHintInfo)
import Monaco (IMarkerData)
import StaticAnalysis.Types (AnalysisState, initAnalysisState)
import Type.Proxy (Proxy(..))
import Web.HTML.Event.DragEvent (DragEvent)

type Pos = Int

data Action
  = ChangeKeyBindings KeyBindings
  | HandleEditorMessage Monaco.Message
  | HandleDragEvent DragEvent
  | HandleDropEvent DragEvent
  | MoveToPosition Pos Pos
  | LoadScript String
  | SetEditorText String
  | BottomPanelAction (BottomPanel.Action BottomPanelView Action)
  | ShowErrorDetail Boolean
  | SendToSimulator
  | ViewAsBlockly
  | InitMarloweProject String
  | SelectHole (Maybe String)
  | MetadataAction MetadataAction
  | SetTimeTemplateParam String Instant
  | SetValueTemplateParam String BigInt
  | AnalyseContract
  | AnalyseReachabilityContract
  | AnalyseContractForCloseRefund
  | Save
  | DoNothing

defaultEvent :: String -> Event
defaultEvent s = A.defaultEvent $ "MarloweEditor." <> s

instance actionIsEvent :: IsEvent Action where
  toEvent (ChangeKeyBindings _) = Just $ defaultEvent "ChangeKeyBindings"
  toEvent (HandleEditorMessage _) = Just $ defaultEvent "HandleEditorMessage"
  toEvent (HandleDragEvent _) = Just $ defaultEvent "HandleDragEvent"
  toEvent (HandleDropEvent _) = Just $ defaultEvent "HandleDropEvent"
  toEvent (MoveToPosition _ _) = Just $ defaultEvent "MoveToPosition"
  toEvent (LoadScript script) = Just $ (defaultEvent "LoadScript")
    { label = Just script }
  toEvent (SetEditorText _) = Just $ defaultEvent "SetEditorText"
  toEvent (BottomPanelAction action) = A.toEvent action
  toEvent (ShowErrorDetail _) = Just $ defaultEvent "ShowErrorDetail"
  toEvent SendToSimulator = Just $ defaultEvent "SendToSimulator"
  toEvent ViewAsBlockly = Just $ defaultEvent "ViewAsBlockly"
  toEvent (InitMarloweProject _) = Just $ defaultEvent "InitMarloweProject"
  toEvent (SelectHole _) = Just $ defaultEvent "SelectHole"
  toEvent (MetadataAction action) = Just $ (defaultEvent "MetadataAction")
    { label = Just $ showConstructor action }
  toEvent (SetTimeTemplateParam _ _) = Nothing
  toEvent (SetValueTemplateParam _ _) = Nothing
  toEvent AnalyseContract = Just $ defaultEvent "AnalyseContract"
  toEvent AnalyseReachabilityContract = Just $ defaultEvent
    "AnalyseReachabilityContract"
  toEvent AnalyseContractForCloseRefund = Just $ defaultEvent
    "AnalyseContractForCloseRefund"
  toEvent Save = Just $ defaultEvent "Save"
  toEvent DoNothing = Nothing

data BottomPanelView
  = StaticAnalysisView
  | MarloweErrorsView
  | MarloweWarningsView
  | MetadataView

derive instance eqBottomPanelView :: Eq BottomPanelView

derive instance genericBottomPanelView :: Generic BottomPanelView _

instance showBottomPanelView :: Show BottomPanelView where
  show = genericShow

type State =
  { keybindings :: KeyBindings
  , bottomPanelState :: BottomPanel.State BottomPanelView
  , showErrorDetail :: Boolean
  , selectedHole :: Maybe String
  , metadataHintInfo :: MetadataHintInfo
  , analysisState :: AnalysisState
  , editorErrors :: Array IMarkerData
  , editorWarnings :: Array IMarkerData
  , hasHoles :: Boolean
  , editorReady :: Boolean
  , tzOffset :: Minutes
  }

_keybindings :: Lens' State KeyBindings
_keybindings = prop (Proxy :: _ "keybindings")

_showErrorDetail :: Lens' State Boolean
_showErrorDetail = prop (Proxy :: _ "showErrorDetail")

_selectedHole :: Lens' State (Maybe String)
_selectedHole = prop (Proxy :: _ "selectedHole")

_metadataHintInfo :: Lens' State MetadataHintInfo
_metadataHintInfo = prop (Proxy :: _ "metadataHintInfo")

_editorErrors :: forall s a. Lens' { editorErrors :: a | s } a
_editorErrors = prop (Proxy :: _ "editorErrors")

_editorWarnings :: forall s a. Lens' { editorWarnings :: a | s } a
_editorWarnings = prop (Proxy :: _ "editorWarnings")

_bottomPanelState :: Lens' State (BottomPanel.State BottomPanelView)
_bottomPanelState = prop (Proxy :: _ "bottomPanelState")

_hasHoles :: Lens' State Boolean
_hasHoles = prop (Proxy :: _ "hasHoles")

_editorReady :: Lens' State Boolean
_editorReady = prop (Proxy :: _ "editorReady")

initialState :: Minutes -> State
initialState tzOffset =
  { keybindings: DefaultBindings
  , bottomPanelState: BottomPanel.initialState MetadataView
  , showErrorDetail: false
  , selectedHole: Nothing
  , metadataHintInfo: mempty
  , analysisState: initAnalysisState
  , tzOffset
  , editorErrors: mempty
  , editorWarnings: mempty
  , hasHoles: false
  , editorReady: false
  }

contractHasHoles :: State -> Boolean
contractHasHoles = view _hasHoles

contractHasErrors :: State -> Boolean
contractHasErrors state = not $ view (_editorErrors <<< to Array.null) state
