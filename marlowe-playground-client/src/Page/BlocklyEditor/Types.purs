module Page.BlocklyEditor.Types where

import Prologue

import Analytics (class IsEvent, Event)
import Analytics as A
import Component.Blockly.Types as Blockly
import Component.BottomPanel.Types as BottomPanel
import Component.MetadataTab.Types (MetadataAction, showConstructor)
import Data.BigInt.Argonaut (BigInt)
import Data.DateTime.Instant (Instant)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Minutes)
import Language.Marlowe.Extended.V1.Metadata (MetadataHintInfo)
import Marlowe.Linter (Warning)
import StaticAnalysis.Types (AnalysisState, initAnalysisState)
import Type.Proxy (Proxy(..))

data Action
  = HandleBlocklyMessage Blockly.Message
  | InitBlocklyProject String
  | SendToSimulator
  | ViewAsMarlowe
  | Save
  | BottomPanelAction (BottomPanel.Action BottomPanelView Action)
  | AnalyseContract
  | AnalyseReachabilityContract
  | AnalyseContractForCloseRefund
  | MetadataAction MetadataAction
  | SetTimeTemplateParam String Instant
  | SetValueTemplateParam String BigInt
  | SelectWarning Warning

defaultEvent :: String -> Event
defaultEvent s = (A.defaultEvent $ "BlocklyEditor." <> s)
  { category = Just "Blockly" }

instance blocklyActionIsEvent :: IsEvent Action where
  toEvent (HandleBlocklyMessage _) = Just $ defaultEvent "HandleBlocklyMessage"
  toEvent (InitBlocklyProject _) = Just $ defaultEvent "InitBlocklyProject"
  toEvent SendToSimulator = Just $ defaultEvent "SendToSimulator"
  toEvent ViewAsMarlowe = Just $ defaultEvent "ViewAsMarlowe"
  toEvent Save = Just $ defaultEvent "Save"
  toEvent (BottomPanelAction action) = A.toEvent action
  toEvent AnalyseContract = Just $ defaultEvent "AnalyseContract"
  toEvent AnalyseReachabilityContract = Just $ defaultEvent
    "AnalyseReachabilityContract"
  toEvent AnalyseContractForCloseRefund = Just $ defaultEvent
    "AnalyseContractForCloseRefund"
  toEvent (MetadataAction action) = Just $ (defaultEvent "MetadataAction")
    { label = Just $ showConstructor action }
  toEvent (SetTimeTemplateParam _ _) = Nothing
  toEvent (SetValueTemplateParam _ _) = Nothing
  toEvent (SelectWarning _) = Just $ defaultEvent "SelectWarning"

data BottomPanelView
  = StaticAnalysisView
  | BlocklyWarningsView
  | MetadataView

derive instance eqBottomPanelView :: Eq BottomPanelView

derive instance genericBottomPanelView :: Generic BottomPanelView _

instance showBottomPanelView :: Show BottomPanelView where
  show = genericShow

type State =
  { errorMessage :: Maybe String
  , marloweCode :: Maybe String
  , hasHoles :: Boolean
  , bottomPanelState :: BottomPanel.State BottomPanelView
  , metadataHintInfo :: MetadataHintInfo
  , analysisState :: AnalysisState
  , tzOffset :: Minutes
  , warnings :: Array Warning
  }

_errorMessage :: Lens' State (Maybe String)
_errorMessage = prop (Proxy :: _ "errorMessage")

_marloweCode :: Lens' State (Maybe String)
_marloweCode = prop (Proxy :: _ "marloweCode")

_hasHoles :: Lens' State Boolean
_hasHoles = prop (Proxy :: _ "hasHoles")

_bottomPanelState :: Lens' State (BottomPanel.State BottomPanelView)
_bottomPanelState = prop (Proxy :: _ "bottomPanelState")

_metadataHintInfo :: Lens' State MetadataHintInfo
_metadataHintInfo = prop (Proxy :: _ "metadataHintInfo")

_analysisState :: Lens' State AnalysisState
_analysisState = prop (Proxy :: _ "analysisState")

_warnings :: Lens' State (Array Warning)
_warnings = prop (Proxy :: _ "warnings")

initialState :: Minutes -> State
initialState tzOffset =
  { errorMessage: Nothing
  , marloweCode: Nothing
  , hasHoles: false
  , bottomPanelState: BottomPanel.initialState MetadataView
  , metadataHintInfo: mempty
  , analysisState: initAnalysisState
  , tzOffset
  , warnings: mempty
  }
