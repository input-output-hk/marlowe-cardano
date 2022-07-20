module Page.HaskellEditor.Types where

import Prologue

import Analytics (class IsEvent, Event)
import Analytics as A
import Component.BottomPanel.Types as BottomPanel
import Component.MetadataTab.Types (MetadataAction, showConstructor)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Extra (parseDecodeJson)
import Data.BigInt.Argonaut (BigInt)
import Data.DateTime.Instant (Instant)
import Data.Generic.Rep (class Generic)
import Data.Lens (Fold', Getter', Lens', _Right, has, to)
import Data.Lens.Record (prop)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Minutes)
import Halogen.Monaco (KeyBindings(..))
import Halogen.Monaco as Monaco
import Language.Haskell.Interpreter
  ( InterpreterError
  , InterpreterResult
  , _InterpreterResult
  )
import Language.Marlowe.Extended.V1 as E
import Language.Marlowe.Extended.V1.Metadata.Types (MetadataHintInfo)
import Network.RemoteData (RemoteData(..), _Loading, _Success)
import StaticAnalysis.Types (AnalysisState, initAnalysisState)
import Text.Pretty (pretty)
import Type.Proxy (Proxy(..))
import Types (WebData)

data Action
  = Compile
  | ChangeKeyBindings KeyBindings
  | HandleEditorMessage Monaco.Message
  | BottomPanelAction (BottomPanel.Action BottomPanelView Action)
  | SendResultToSimulator
  | InitHaskellProject MetadataHintInfo String
  | SetTimeTemplateParam String Instant
  | SetValueTemplateParam String BigInt
  | AnalyseContract
  | AnalyseReachabilityContract
  | AnalyseContractForCloseRefund
  | MetadataAction MetadataAction
  | DoNothing

defaultEvent :: String -> Event
defaultEvent s = A.defaultEvent $ "Haskell." <> s

instance actionIsEvent :: IsEvent Action where
  toEvent Compile = Just $ defaultEvent "Compile"
  toEvent (ChangeKeyBindings _) = Just $ defaultEvent "ChangeKeyBindings"
  toEvent (HandleEditorMessage _) = Just $ defaultEvent "HandleEditorMessage"
  toEvent (BottomPanelAction action) = A.toEvent action
  toEvent SendResultToSimulator = Just $ defaultEvent "SendResultToSimulator"
  toEvent (InitHaskellProject _ _) = Just $ defaultEvent "InitHaskellProject"
  toEvent (SetTimeTemplateParam _ _) = Nothing
  toEvent (SetValueTemplateParam _ _) = Nothing
  toEvent AnalyseContract = Just $ defaultEvent "AnalyseContract"
  toEvent AnalyseReachabilityContract = Just $ defaultEvent
    "AnalyseReachabilityContract"
  toEvent AnalyseContractForCloseRefund = Just $ defaultEvent
    "AnalyseContractForCloseRefund"
  toEvent (MetadataAction action) = Just $ (defaultEvent "MetadataAction")
    { label = Just $ showConstructor action }
  toEvent DoNothing = Nothing

type State =
  { keybindings :: KeyBindings
  , compilationResult ::
      WebData (Either InterpreterError (InterpreterResult String))
  , bottomPanelState :: BottomPanel.State BottomPanelView
  , metadataHintInfo :: MetadataHintInfo
  , analysisState :: AnalysisState
  , tzOffset :: Minutes
  , editorReady :: Boolean
  }

_haskellEditorKeybindings :: Lens' State KeyBindings
_haskellEditorKeybindings = prop (Proxy :: _ "keybindings")

_compilationResult
  :: Lens' State
       (WebData (Either InterpreterError (InterpreterResult String)))
_compilationResult = prop (Proxy :: _ "compilationResult")

_metadataHintInfo :: Lens' State MetadataHintInfo
_metadataHintInfo = prop (Proxy :: _ "metadataHintInfo")

_analysisState :: Lens' State AnalysisState
_analysisState = prop (Proxy :: _ "analysisState")

_editorReady :: Lens' State Boolean
_editorReady = prop (Proxy :: _ "editorReady")

--- Language.Haskell.Interpreter is missing this ---
_result :: forall s a. Lens' { result :: a | s } a
_result = prop (Proxy :: _ "result")

_Pretty :: Getter' String String
_Pretty = to f
  where
  f ugly = case parseJSONContract ugly of
    Right contract -> (show <<< pretty) contract
    Left _ -> ugly

  parseJSONContract :: String -> Either JsonDecodeError E.Contract
  parseJSONContract = parseDecodeJson

_ContractString :: forall r. Monoid r => Fold' r State String
_ContractString = _compilationResult <<< _Success <<< _Right
  <<< _InterpreterResult
  <<< _result
  <<< _Pretty

_bottomPanelState :: Lens' State (BottomPanel.State BottomPanelView)
_bottomPanelState = prop (Proxy :: _ "bottomPanelState")

initialState :: Minutes -> State
initialState tzOffset =
  { keybindings: DefaultBindings
  , compilationResult: NotAsked
  , bottomPanelState: BottomPanel.initialState MetadataView
  , metadataHintInfo: mempty
  , analysisState: initAnalysisState
  , tzOffset
  , editorReady: false
  }

isCompiling :: State -> Boolean
isCompiling = has (_compilationResult <<< _Loading)

data BottomPanelView
  = StaticAnalysisView
  | ErrorsView
  | GeneratedOutputView
  | MetadataView

derive instance eqBottomPanelView :: Eq BottomPanelView

derive instance genericBottomPanelView :: Generic BottomPanelView _

instance showBottomPanelView :: Show BottomPanelView where
  show = genericShow
