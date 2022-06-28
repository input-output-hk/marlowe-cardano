module Page.JavascriptEditor.Types where

import Prologue

import Analytics (class IsEvent, Event)
import Analytics as A
import Component.BottomPanel.Types as BottomPanel
import Component.MetadataTab.Types (MetadataAction, showConstructor)
import Data.BigInt.Argonaut (BigInt)
import Data.DateTime.Instant (Instant)
import Data.Generic.Rep (class Generic)
import Data.Lens (Fold', Getter', Lens', Prism', prism, to, (^.))
import Data.Lens.Record (prop)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Minutes)
import Halogen.Monaco (KeyBindings(..))
import Halogen.Monaco as Monaco
import Language.Javascript.Interpreter (_result)
import Language.Javascript.Interpreter as JS
import Language.Marlowe.Extended.V1 (Contract)
import Language.Marlowe.Extended.V1.Metadata (MetadataHintInfo)
import StaticAnalysis.Types (AnalysisState, initAnalysisState)
import Text.Pretty (pretty)
import Type.Proxy (Proxy(..))

data CompilationState
  = NotCompiled
  | Compiling
  | CompilationError JS.CompilationError
  | CompiledSuccessfully (JS.InterpreterResult Contract)

_CompiledSuccessfully :: Prism' CompilationState (JS.InterpreterResult Contract)
_CompiledSuccessfully = prism CompiledSuccessfully unwrap
  where
  unwrap (CompiledSuccessfully x) = Right x

  unwrap y = Left y

_Pretty :: Getter' Contract String
_Pretty = to (show <<< pretty)

_ContractString :: forall r. Monoid r => Fold' r State String
_ContractString = _compilationResult <<< _CompiledSuccessfully <<< _result <<<
  _Pretty

data Action
  = Compile
  | ChangeKeyBindings KeyBindings
  | HandleEditorMessage Monaco.Message
  | BottomPanelAction (BottomPanel.Action BottomPanelView Action)
  | SendResultToSimulator
  | InitJavascriptProject MetadataHintInfo String
  | SetTimeTemplateParam String Instant
  | SetValueTemplateParam String BigInt
  | AnalyseContract
  | AnalyseReachabilityContract
  | AnalyseContractForCloseRefund
  | MetadataAction MetadataAction
  | DoNothing

defaultEvent :: String -> Event
defaultEvent s = A.defaultEvent $ "Javascript." <> s

instance actionIsEvent :: IsEvent Action where
  toEvent Compile = Just $ defaultEvent "Compile"
  toEvent (ChangeKeyBindings _) = Just $ defaultEvent "ChangeKeyBindings"
  toEvent (HandleEditorMessage _) = Just $ defaultEvent "HandleEditorMessage"
  toEvent (BottomPanelAction action) = A.toEvent action
  toEvent SendResultToSimulator = Just $ defaultEvent "SendResultToSimulator"
  toEvent (InitJavascriptProject _ _) = Just $ defaultEvent
    "InitJavascriptProject"
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

type DecorationIds =
  { topDecorationId :: String
  , bottomDecorationId :: String
  }

_topDecorationId :: Lens' DecorationIds String
_topDecorationId = prop (Proxy :: _ "topDecorationId")

_bottomDecorationId :: Lens' DecorationIds String
_bottomDecorationId = prop (Proxy :: _ "bottomDecorationId")

type State =
  { keybindings :: KeyBindings
  , bottomPanelState :: BottomPanel.State BottomPanelView
  , compilationResult :: CompilationState
  , decorationIds :: Maybe DecorationIds
  , metadataHintInfo :: MetadataHintInfo
  , analysisState :: AnalysisState
  , tzOffset :: Minutes
  , editorReady :: Boolean
  }

_keybindings :: Lens' State KeyBindings
_keybindings = prop (Proxy :: _ "keybindings")

_compilationResult :: Lens' State CompilationState
_compilationResult = prop (Proxy :: _ "compilationResult")

_decorationIds :: Lens' State (Maybe DecorationIds)
_decorationIds = prop (Proxy :: _ "decorationIds")

_bottomPanelState :: Lens' State (BottomPanel.State BottomPanelView)
_bottomPanelState = prop (Proxy :: _ "bottomPanelState")

_metadataHintInfo :: Lens' State MetadataHintInfo
_metadataHintInfo = prop (Proxy :: _ "metadataHintInfo")

_analysisState :: Lens' State AnalysisState
_analysisState = prop (Proxy :: _ "analysisState")

_editorReady :: Lens' State Boolean
_editorReady = prop (Proxy :: _ "editorReady")

isCompiling :: State -> Boolean
isCompiling state = case state ^. _compilationResult of
  Compiling -> true
  _ -> false

initialState :: Minutes -> State
initialState tzOffset =
  { keybindings: DefaultBindings
  , bottomPanelState: BottomPanel.initialState MetadataView
  , compilationResult: NotCompiled
  , decorationIds: Nothing
  , metadataHintInfo: mempty
  , analysisState: initAnalysisState
  , tzOffset
  , editorReady: false
  }

data BottomPanelView
  = StaticAnalysisView
  | ErrorsView
  | GeneratedOutputView
  | MetadataView

derive instance eqBottomPanelView :: Eq BottomPanelView

derive instance genericBottomPanelView :: Generic BottomPanelView _

instance showBottomPanelView :: Show BottomPanelView where
  show = genericShow
