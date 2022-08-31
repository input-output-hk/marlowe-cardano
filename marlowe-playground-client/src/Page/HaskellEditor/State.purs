module Page.HaskellEditor.State
  ( handleAction
  , editorGetValue
  ) where

import Prologue hiding (div)

import CloseAnalysis (analyseClose)
import Component.BottomPanel.State (handleAction) as BottomPanel
import Component.BottomPanel.Types (Action(..), State) as BottomPanel
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Extra (parseDecodeJson)
import Data.Array (catMaybes)
import Data.Either (hush)
import Data.Foldable (for_)
import Data.Lens (assign, modifying, over, set, use)
import Data.Map as Map
import Data.Map.Ordered.OMap as OMap
import Data.Maybe (fromMaybe, maybe)
import Data.String as String
import Data.Time.Duration (Minutes(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Now (now)
import Examples.Haskell.Contracts (example) as HE
import Halogen (HalogenM, liftEffect, modify_, query)
import Halogen.Extra (mapSubmodule)
import Halogen.Monaco (Message(..), Query(..)) as Monaco
import Language.Haskell.Interpreter
  ( CompilationError(..)
  , InterpreterError(..)
  , InterpreterResult(..)
  )
import Language.Haskell.Monaco as HM
import Language.Marlowe.Extended.V1 (Contract)
import Language.Marlowe.Extended.V1.Metadata
  ( getMetadataHintInfo
  )
import Language.Marlowe.Extended.V1.Metadata.Types
  ( MetaData
  , MetadataHintInfo

  )
import MainFrame.Types (ChildSlots, _haskellEditorSlot)
import Marlowe (Api, postApiCompile)
import Marlowe.Template (_timeContent, _valueContent, getPlaceholderIds)
import Marlowe.Template as Template
import Monaco (IMarkerData, markerSeverity)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Page.HaskellEditor.Types
  ( Action(..)
  , BottomPanelView(..)
  , State
  , _bottomPanelState
  , _compilationResult
  , _editorReady
  , _haskellEditorKeybindings
  , _metadataHintInfo
  )
import Servant.PureScript (class MonadAjax)
import SessionStorage as SessionStorage
import StaticAnalysis.Reachability (analyseReachability)
import StaticAnalysis.StaticTools (analyseContract)
import StaticAnalysis.Types
  ( AnalysisExecutionState(..)
  , _analysisExecutionState
  , _analysisState
  , _templateContent
  )
import StaticData (haskellBufferLocalStorageKey)
import Webghc.Server (CompileRequest(..))

toBottomPanel
  :: forall m a
   . Functor m
  => HalogenM (BottomPanel.State BottomPanelView)
       (BottomPanel.Action BottomPanelView Action)
       ChildSlots
       Void
       m
       a
  -> HalogenM State Action ChildSlots Void m a
toBottomPanel = mapSubmodule _bottomPanelState BottomPanelAction

handleAction
  :: forall m
   . MonadAff m
  => MonadAjax Api m
  => MetaData
  -> Action
  -> HalogenM State Action ChildSlots Void m Unit
handleAction _ DoNothing = pure unit

handleAction _ (HandleEditorMessage Monaco.EditorReady) = do
  editorSetTheme
  mContents <- liftEffect $ SessionStorage.getItem haskellBufferLocalStorageKey
  editorSetValue $ fromMaybe HE.example mContents
  assign _editorReady true

handleAction _ (HandleEditorMessage (Monaco.TextChanged text)) = do
  -- When the Monaco component start it fires two messages at the same time, an EditorReady
  -- and TextChanged. Because of how Halogen works, it interwines the handleActions calls which
  -- can cause problems while setting and getting the values of the session storage. To avoid
  -- starting with an empty text editor we use an editorReady flag to ignore the text changes until
  -- we are ready to go. Eventually we could remove the initial TextChanged event, but we need to check
  -- that it doesn't break the plutus playground.
  editorReady <- use _editorReady
  when editorReady do
    liftEffect $ SessionStorage.setItem haskellBufferLocalStorageKey text
    assign _compilationResult NotAsked

handleAction _ (ChangeKeyBindings bindings) = do
  assign _haskellEditorKeybindings bindings
  void $ query _haskellEditorSlot unit (Monaco.SetKeyBindings bindings unit)

handleAction metadata Compile = do
  clearAnalysisResults
  mContents <- editorGetValue
  case mContents of
    Nothing -> pure unit
    Just code -> do
      assign _compilationResult Loading
      result <- lift $ RemoteData.fromEither
        <$> postApiCompile (CompileRequest { code, implicitPrelude: true })
      assign _compilationResult result
      -- Update the error display.
      case result of
        Success (Left _) -> handleAction metadata $ BottomPanelAction
          (BottomPanel.ChangePanel ErrorsView)
        Success (Right (InterpreterResult interpretedResult)) -> do
          let
            mContract :: Maybe Contract
            mContract = (hush <<< parseDecodeJson)
              interpretedResult.result

            metadataHints :: MetadataHintInfo
            metadataHints = maybe mempty getMetadataHintInfo mContract
          currentTime <- liftEffect now

          for_ mContract \contract ->
            modify_
              $
                over (_analysisState <<< _templateContent)
                  ( Template.updateTemplateContent
                      currentTime
                      (Minutes 5.0)
                      (OMap.keys metadata.timeParameterDescriptions)
                      (getPlaceholderIds contract)
                  )
                  <<< set _metadataHintInfo metadataHints
        _ -> pure unit
      let
        markers = case result of
          Success (Left errors) -> toMarkers errors
          _ -> []
      void $ query _haskellEditorSlot unit
        (Monaco.SetModelMarkers markers identity)

handleAction metadata (BottomPanelAction (BottomPanel.PanelAction action)) =
  handleAction
    metadata
    action

handleAction _ (BottomPanelAction action) = do
  toBottomPanel (BottomPanel.handleAction action)

handleAction _ SendResultToSimulator = pure unit

handleAction _ (InitHaskellProject metadataHints contents) = do
  editorSetValue contents
  assign _metadataHintInfo metadataHints
  liftEffect $ SessionStorage.setItem haskellBufferLocalStorageKey contents

handleAction _ (SetValueTemplateParam key value) = do
  clearAnalysisResults
  modifying
    (_analysisState <<< _templateContent <<< _valueContent)
    (Map.insert key value)

handleAction _ (SetTimeTemplateParam key value) = do
  clearAnalysisResults
  modifying
    (_analysisState <<< _templateContent <<< _timeContent)
    (Map.insert key value)

handleAction _ (MetadataAction _) = pure unit

handleAction _ AnalyseContract = analyze analyseContract

handleAction _ AnalyseReachabilityContract = analyze analyseReachability

handleAction _ AnalyseContractForCloseRefund = analyze analyseClose

clearAnalysisResults :: forall m. HalogenM State Action ChildSlots Void m Unit
clearAnalysisResults = assign
  (_analysisState <<< _analysisExecutionState)
  NoneAsked

-- This function runs a static analysis to the compiled code if it compiled successfully.
analyze
  :: forall m
   . MonadAff m
  => MonadAjax Api m
  => (Contract -> HalogenM State Action ChildSlots Void m Unit)
  -> HalogenM State Action ChildSlots Void m Unit
analyze doAnalyze = do
  compilationResult <- use _compilationResult
  case compilationResult of
    Success (Right (InterpreterResult interpretedResult)) ->
      let
        mContract = (hush <<< parseDecodeJson)
          interpretedResult.result
      in
        for_ mContract doAnalyze
    _ -> pure unit

editorSetTheme
  :: forall state action msg m. HalogenM state action ChildSlots msg m Unit
editorSetTheme = void $ query _haskellEditorSlot unit
  (Monaco.SetTheme HM.daylightTheme.name unit)

editorSetValue
  :: forall state action msg m
   . String
  -> HalogenM state action ChildSlots msg m Unit
editorSetValue contents = void $ query _haskellEditorSlot unit
  (Monaco.SetText contents unit)

editorGetValue
  :: forall state action msg m
   . HalogenM state action ChildSlots msg m (Maybe String)
editorGetValue = query _haskellEditorSlot unit (Monaco.GetText identity)

toMarkers :: InterpreterError -> Array IMarkerData
toMarkers (TimeoutError _) = []

toMarkers (CompilationErrors errors) = catMaybes (toMarker <$> errors)

toMarker :: CompilationError -> Maybe IMarkerData
toMarker (RawError _) = Nothing

toMarker (CompilationError { row, column, text }) =
  Just
    { severity: markerSeverity "Error"
    , message: String.joinWith "\\n" text
    , startLineNumber: row
    , startColumn: column
    , endLineNumber: row
    , endColumn: column
    , code: mempty
    , source: mempty
    }
