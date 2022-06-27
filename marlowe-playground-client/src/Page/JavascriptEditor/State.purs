module Page.JavascriptEditor.State
  ( handleAction
  , mkEditor
  , editorGetValue
  ) where

import Prologue hiding (div)

import CloseAnalysis (analyseClose)
import Component.BottomPanel.State (handleAction) as BottomPanel
import Component.BottomPanel.Types (Action(..), State) as BottomPanel
import Data.Array as Array
import Data.Lens (assign, modifying, over, set, use, view)
import Data.List ((:))
import Data.List as List
import Data.Map as Map
import Data.Map.Ordered.OMap as OMap
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (drop, joinWith, length, take)
import Data.Time.Duration (Minutes(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Now (now)
import Examples.JS.Contracts as JSE
import Halogen (Component, HalogenM, gets, liftEffect, modify_, query)
import Halogen.Extra (mapSubmodule)
import Halogen.Monaco (Message(..), Query(..)) as Monaco
import Halogen.Monaco (Message, Query, monacoComponent)
import Language.Haskell.Monaco as HM
import Language.Javascript.Interpreter (InterpreterResult(..))
import Language.Javascript.Interpreter as JSI
import Language.Javascript.Monaco as JSM
import Language.Marlowe.Extended.V1 (Contract)
import Language.Marlowe.Extended.V1.Metadata
  ( MetaData
  , MetadataHintInfo
  , getMetadataHintInfo
  )
import MainFrame.Types (ChildSlots, _jsEditorSlot)
import Marlowe (Api)
import Marlowe.Template
  ( _timeContent
  , _valueContent
  , getPlaceholderIds
  , updateTemplateContent
  )
import Monaco (IRange, getModel, isError, setValue)
import Page.JavascriptEditor.Types
  ( Action(..)
  , BottomPanelView(..)
  , CompilationState(..)
  , State
  , _bottomPanelState
  , _compilationResult
  , _decorationIds
  , _editorReady
  , _keybindings
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
import StaticData (jsBufferLocalStorageKey)
import StaticData as StaticData
import Text.Extra (lines)

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

checkDecorationPosition :: Int -> Maybe IRange -> Maybe IRange -> Boolean
checkDecorationPosition
  numLines
  (Just { endLineNumber })
  (Just { startLineNumber }) = (endLineNumber == decorationHeaderLines) &&
  (startLineNumber == numLines - decorationFooterLines + 1)

checkDecorationPosition _ _ _ = false

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
  mContents <- liftEffect $ SessionStorage.getItem jsBufferLocalStorageKey
  editorSetValue $ fromMaybe JSE.example mContents
  assign _editorReady true

handleAction _ (HandleEditorMessage (Monaco.TextChanged text)) = do
  clearAnalysisResults
  -- When the Monaco component start it fires two messages at the same time, an EditorReady
  -- and TextChanged. Because of how Halogen works, it interwines the handleActions calls which
  -- can cause problems while setting and getting the values of the session storage. To avoid
  -- starting with an empty text editor we use an editorReady flag to ignore the text changes until
  -- we are ready to go. Eventually we could remove the initial TextChanged event, but we need to check
  -- that it doesn't break the plutus playground.
  editorReady <- use _editorReady
  when editorReady do
    -- TODO: This handler manages the logic of having a restricted range that cannot be modified. But the
    --       current implementation uses editorSetValue to overwrite the editor contents with the last
    --       correct value (taken from session storage). By using editorSetValue inside the TextChanged handler
    --       the events get fired multiple times on init, which makes hasUnsavedChanges always true for a new
    --       JS project or a project load.
    --
    --       Once the PR 2498 gets merged, I want to try changing the web-commons Monaco component so that the
    --       TextChanged handler returns an IModelContentChangedEvent instead of a string. That event cointains
    --       information of the range of the modifications, and if the action was triggered by an undo/redo
    --       action. With that information we can reimplement this by firing an undo event if a "read only"
    --       decoration. At this moment I'm not sure if that will solve the bubble problem but at least it will
    --       allow us to decouple from session storage.
    let
      prunedText = pruneJSboilerplate text

      numLines = Array.length $ lines text
    mDecorIds <- gets $ view _decorationIds
    case mDecorIds of
      Just decorIds -> do
        mRangeHeader <- query _jsEditorSlot unit
          (Monaco.GetDecorationRange decorIds.topDecorationId identity)
        mRangeFooter <- query _jsEditorSlot unit
          (Monaco.GetDecorationRange decorIds.bottomDecorationId identity)
        mContent <- liftEffect $ SessionStorage.getItem jsBufferLocalStorageKey
        if ((mContent == Nothing) || (mContent == Just prunedText)) then
          -- The case where `mContent == Just prunedText` is to prevent potential infinite loops, it should not happen
          assign _compilationResult NotCompiled
        else if
          checkJSboilerplate text && checkDecorationPosition numLines
            mRangeHeader
            mRangeFooter then
          ( do
              liftEffect $ SessionStorage.setItem jsBufferLocalStorageKey
                prunedText
              assign _compilationResult NotCompiled
          )
        else
          editorSetValue (fromMaybe "" mContent)
      Nothing -> editorSetValue prunedText

handleAction _ (ChangeKeyBindings bindings) = do
  assign _keybindings bindings
  void $ query _jsEditorSlot unit (Monaco.SetKeyBindings bindings unit)

handleAction metadata Compile = do
  clearAnalysisResults
  maybeModel <- query _jsEditorSlot unit (Monaco.GetModel identity)
  compilationResult <- case maybeModel of
    Nothing -> pure NotCompiled
    Just model -> do
      assign _compilationResult Compiling
      maybeMarkers <- query _jsEditorSlot unit (Monaco.GetModelMarkers identity)
      case
        map ((List.filter (\e -> isError e.severity)) <<< Array.toUnfoldable)
          maybeMarkers
        of
        Just ({ message, startLineNumber, startColumn } : _) ->
          pure $ CompilationError
            $ JSI.CompilationError
                { row: startLineNumber
                , column: startColumn
                , text: [ message ]
                }
        _ -> do
          res <- liftAff $ JSI.eval model
          case res of
            Left err -> pure $ CompilationError err
            Right result -> do
              let
                contract :: Contract
                contract = (unwrap result).result

                metadataHints :: MetadataHintInfo
                metadataHints = getMetadataHintInfo contract
              currentTime <- liftEffect now
              modify_
                $
                  over (_analysisState <<< _templateContent)
                    ( updateTemplateContent
                        currentTime
                        (Minutes 5.0)
                        (OMap.keys metadata.timeParameterDescriptions)
                        (getPlaceholderIds contract)
                    )
                    <<< set _metadataHintInfo metadataHints
              pure $ CompiledSuccessfully result
  assign _compilationResult compilationResult
  case compilationResult of
    (CompilationError _) -> handleAction metadata $ BottomPanelAction
      (BottomPanel.ChangePanel ErrorsView)
    _ -> pure unit

handleAction metadata (BottomPanelAction (BottomPanel.PanelAction action)) =
  handleAction metadata action

handleAction _ (BottomPanelAction action) = do
  toBottomPanel (BottomPanel.handleAction action)

handleAction _ SendResultToSimulator = pure unit

handleAction _ (InitJavascriptProject metadataHints prunedContent) = do
  editorSetValue prunedContent
  assign _metadataHintInfo metadataHints
  liftEffect $ SessionStorage.setItem jsBufferLocalStorageKey prunedContent

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
    CompiledSuccessfully (InterpreterResult interpretedResult) -> doAnalyze
      interpretedResult.result
    _ -> pure unit

decorationHeader :: String
decorationHeader =
  """import {
    PK, Role, Account, Party, ada, AvailableMoney, Constant, ConstantParam,
    NegValue, AddValue, SubValue, MulValue, DivValue, ChoiceValue, TimeIntervalStart,
    TimeIntervalEnd, UseValue, Cond, AndObs, OrObs, NotObs, ChoseSomething,
    ValueGE, ValueGT, ValueLT, ValueLE, ValueEQ, TrueObs, FalseObs, Deposit,
    Choice, Notify, Close, Pay, If, When, Let, Assert, SomeNumber, AccountId,
    ChoiceId, Token, ValueId, Value, EValue, Observation, Bound, Action, Payee,
    Case, Timeout, ETimeout, TimeParam, Contract
} from 'marlowe-js';

(function (): Contract {"""

decorationFooter :: String
decorationFooter = "})"

decorationHeaderLines :: Int
decorationHeaderLines = Array.length $ lines decorationHeader

decorationFooterLines :: Int
decorationFooterLines = Array.length $ lines decorationFooter

lengthOfHeader :: Int
lengthOfHeader = length decorationHeader

lengthOfFooter :: Int
lengthOfFooter = length decorationFooter

editorSetValue
  :: forall m
   . MonadAff m
  => String
  -> HalogenM State Action ChildSlots Void m Unit
editorSetValue contents = do
  let
    decoratedContent = joinWith "\n"
      [ decorationHeader, contents, decorationFooter ]

    numLines = Array.length $ lines decoratedContent

    decorationOptions =
      { isWholeLine: true
      , className: "monaco-readonly-decoration"
      , linesDecorationsClassName: ""
      }

    topRange =
      { startLineNumber: 1
      , startColumn: 0
      , endLineNumber: decorationHeaderLines
      , endColumn: 0
      }

    bottomRange =
      { startLineNumber: (numLines - decorationFooterLines + 1)
      , startColumn: 0
      , endLineNumber: numLines
      , endColumn: 0
      }
  void $ query _jsEditorSlot unit $ Monaco.SetText decoratedContent unit
  -- TODO: Refactor HandleEditorMessage Monaco.TextChanged so we can store the array of decorations instead of having to split them between top
  --       and bottom
  mDecorationIds <- use _decorationIds
  let
    oldHeaderDecoration = maybe [] (Array.singleton <<< _.topDecorationId)
      mDecorationIds

    oldFooterDecoration = maybe [] (Array.singleton <<< _.bottomDecorationId)
      mDecorationIds
  mNewHeaderIds <-
    query _jsEditorSlot unit
      $ Monaco.SetDeltaDecorations oldHeaderDecoration
          [ { range: topRange, options: decorationOptions } ]
          identity
  mNewFooterIds <-
    query _jsEditorSlot unit
      $ Monaco.SetDeltaDecorations oldFooterDecoration
          [ { range: bottomRange, options: decorationOptions } ]
          identity
  case mNewHeaderIds /\ mNewFooterIds of
    Just [ topDecorationId ] /\ Just [ bottomDecorationId ] ->
      assign _decorationIds $ Just { topDecorationId, bottomDecorationId }
    _ -> pure unit

checkJSboilerplate :: String -> Boolean
checkJSboilerplate content =
  let
    header = (take (lengthOfHeader + 1) content)

    footer = (drop (length content - lengthOfFooter - 1) content)
  in
    (header == decorationHeader <> "\n") && (footer == "\n" <> decorationFooter)

pruneJSboilerplate :: String -> String
pruneJSboilerplate content =
  let
    noHeader = (drop (lengthOfHeader + 1) content)
  in
    take (length noHeader - lengthOfFooter - 1) noHeader

editorGetValue
  :: forall m
   . MonadAff m
  => HalogenM State Action ChildSlots Void m (Maybe String)
editorGetValue = do
  mContent <- query _jsEditorSlot unit (Monaco.GetText identity)
  pure
    ( map
        pruneJSboilerplate
        mContent
    )

editorSetTheme
  :: forall state action msg m. HalogenM state action ChildSlots msg m Unit
editorSetTheme = void $ query _jsEditorSlot unit
  (Monaco.SetTheme HM.daylightTheme.name unit)

mkEditor
  :: forall m. MonadEffect m => MonadAff m => Component Query Unit Message m
mkEditor = monacoComponent $ JSM.settings setup
  where
  setup editor =
    liftEffect do
      mContents <- SessionStorage.getItem StaticData.jsBufferLocalStorageKey
      let
        contents = fromMaybe JSE.example mContents

        decoratedContent = joinWith "\n"
          [ decorationHeader, contents, decorationFooter ]
      model <- getModel editor
      setValue model decoratedContent
      pure unit
