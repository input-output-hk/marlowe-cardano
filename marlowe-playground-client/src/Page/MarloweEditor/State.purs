module Page.MarloweEditor.State
  ( handleAction
  , editorGetValue
  ) where

import Prologue hiding (div)

import CloseAnalysis (analyseClose)
import Component.BottomPanel.State (handleAction) as BottomPanel
import Component.BottomPanel.Types (Action(..), State) as BottomPanel
import Control.Monad.Except (lift)
import Control.Monad.Maybe.Extra (hoistMaybe)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array as Array
import Data.Either (hush)
import Data.Foldable (for_)
import Data.Lens (assign, modifying, over, preview, set, use)
import Data.Lens.Index (ix)
import Data.Map as Map
import Data.Map.Ordered.OMap as OMap
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), codePointFromChar, contains)
import Data.String as String
import Data.Time.Duration (Minutes(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Now (now)
import Examples.Marlowe.Contracts (example) as ME
import Halogen (HalogenM, liftEffect, modify_, query)
import Halogen as H
import Halogen.Extra (mapSubmodule)
import Halogen.Monaco (Message(..), Query(..)) as Monaco
import MainFrame.Types (ChildSlots, _marloweEditorPageSlot)
import Marlowe (Api)
import Marlowe.Extended as Extended
import Marlowe.Extended.Metadata (MetaData, MetadataHintInfo)
import Marlowe.Holes as Holes
import Marlowe.LinterText as Linter
import Marlowe.Monaco (updateAdditionalContext)
import Marlowe.Monaco as MM
import Marlowe.Parser (parseContract)
import Marlowe.Template (TemplateContent)
import Marlowe.Template as Template
import Monaco (isError, isWarning)
import Page.MarloweEditor.Types
  ( Action(..)
  , BottomPanelView
  , State
  , _bottomPanelState
  , _editorErrors
  , _editorReady
  , _editorWarnings
  , _hasHoles
  , _keybindings
  , _metadataHintInfo
  , _selectedHole
  , _showErrorDetail
  )
import Servant.PureScript (class MonadAjax)
import SessionStorage as SessionStorage
import StaticAnalysis.Reachability
  ( analyseReachability
  , getUnreachableContracts
  )
import StaticAnalysis.StaticTools (analyseContract)
import StaticAnalysis.Types
  ( AnalysisExecutionState(..)
  , _analysisExecutionState
  , _analysisState
  , _templateContent
  )
import StaticData (marloweBufferLocalStorageKey)
import StaticData as StaticData
import Text.Pretty (pretty)
import Web.Event.Extra (preventDefault, readFileFromDragEvent)

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

handleAction _ (ChangeKeyBindings bindings) = do
  assign _keybindings bindings
  void $ query _marloweEditorPageSlot unit (Monaco.SetKeyBindings bindings unit)

handleAction metadata (HandleEditorMessage Monaco.EditorReady) = do
  editorSetTheme
  mContents <- liftEffect $ SessionStorage.getItem marloweBufferLocalStorageKey
  editorSetValue $ fromMaybe ME.example mContents
  for_ mContents $ processMarloweCode metadata
  assign _editorReady true

handleAction metadata (HandleEditorMessage (Monaco.TextChanged text)) = do
  -- When the Monaco component start it fires two messages at the same time, an EditorReady
  -- and TextChanged. Because of how Halogen works, it interwines the handleActions calls which
  -- can cause problems while setting and getting the values of the session storage. To avoid
  -- starting with an empty text editor we use an editorReady flag to ignore the text changes until
  -- we are ready to go. Eventually we could remove the initial TextChanged event, but we need to check
  -- that it doesn't break the plutus playground.
  editorReady <- use _editorReady
  when editorReady do
    assign _selectedHole Nothing
    liftEffect $ SessionStorage.setItem marloweBufferLocalStorageKey text
    processMarloweCode metadata text

handleAction _ (HandleDragEvent event) = liftEffect $ preventDefault event

handleAction _ (HandleDropEvent event) = do
  liftEffect $ preventDefault event
  contents <- liftAff $ readFileFromDragEvent event
  void $ editorSetValue contents

handleAction _ (MoveToPosition lineNumber column) = do
  void $ query _marloweEditorPageSlot unit
    (Monaco.SetPosition { column, lineNumber } unit)

handleAction _ (LoadScript key) = do
  for_ (preview (ix key) StaticData.marloweContracts) \contents -> do
    let
      prettyContents = case parseContract contents of
        Right pcon -> show $ pretty pcon
        Left _ -> contents
    editorSetValue prettyContents
    liftEffect $ SessionStorage.setItem marloweBufferLocalStorageKey
      prettyContents

handleAction _ (SetEditorText contents) = editorSetValue contents

handleAction metadata (BottomPanelAction (BottomPanel.PanelAction action)) =
  handleAction
    metadata
    action

handleAction _ (BottomPanelAction action) = do
  toBottomPanel (BottomPanel.handleAction action)

handleAction _ (ShowErrorDetail val) = assign _showErrorDetail val

handleAction _ SendToSimulator = pure unit

handleAction _ ViewAsBlockly = pure unit

handleAction _ (InitMarloweProject contents) = do
  editorSetValue contents
  liftEffect $ SessionStorage.setItem marloweBufferLocalStorageKey contents

handleAction _ (SelectHole hole) = assign _selectedHole hole

handleAction _ (SetValueTemplateParam key value) =
  modifying
    (_analysisState <<< _templateContent <<< Template._valueContent)
    (Map.insert key value)

handleAction _ (SetTimeTemplateParam key value) =
  modifying
    (_analysisState <<< _templateContent <<< Template._timeContent)
    (Map.insert key value)

handleAction _ (MetadataAction _) = pure unit

handleAction metadata AnalyseContract = runAnalysis metadata $ analyseContract

handleAction metadata AnalyseReachabilityContract = runAnalysis metadata $
  analyseReachability

handleAction metadata AnalyseContractForCloseRefund = runAnalysis metadata $
  analyseClose

handleAction metadata ClearAnalysisResults = do
  assign (_analysisState <<< _analysisExecutionState) NoneAsked
  mContents <- editorGetValue
  for_ mContents $ processMarloweCode metadata

handleAction _ Save = pure unit

runAnalysis
  :: forall m
   . MonadAff m
  => MetaData
  -> (Extended.Contract -> HalogenM State Action ChildSlots Void m Unit)
  -> HalogenM State Action ChildSlots Void m Unit
runAnalysis metadata doAnalyze =
  void
    $ runMaybeT do
        contents <- MaybeT $ editorGetValue
        contract <- hoistMaybe $ parseContract' contents
        lift
          $ do
              doAnalyze contract
              processMarloweCode metadata contents

parseContract' :: String -> Maybe Extended.Contract
parseContract' = Holes.fromTerm <=< hush <<< parseContract

-- This function makes all the heavy processing needed to have the Editor state in sync with current changes.
processMarloweCode
  :: forall m
   . MonadAff m
  => MetaData
  -> String
  -> HalogenM State Action ChildSlots Void m Unit
processMarloweCode metadata text = do
  analysisExecutionState <- use (_analysisState <<< _analysisExecutionState)
  oldMetadataInfo <- use _metadataHintInfo
  currentTime <- liftEffect now

  let
    eParsedContract = parseContract text

    unreachableContracts = getUnreachableContracts analysisExecutionState

    (Tuple markerData additionalContext) = Linter.markers unreachableContracts
      eParsedContract

    errorMarkers =
      markerData
        # Array.filter (isError <<< _.severity)

    -- The initial message of a hole warning is very lengthy, so we trim it before
    -- displaying it.
    -- see https://github.com/input-output-hk/plutus/pull/2560#discussion_r550252989
    warningMarkers =
      markerData
        # Array.filter (isWarning <<< _.severity)
        <#> \marker ->
          let
            trimmedMessage =
              if String.take 6 marker.source == "Hole: " then
                String.takeWhile (\c -> c /= codePointFromChar '\n')
                  marker.message
              else
                marker.message
          in
            marker { message = trimmedMessage }

    mContract :: Maybe Extended.Contract
    mContract = Holes.fromTerm =<< hush eParsedContract

    metadataInfo :: MetadataHintInfo
    metadataInfo = fromMaybe oldMetadataInfo additionalContext.metadataHints

    hasHoles =
      not $ Array.null
        $ Array.filter (contains (Pattern "hole") <<< _.message) warningMarkers

    -- If we can get an Extended contract from the holes contract (basically if it has no holes)
    -- then update the template content. If not, leave them as they are
    maybeUpdateTemplateContent :: TemplateContent -> TemplateContent
    maybeUpdateTemplateContent = case mContract of
      Nothing -> identity
      Just contract ->
        Template.updateTemplateContent
          currentTime
          (Minutes 5.0)
          (OMap.keys metadata.timeParameterDescriptions)
          (Template.getPlaceholderIds contract)
  void $ H.request _marloweEditorPageSlot unit $ Monaco.SetModelMarkers
    markerData
  modify_
    ( set _editorWarnings warningMarkers
        <<< set _editorErrors errorMarkers
        <<< set _hasHoles hasHoles
        <<< over (_analysisState <<< _templateContent)
          maybeUpdateTemplateContent
        <<< set _metadataHintInfo metadataInfo
    )
  {-
    There are three different Monaco objects that require the linting information:
      * Markers
      * Code completion (type aheads)
      * Code suggestions (Quick fixes)
     To avoid having to recalculate the linting multiple times, we add aditional context to the providers
     whenever the code changes.
  -}
  mProviders <- query _marloweEditorPageSlot unit (Monaco.GetObjects identity)
  case mProviders of
    Just
      { codeActionProvider: Just caProvider
      , completionItemProvider: Just ciProvider
      } -> liftEffect $ updateAdditionalContext caProvider ciProvider
      additionalContext
    _ -> pure unit

editorSetTheme
  :: forall state action msg m. HalogenM state action ChildSlots msg m Unit
editorSetTheme = void $ query _marloweEditorPageSlot unit
  (Monaco.SetTheme MM.daylightTheme.name unit)

editorSetValue
  :: forall state action msg m
   . String
  -> HalogenM state action ChildSlots msg m Unit
editorSetValue contents = void $ query _marloweEditorPageSlot unit
  (Monaco.SetText contents unit)

editorGetValue
  :: forall state action msg m
   . HalogenM state action ChildSlots msg m (Maybe String)
editorGetValue = query _marloweEditorPageSlot unit (Monaco.GetText identity)
