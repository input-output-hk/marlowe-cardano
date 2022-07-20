module Page.BlocklyEditor.State where

import Prologue

import CloseAnalysis (analyseClose)
import Component.Blockly.Types as Blockly
import Component.BottomPanel.State (handleAction) as BottomPanel
import Component.BottomPanel.Types (Action(..), State) as BottomPanel
import Control.Monad.Except (ExceptT(..), except, lift, runExceptT)
import Control.Monad.Maybe.Extra (hoistMaybe)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array as Array
import Data.Either (hush, note)
import Data.Lens (assign, modifying, over, set, use, view)
import Data.Map as Map
import Data.Map.Ordered.OMap as OMap
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Minutes(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Examples.Marlowe.Contracts (example) as ME
import Halogen (HalogenM, modify_)
import Halogen as H
import Halogen.Extra (mapSubmodule)
import Language.Marlowe.Extended.V1 as Extended
import Language.Marlowe.Extended.V1.Metadata (MetaData)
import MainFrame.Types (ChildSlots, _blocklySlot)
import Marlowe (Api)
import Marlowe.Blockly as MB
import Marlowe.Holes as Holes
import Marlowe.Linter as Linter
import Marlowe.Template (TemplateContent)
import Marlowe.Template as Template
import Page.BlocklyEditor.Types
  ( Action(..)
  , BottomPanelView
  , State
  , _bottomPanelState
  , _errorMessage
  , _hasHoles
  , _marloweCode
  , _metadataHintInfo
  , _warnings
  )
import Servant.PureScript (class MonadAjax)
import SessionStorage as SessionStorage
import Simulator.Lenses (_templateContent)
import StaticAnalysis.Reachability
  ( analyseReachability
  , getUnreachableContracts
  )
import StaticAnalysis.StaticTools (analyseContract)
import StaticAnalysis.Types
  ( AnalysisExecutionState(..)
  , _analysisExecutionState
  , _analysisState
  )
import StaticData (marloweBufferLocalStorageKey)
import Text.Pretty (pretty)

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
handleAction metadata (HandleBlocklyMessage Blockly.BlocklyReady) = do
  mContents <- liftEffect $ SessionStorage.getItem marloweBufferLocalStorageKey
  handleAction metadata $ InitBlocklyProject $ fromMaybe ME.example mContents

handleAction metadata (HandleBlocklyMessage Blockly.CodeChange) = do
  clearAnalysisResults
  processBlocklyCode metadata

handleAction _ (HandleBlocklyMessage (Blockly.BlockSelection selection)) =
  case mBlockDefinition of
    Nothing -> H.tell _blocklySlot unit $ Blockly.SetToolbox MB.toolbox
    Just definition -> H.tell _blocklySlot unit $ Blockly.SetToolbox $
      MB.toolboxWithHoles definition
  where
  mBlockDefinition = do
    { blockType } <- selection
    Map.lookup blockType MB.definitionsMap

handleAction metadata (InitBlocklyProject code) = do
  -- NOTE: We already save the code in session storage as part of the processBlocklyCode.
  --       The reason to also add it here was to solve a small bug introduced in PR 3478.
  --       The problem is that when we create a new project, the BlocklyReady event is
  --       fired some time between the first InitBlocklyProject and processBlocklyCode, so
  --       a second InitBlocklyProject was called with the previous code.
  --       Saving the code twice solves the problem but we are still firing 2 InitBlocklyProject
  --       when we start a new project, so we might want to revisit this later.
  liftEffect $ SessionStorage.setItem marloweBufferLocalStorageKey code
  H.tell _blocklySlot unit $ Blockly.SetCode code
  processBlocklyCode metadata
  -- Reset the toolbox
  H.tell _blocklySlot unit $ Blockly.SetToolbox MB.toolbox

handleAction _ SendToSimulator = pure unit

handleAction _ ViewAsMarlowe = pure unit

handleAction _ Save = pure unit

handleAction metadata (BottomPanelAction (BottomPanel.PanelAction action)) =
  handleAction metadata action

handleAction _ (BottomPanelAction action) = toBottomPanel
  (BottomPanel.handleAction action)

handleAction _ (SetValueTemplateParam key value) = do
  clearAnalysisResults
  modifying
    (_analysisState <<< _templateContent <<< Template._valueContent)
    (Map.insert key value)

handleAction _ (SetTimeTemplateParam key value) = do
  clearAnalysisResults
  modifying
    (_analysisState <<< _templateContent <<< Template._timeContent)
    (Map.insert key value)

handleAction _ (MetadataAction _) = pure unit

handleAction metadata AnalyseContract = runAnalysis metadata analyseContract

handleAction metadata AnalyseReachabilityContract = runAnalysis metadata
  analyseReachability

handleAction metadata AnalyseContractForCloseRefund = runAnalysis metadata
  analyseClose

handleAction _ (SelectWarning warning) = H.tell _blocklySlot unit $
  Blockly.SelectWarning warning

clearAnalysisResults :: forall m. HalogenM State Action ChildSlots Void m Unit
clearAnalysisResults = assign
  (_analysisState <<< _analysisExecutionState)
  NoneAsked

-- This function reads the Marlowe code from blockly and, process it and updates the component state
processBlocklyCode
  :: forall m
   . MonadAff m
  => MetaData
  -> HalogenM State Action ChildSlots Void m Unit
processBlocklyCode metadata = do
  eContract <-
    runExceptT do
      block <- ExceptT $ note "Blockly Workspace is empty" <$> H.request
        _blocklySlot
        unit
        Blockly.GetBlockRepresentation
      except $ MB.blockToContract block

  currentTime <- liftEffect now
  case eContract of
    Left e ->
      modify_
        ( set _errorMessage (Just $ unexpected e)
            <<< set _marloweCode Nothing
        )
    Right holesContract -> do
      analysisExecutionState <- use (_analysisState <<< _analysisExecutionState)
      let
        unreachableContracts = getUnreachableContracts analysisExecutionState

        lintingState = Linter.lint unreachableContracts holesContract

        prettyContract = show $ pretty holesContract

        -- If we can get an Extended contract from the holes contract (basically if it has no holes)
        -- then update the template content. If not, leave them as they are
        maybeUpdateTemplateContent :: TemplateContent -> TemplateContent
        maybeUpdateTemplateContent = case Holes.fromTerm holesContract of
          Just (contract :: Extended.Contract) -> Template.updateTemplateContent
            currentTime
            (Minutes 5.0)
            (OMap.keys metadata.timeParameterDescriptions)
            (Template.getPlaceholderIds contract)
          Nothing -> identity
      liftEffect $ SessionStorage.setItem marloweBufferLocalStorageKey
        prettyContract
      modify_
        ( set _errorMessage Nothing
            <<< set _marloweCode (Just $ prettyContract)
            <<< set _hasHoles (Linter.hasHoles $ lintingState)
            <<< set _warnings
              (Array.fromFoldable $ view Linter._warnings lintingState)
            <<< set _metadataHintInfo (view Linter._metadataHints lintingState)
            <<< over (_analysisState <<< _templateContent)
              maybeUpdateTemplateContent
        )
  where
  unexpected s =
    "An unexpected error has occurred, please raise a support issue at https://github.com/input-output-hk/marlowe-cardano/issues/new: "
      <> s

runAnalysis
  :: forall m
   . MonadAff m
  => MetaData
  -> (Extended.Contract -> HalogenM State Action ChildSlots Void m Unit)
  -> HalogenM State Action ChildSlots Void m Unit
runAnalysis metadata doAnalyze =
  void
    $ runMaybeT do
        block <- MaybeT $ H.request _blocklySlot unit
          Blockly.GetBlockRepresentation
        -- FIXME: See if we can use runExceptT and show the error somewhere
        contract <- MaybeT $ pure $ Holes.fromTerm =<<
          (hush $ MB.blockToContract block)
        lift do
          doAnalyze contract
          processBlocklyCode metadata

editorGetValue
  :: forall state action msg m
   . HalogenM state action ChildSlots msg m (Maybe String)
editorGetValue =
  runMaybeT do
    block <- MaybeT $ H.request _blocklySlot unit Blockly.GetBlockRepresentation
    contract <- hoistMaybe $ hush $ MB.blockToContract block
    pure $ show $ pretty $ contract
