module MainFrame.State (component) where

import Prologue hiding (div)

import CallByName.Alt ((<|>))
import Component.Blockly.Types as Blockly
import Component.BottomPanel.Types (Action(..)) as BP
import Component.ConfirmUnsavedNavigation.Types (Action(..)) as ConfirmUnsavedNavigation
import Component.Demos.Types (Action(..), Demo(..)) as Demos
import Component.MetadataTab.State (carryMetadataAction)
import Component.NewProject.Types (Action(..), emptyState) as NewProject
import Contrib.Halogen.Store (getsStore) as HS
import Contrib.Halogen.Store (useStore)
import Contrib.Halogen.Store.Monad (preuseStore)
import Control.Bind (bindFlipped)
import Control.Monad.Except (ExceptT(..), lift, runExceptT)
import Control.Monad.Maybe.Extra (hoistMaybe)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.State (modify_)
import Data.Argonaut.Extra (encodeStringifyJson, parseDecodeJson)
import Data.Bifunctor (lmap)
import Data.Either (hush, note)
import Data.Foldable (fold, for_)
import Data.Function (on)
import Data.Function.Uncurried (runFn2)
import Data.Lens (_Just, assign, has, over, preview, set, use, (^.))
import Data.Lens.Extra (peruse)
import Data.Lens.Index (ix)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (un, unwrap)
import Data.RawJson (RawJson(..))
import Debug (traceM)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Gist (gistId)
import Gists.Extra (GistId)
import Gists.Types (GistAction(..))
import Gists.Types (parseGistUrl) as Gists
import Halogen (Component, getRef, liftEffect, subscribe')
import Halogen as H
import Halogen.Analytics (withAnalytics)
import Halogen.Extra (mapSubmodule)
import Halogen.Monaco (KeyBindings(DefaultBindings))
import Halogen.Monaco as Monaco
import Halogen.Query (HalogenM)
import Halogen.Query.Event (eventListener)
import Halogen.Store.Connect (connect) as HS
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Monad (updateStore) as HS
import Halogen.Store.Select (select, selectEq) as HS
import Halogen.VDom.Util (refEq)
import MainFrame.Types
  ( Action(..)
  , ChildSlots
  , Input
  , ModalView(..)
  , Query(..)
  , State
  , StoreContext
  , View(..)
  , _authStatus
  , _blocklyEditorState
  , _contractMetadata
  , _contractMetadata'
  , _createGistResult
  , _gistId
  , _hasUnsavedChanges
  , _haskellState
  , _input
  , _javascriptState
  , _loadGistResult
  , _marloweEditorState
  , _project
  , _rename
  , _saveAs
  , _showBottomPanel
  , _showModal
  , _simulationState
  , _view
  , _workflow
  , modalBackdropLabel
  )
import MainFrame.View (render)
import Marlowe (Api)
import Marlowe as Server
import Marlowe.Extended.Metadata (emptyContractMetadata, getHintsFromMetadata)
import Marlowe.Gists (PlaygroundFiles)
import Network.RemoteData (RemoteData(..), _Success, fromEither)
import Page.BlocklyEditor.State as BlocklyEditor
import Page.BlocklyEditor.Types (_marloweCode)
import Page.BlocklyEditor.Types as BE
import Page.HaskellEditor.State as HaskellEditor
import Page.HaskellEditor.Types
  ( Action(..)
  , State
  , _ContractString
  , _metadataHintInfo
  , initialState
  ) as HE
import Page.JavascriptEditor.State as JavascriptEditor
import Page.JavascriptEditor.Types
  ( Action(..)
  , State
  , _ContractString
  , _metadataHintInfo
  , initialState
  ) as JS
import Page.JavascriptEditor.Types (CompilationState(..))
import Page.MarloweEditor.State as MarloweEditor
import Page.MarloweEditor.Types as ME
import Page.Simulation.State as Simulation
import Page.Simulation.Types as ST
import Project
  ( Language(..)
  , Project
  , ProjectName(..)
  , SourceCode(..)
  , Workflow
      ( JavascriptWorkflow
      , HaskellWorkflow
      , BlocklyWorkflow
      , MarloweWorkflow
      )
  )
import Project as Project
import Rename.State (handleAction) as Rename
import Rename.Types (Action(..), State, emptyState) as Rename
import Rename.Types as Rename.Types
import Router (Route, SubRoute)
import Router as Router
import Routing.Duplex as RD
import Routing.Hash as Routing
import SaveAs.State (handleAction) as SaveAs
import SaveAs.Types (Action(..), State, _status, emptyState) as SaveAs
import SaveAs.Types as SaveAs.Types
import Servant.PureScript (class MonadAjax, printAjaxError)
import Session as Auth
import SessionStorage as SessionStorage
import StaticData as StaticData
import Store (Action, State(..), _State, reset) as Store
import Store.AuthState as Store.AuthState
import Store.OverlayState (OverlayState, OverlayStateAction(..))
import Store.OverlayState as Store.OverlayState
import Store.ProjectState (ProjectStateAction(..), _projectState)
import Store.ProjectState as Store.ProjectState
import Type.Constraints (class MonadAffAjaxStore)
import Types (WebpackBuildMode(..))
import Web.Event.Event as Event
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document) as Web
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes (keyup)
import Web.UIEvent.MouseEvent as MouseEvent

-- We use `storeSelectorFn` and `input` from state to
-- perform full state reset during logout.
storeSelectorFn :: Store.State -> StoreContext
storeSelectorFn = \(Store.State { overlayState, projectState }) ->
  { overlayState, projectState }

initialState
  :: { input :: Input, context :: StoreContext } -> State
initialState { input: input@{ tzOffset, webpackBuildMode }, context } =
  { input
  , overlayState: context.overlayState
  , view: HomePage
  , jsCompilationResult: NotCompiled
  , showBottomPanel: true
  , haskellState: HE.initialState tzOffset
  , javascriptState: JS.initialState tzOffset
  , marloweEditorState: ME.initialState tzOffset
  , blocklyEditorState: BE.initialState tzOffset
  , simulationState: Simulation.mkState tzOffset
  , jsEditorKeybindings: DefaultBindings
  , activeJSDemo: mempty
  , newProject: NewProject.emptyState
  , rename: Rename.emptyState
  , saveAs: SaveAs.emptyState
  , authStatus: NotAsked
  , gistId: Nothing
  , createGistResult: NotAsked
  , loadGistResult: Right NotAsked
  , showModal: Nothing
  , hasUnsavedChanges: false
  -- , workflow: Nothing
  , project: _.project <$> context.projectState
  , featureFlags:
      { fsProjectStorage: webpackBuildMode == Development
      , logout: webpackBuildMode == Development
      }
  }

------------------------------------------------------------
component
  :: forall m
   . MonadAffAjaxStore m
  => Component Query Input Void m
component = do
  let
    storeContextEq = eq `on` (map _.version <<< _.projectState)
      && eq `on` _.overlayState
  HS.connect (HS.select storeContextEq storeSelectorFn) $
    H.mkComponent
      { initialState
      , render
      , eval:
          H.mkEval
            { handleQuery
            , handleAction: fullHandleAction
            , receive: Just <<< Receive
            , initialize: Just Init
            , finalize: Nothing
            }
      }

toSimulation
  :: forall m a
   . Functor m
  => HalogenM ST.State ST.Action ChildSlots Void m a
  -> HalogenM State Action ChildSlots Void m a
toSimulation = mapSubmodule _simulationState SimulationAction

toHaskellEditor
  :: forall m a
   . Functor m
  => HalogenM HE.State HE.Action ChildSlots Void m a
  -> HalogenM State Action ChildSlots Void m a
toHaskellEditor = mapSubmodule _haskellState HaskellAction

toMarloweEditor
  :: forall m a
   . Functor m
  => HalogenM ME.State ME.Action ChildSlots Void m a
  -> HalogenM State Action ChildSlots Void m a
toMarloweEditor = mapSubmodule _marloweEditorState MarloweEditorAction

toJavascriptEditor
  :: forall m a
   . Functor m
  => HalogenM JS.State JS.Action ChildSlots Void m a
  -> HalogenM State Action ChildSlots Void m a
toJavascriptEditor = mapSubmodule _javascriptState JavascriptAction

toBlocklyEditor
  :: forall m a
   . Functor m
  => HalogenM BE.State BE.Action ChildSlots Void m a
  -> HalogenM State Action ChildSlots Void m a
toBlocklyEditor = mapSubmodule _blocklyEditorState BlocklyEditorAction

-- toProjects
--   :: forall m a
--    . Functor m
--   => HalogenM Projects.State Projects.Action ChildSlots Void m a
--   -> HalogenM State Action ChildSlots Void m a
-- toProjects = mapSubmodule _projects ProjectsAction

toRename
  :: forall m a
   . Functor m
  => HalogenM Rename.State Rename.Action ChildSlots Void m a
  -> HalogenM State Action ChildSlots Void m a
toRename = mapSubmodule _rename RenameAction

toSaveAs
  :: forall m a
   . Functor m
  => HalogenM SaveAs.State SaveAs.Action ChildSlots Void m a
  -> HalogenM State Action ChildSlots Void m a
toSaveAs = mapSubmodule _saveAs SaveAsAction

------------------------------------------------------------
handleSubRoute
  :: forall m
   . MonadAff m
  => MonadAjax Api m
  => SubRoute
  -> HalogenM State Action ChildSlots Void m Unit
handleSubRoute Router.Home = selectView HomePage

handleSubRoute Router.Simulation = selectView Simulation

handleSubRoute Router.MarloweEditor = selectView MarloweEditor

handleSubRoute Router.HaskellEditor = selectView HaskellEditor

handleSubRoute Router.JSEditor = selectView JSEditor

handleSubRoute Router.Blockly = selectView BlocklyEditor

-- This route is supposed to be called by the github oauth flow after a succesful login flow
-- It is supposed to be run inside a popup window
handleSubRoute Router.GithubAuthCallback = do
  liftEffect Auth.onAuthResponse

handleRoute
  :: forall m
   . MonadAffAjaxStore m
  => Route
  -> HalogenM State Action ChildSlots Void m Unit
handleRoute { gistId: (Just gistId), subroute } = do
  handleActionWithoutNavigationGuard (GistAction (SetGistUrl (unwrap gistId)))
  handleActionWithoutNavigationGuard (GistAction LoadGist)
  handleSubRoute subroute

handleRoute { subroute } = handleSubRoute subroute

handleQuery
  :: forall m a
   . MonadAffAjaxStore m
  => Query a
  -> HalogenM State Action ChildSlots Void m (Maybe a)
handleQuery (ChangeRoute route next) = do
  -- Without the following each route is handled twice, once when we call selectView ourselves
  -- and another which is triggered in Main, when the route changes.
  currentView <- use _view
  when (routeToView route /= Just currentView) $ handleRoute route
  pure $ Just next

------------------------------------------------------------
fullHandleAction
  :: forall m
   . MonadAffAjaxStore m
  => Action
  -> HalogenM State Action ChildSlots Void m Unit
fullHandleAction =
  withAccidentalNavigationGuard
    $ withSessionStorage
    $ withAnalytics
        handleAction

handleActionWithoutNavigationGuard
  :: forall m
   . MonadAffAjaxStore m
  => Action
  -> HalogenM State Action ChildSlots Void m Unit
handleActionWithoutNavigationGuard =
  withSessionStorage
    $ withAnalytics
        ( handleAction
        )

assignWorkflow
  :: forall m
   . MonadAffAjaxStore m
  => Workflow
  -> HalogenM State Action ChildSlots Void m Unit
assignWorkflow workflow = do
  modify_ $ over _project $ map $ flip Project.trySetWorkflow workflow

closeModal = do
  updateStore $ Store.OverlayState.action $ ReleaseBackdrop
  assign _showModal Nothing

loadProject project = do
  let
    metadataHints = project ^. Project._metadataHints
    metadata = project ^. Project._metadata
    SourceCode code = project ^. Project._code
    workflow = Project.getWorkflow project

  case workflow of
    HaskellWorkflow -> toHaskellEditor
      $ HaskellEditor.handleAction metadata
      $ HE.InitHaskellProject metadataHints
      $ code
    JavascriptWorkflow -> toJavascriptEditor
      $ JavascriptEditor.handleAction metadata
      $ JS.InitJavascriptProject metadataHints
      $ code
    MarloweWorkflow -> toMarloweEditor
      $ MarloweEditor.handleAction metadata
      $ ME.InitMarloweProject code
    BlocklyWorkflow -> toBlocklyEditor
      $ BlocklyEditor.handleAction metadata
      $ BE.InitBlocklyProject code

  assign _project (Just project)
  assign (_haskellState <<< HE._metadataHintInfo) metadataHints
  assign (_javascriptState <<< JS._metadataHintInfo) metadataHints

  updateStore $ Store.ProjectState.action $ OnProjectLoaded $ project
  selectView $ workflowView workflow

loadProjectFromSource projectName workflow sourceCode@(SourceCode code) metadata =
  do
    let
      project =
        flip Project.trySetWorkflow workflow
          <<< flip Project.setProjectName projectName
          <<< flip Project.setMetadata metadata
          $ Project.fromSourceCode
              sourceCode
              (Project.workflowLanguage workflow)
    loadProject project

-- This handleAction can be called recursively, but because we use HOF to extend the functionality
-- of the component, whenever we need to recurse we most likely be calling one of the extended functions
-- defined above (handleActionWithoutNavigationGuard or fullHandleAction)
handleAction
  :: forall m
   . MonadAffAjaxStore m
  => Action
  -> HalogenM State Action ChildSlots Void m Unit
handleAction Init = do
  hash <- liftEffect Routing.getHash
  case (RD.parse Router.route) hash of
    Right route -> handleRoute route
    Left _ -> handleRoute { subroute: Router.Home, gistId: Nothing }
  document <- liftEffect $ Web.document =<< Web.window
  subscribe' \sid ->
    eventListener keyup (toEventTarget document)
      (map (HandleKey sid) <<< KE.fromEvent)
  -- Load session data if available
  void
    $ runMaybeT do
        bundleJSON <- MaybeT $ liftEffect $ SessionStorage.getItem
          StaticData.sessionStorageKey
        project <- hoistMaybe $ do
          bundle <- hush $ parseDecodeJson bundleJSON
          pure $ Project.fromBundle Nothing bundle
        let
          metadataHints = project ^. Project._metadataHints
        H.modify_ $ _ { project = Just project }
          <<< set (_haskellState <<< HE._metadataHintInfo) metadataHints
          <<< set (_javascriptState <<< JS._metadataHintInfo) metadataHints

  checkAuthStatus

handleAction (Receive { context }) = do
  traceM context
  H.modify_ _
    { overlayState = context.overlayState
    , project = _.project <$> context.projectState
    }

handleAction (HandleKey _ ev)
  | KE.key ev == "Escape" = closeModal
  | KE.key ev == "Enter" = do
      modalView <- use _showModal
      case modalView of
        Just RenameProject -> handleAction (RenameAction Rename.SaveProject)
        Just SaveProjectAs -> handleAction (SaveAsAction SaveAs.SaveProject)
        _ -> pure unit
  | otherwise = pure unit

handleAction (HaskellAction action) = do
  metadata <- use _contractMetadata'
  toHaskellEditor (HaskellEditor.handleAction metadata action)
  case action of
    HE.SendResultToSimulator -> do
      mContract <- peruse (_haskellState <<< HE._ContractString)
      let
        contract = fold mContract
      sendToSimulation contract
    HE.HandleEditorMessage (Monaco.TextChanged _) ->
      setUnsavedChangesForLanguage HaskellWorkflow true
    HE.InitHaskellProject _ _ -> setUnsavedChangesForLanguage HaskellWorkflow
      false
    HE.BottomPanelAction (BP.PanelAction (HE.MetadataAction metadataAction)) ->
      carryMetadataAction metadataAction
    _ -> pure unit

handleAction (JavascriptAction action) = do
  metadata <- use _contractMetadata'

  toJavascriptEditor (JavascriptEditor.handleAction metadata action)
  case action of
    JS.SendResultToSimulator -> do
      mContract <- peruse (_javascriptState <<< JS._ContractString)
      let
        contract = fold mContract
      sendToSimulation contract
    JS.HandleEditorMessage (Monaco.TextChanged _) ->
      setUnsavedChangesForLanguage JavascriptWorkflow true
    JS.InitJavascriptProject _ _ -> setUnsavedChangesForLanguage
      JavascriptWorkflow
      false
    JS.BottomPanelAction (BP.PanelAction (JS.MetadataAction metadataAction)) ->
      carryMetadataAction metadataAction
    _ -> pure unit

handleAction (MarloweEditorAction action) = do
  metadata <- use _contractMetadata'
  toMarloweEditor $ MarloweEditor.handleAction metadata action
  case action of
    ME.SendToSimulator -> do
      mContents <- MarloweEditor.editorGetValue
      for_ mContents \contents ->
        sendToSimulation contents
    ME.ViewAsBlockly -> do
      mSource <- MarloweEditor.editorGetValue
      for_ mSource \source -> do
        void $ toBlocklyEditor $ BlocklyEditor.handleAction metadata $
          BE.InitBlocklyProject source
        assignWorkflow BlocklyWorkflow
        selectView BlocklyEditor
    ME.HandleEditorMessage (Monaco.TextChanged _) ->
      setUnsavedChangesForLanguage MarloweWorkflow true
    ME.InitMarloweProject _ -> setUnsavedChangesForLanguage MarloweWorkflow
      false
    ME.BottomPanelAction (BP.PanelAction (ME.MetadataAction metadataAction)) ->
      carryMetadataAction metadataAction
    _ -> pure unit

handleAction (BlocklyEditorAction action) = do
  metadata <- use _contractMetadata'

  toBlocklyEditor $ BlocklyEditor.handleAction metadata action
  case action of
    BE.SendToSimulator -> do
      mCode <- use (_blocklyEditorState <<< _marloweCode)
      for_ mCode \contents -> sendToSimulation contents
    BE.ViewAsMarlowe -> do
      -- TODO: doing an effect that returns a maybe value and doing an action on the possible
      -- result is a pattern that we have repeated a lot in this file. See if we could refactor
      -- into something like this: https://github.com/input-output-hk/plutus/pull/2560#discussion_r549892291
      mCode <- use (_blocklyEditorState <<< _marloweCode)
      for_ mCode \code -> do
        selectView MarloweEditor
        assignWorkflow MarloweWorkflow
        toMarloweEditor $ MarloweEditor.handleAction metadata $
          ME.InitMarloweProject
            code
    BE.HandleBlocklyMessage Blockly.CodeChange -> setUnsavedChangesForLanguage
      BlocklyWorkflow
      true
    BE.BottomPanelAction (BP.PanelAction (BE.MetadataAction metadataAction)) ->
      carryMetadataAction metadataAction
    _ -> pure unit

handleAction (SimulationAction action) = do
  metadata <- use _contractMetadata'
  toSimulation (Simulation.handleAction metadata action)
  case action of
    ST.EditSource -> do
      mLang <- peruse _workflow
      for_ mLang \lang -> selectView $ workflowView lang
    _ -> pure unit

handleAction (ChangeView view) = selectView view

handleAction (ShowBottomPanel val) = do
  assign _showBottomPanel val
  pure unit

handleAction (LoadProject project) = loadProject project

handleAction (NewProjectAction (NewProject.CreateProject workflow)) = do
  let
    maybeCode = case workflow of
      HaskellWorkflow -> Map.lookup "Example" StaticData.demoFiles
      JavascriptWorkflow -> Map.lookup "Example" StaticData.demoFilesJS
      MarloweWorkflow -> Map.lookup "Example" StaticData.marloweContracts
      BlocklyWorkflow -> Map.lookup "Example" StaticData.marloweContracts
  case maybeCode of
    Just code -> loadProjectFromSource
      Nothing
      workflow
      (SourceCode code)
      emptyContractMetadata
    Nothing -> pure unit
  closeModal

handleAction (NewProjectAction NewProject.Cancel) = fullHandleAction
  (CloseModal Nothing)

handleAction (DemosAction (Demos.LoadDemo workflow (Demos.Demo key))) = do
  let
    maybeCode = case workflow of
      HaskellWorkflow -> Map.lookup key StaticData.demoFiles
      JavascriptWorkflow -> Map.lookup key StaticData.demoFilesJS
      MarloweWorkflow -> (preview (ix key) StaticData.marloweContracts)
      BlocklyWorkflow -> (preview (ix key) StaticData.marloweContracts)
    metadata = fromMaybe emptyContractMetadata $ Map.lookup key
      StaticData.demoFilesMetadata
  case maybeCode of
    Just code -> loadProjectFromSource
      (Just $ ProjectName metadata.contractName)
      workflow
      (SourceCode code)
      metadata
    Nothing -> pure unit

  closeModal

handleAction (DemosAction Demos.Cancel) = fullHandleAction (CloseModal Nothing)

handleAction (RenameAction action@Rename.SaveProject) = do
  projectName <- use (_rename <<< Rename.Types._projectName)
  updateStore $ Store.ProjectState.action $ OnProjectNameChanged $ ProjectName
    projectName
  closeModal
  toRename $ Rename.handleAction action

handleAction (RenameAction action) = toRename $ Rename.handleAction action

handleAction (SaveAsAction action@SaveAs.SaveProject) = do
  currentGistId <- use _gistId
  projectName <- use (_saveAs <<< SaveAs.Types._projectName)

  assign _gistId Nothing
  assign (_saveAs <<< SaveAs._status) Loading

  handleGistAction PublishOrUpdateGist
  res <- peruse (_createGistResult <<< _Success)
  case res of
    Just gist -> do
      updateStore $ Store.ProjectState.action $ OnProjectNameChanged $
        ProjectName projectName
      closeModal
      assign (_saveAs <<< SaveAs._status) NotAsked
    Nothing -> do
      assign (_saveAs <<< SaveAs._status) (Failure "Could not save project")
      assign _gistId currentGistId
  toSaveAs $ SaveAs.handleAction action

handleAction (SaveAsAction SaveAs.Cancel) = fullHandleAction
  (CloseModal Nothing)

handleAction (SaveAsAction action) = toSaveAs $ SaveAs.handleAction action

handleAction CheckAuthStatus = checkAuthStatus

handleAction (GistAction subEvent) = handleGistAction subEvent

handleAction (OpenModal OpenProject) = do
  updateStore $ Store.OverlayState.action $ UseBackdrop
  assign _showModal $ Just OpenProject
--   toProjects $ Projects.handleAction Projects.LoadProjects

handleAction (OpenModal RenameProject) = do
  updateStore $ Store.OverlayState.action $ UseBackdrop
  currentName <- peruse (_project <<< _Just <<< Project._projectName <<< _Just)
  assign (_rename <<< Rename.Types._projectName)
    (maybe "" (un ProjectName) currentName)
  assign _showModal $ Just RenameProject

handleAction (OpenModal modalView) = do
  updateStore $ Store.OverlayState.action $ UseBackdrop
  assign _showModal $ Just modalView

handleAction (CloseModal maybeAction) = do
  closeModal
  case maybeAction of
    Just action -> handleAction action
    Nothing -> pure unit

handleAction (ModalBackdropClick mouseEvent) = do
  maybeBackdropElement <- getRef modalBackdropLabel
  let
    event = MouseEvent.toEvent mouseEvent
    backdropClicked = fromMaybe false do
      target <- Event.target event
      targetElement <-
        HTMLElement.toElement <$> HTMLElement.fromEventTarget target
      backdropElement <- maybeBackdropElement
      pure $ runFn2 refEq backdropElement targetElement

  when backdropClicked $ do
    handleAction (CloseModal Nothing)

-- | FIXME: We should probably accept here also continuation for
-- | unsuccessful login.
handleAction (OpenLoginPopup intendedAction) = do
  possiblySession <- liftAff Auth.login
  -- | FIXME: We should inform the user about the result.
  -- | We get full information back.
  fullHandleAction (CloseModal Nothing)
  assignAuthStatus possiblySession
  case possiblySession of
    Success (Just _) -> fullHandleAction intendedAction
    _ -> pure unit

handleAction (ConfirmUnsavedNavigationAction intendedAction modalAction) =
  handleConfirmUnsavedNavigationAction intendedAction modalAction

handleAction Logout = do
  lift Server.getApiLogout >>= case _ of
    -- TODO: Proper error reporting
    Left err -> pure unit
    Right (RawJson _) -> do
      (input :: Input) <- use _input
      selectView HomePage
      context <- HS.getsStore storeSelectorFn
      H.put $ ((initialState { input, context }) :: State)
      HS.updateStore $ Store.reset
      handleAction Init

sendToSimulation
  :: forall m
   . MonadAffAjaxStore m
  => String
  -> HalogenM State Action ChildSlots Void m Unit
sendToSimulation contract = do
  metadata <- use _contractMetadata'
  selectView Simulation
  toSimulation $ Simulation.handleAction metadata (ST.LoadContract contract)

workflowView :: Workflow -> View
workflowView = case _ of
  HaskellWorkflow -> HaskellEditor
  MarloweWorkflow -> MarloweEditor
  BlocklyWorkflow -> BlocklyEditor
  JavascriptWorkflow -> JSEditor

routeToView :: Route -> Maybe View
routeToView { subroute } = case subroute of
  Router.Home -> Just HomePage
  Router.Simulation -> Just Simulation
  Router.HaskellEditor -> Just HaskellEditor
  Router.MarloweEditor -> Just MarloweEditor
  Router.JSEditor -> Just JSEditor
  Router.Blockly -> Just BlocklyEditor
  Router.GithubAuthCallback -> Nothing

viewToRoute :: View -> Router.SubRoute
viewToRoute = case _ of
  HomePage -> Router.Home
  MarloweEditor -> Router.MarloweEditor
  Simulation -> Router.Simulation
  HaskellEditor -> Router.HaskellEditor
  JSEditor -> Router.JSEditor
  BlocklyEditor -> Router.Blockly

------------------------------------------------------------

assignAuthStatus
  :: forall m
   . MonadAff m
  => MonadAjax Api m
  => MonadStore Store.Action Store.State m
  => Auth.AuthResponse
  -> HalogenM State Action ChildSlots Void m Unit
assignAuthStatus status = do
  assign _authStatus status
  updateStore $ Store.AuthState.action status

checkAuthStatus
  :: forall m
   . MonadAffAjaxStore m
  => HalogenM State Action ChildSlots Void m Unit
checkAuthStatus = do
  assignAuthStatus Loading
  session <- lift Auth.fetchSession
  assignAuthStatus session

------------------------------------------------------------
createFiles
  :: forall m
   . MonadAff m
  => MonadAjax Api m
  => HalogenM State Action ChildSlots Void m PlaygroundFiles
createFiles = do
  let
    pruneEmpty :: forall a. Eq a => Monoid a => Maybe a -> Maybe a
    pruneEmpty (Just v)
      | v == mempty = Nothing

    pruneEmpty m = m

    -- playground is a meta-data file that we currently just use as a tag to check if a gist is a marlowe playground gist
    playground = "{}"
  metadata <- Just <$> encodeStringifyJson <$> use _contractMetadata'
  workflow <- peruse _workflow
  let
    emptyFiles = (mempty :: PlaygroundFiles)
      { playground = playground, metadata = metadata }
  case workflow of
    Just MarloweWorkflow -> do
      marlowe <- pruneEmpty <$> MarloweEditor.editorGetValue
      pure $ emptyFiles { marlowe = marlowe }
    Just BlocklyWorkflow -> do
      blockly <- pruneEmpty <$> BlocklyEditor.editorGetValue
      pure $ emptyFiles { blockly = blockly }
    Just HaskellWorkflow -> do
      haskell <- pruneEmpty <$> HaskellEditor.editorGetValue
      pure $ emptyFiles { haskell = haskell }
    Just JavascriptWorkflow -> do
      javascript <- pruneEmpty <$> toJavascriptEditor
        JavascriptEditor.editorGetValue
      pure $ emptyFiles { javascript = javascript }
    Nothing -> mempty

handleGistAction
  :: forall m
   . MonadAff m
  => MonadAjax Api m
  => GistAction
  -> HalogenM State Action ChildSlots Void m Unit
handleGistAction PublishOrUpdateGist = do
  pure unit
-- | FIXME: paluh
--   description <- use _projectName
--   files <- createFiles
--   let
--     newGist = mkNewGist description $ files
--   void
--     $ runMaybeT do
--         mGist <- use _gistId
--         assign _createGistResult Loading
--         newResult <-
--           lift
--             $ lift
--             $ case mGist of
--                 Nothing -> Server.postApiGists newGist
--                 Just gistId -> Server.postApiGistsByGistId newGist gistId
--         assign _createGistResult $ fromEither newResult
--         gistId <- hoistMaybe $ preview (_Right <<< gistId) newResult
--         modify_
--           ( set _gistId (Just gistId)
--               <<< set _loadGistResult (Right NotAsked)
--               <<< set _hasUnsavedChanges false
--           )

handleGistAction (SetGistUrl url) = do
  case Gists.parseGistUrl url of
    Right newGistUrl ->
      modify_
        ( set _createGistResult NotAsked
            <<< set _loadGistResult (Right NotAsked)
            <<< set _gistId (Just newGistUrl)
        )
    Left _ -> pure unit

-- TODO: This action is only called when loading the site with a gistid param, something like
-- https://<base_url>/#/marlowe?gistid=<gist_id>
-- But it's not loading the gist correctly. For now I'm leaving it as it is, but we should rethink
-- this functionality in the redesign.
--
-- A separate issue is that the gistid is loaded in the state instead of passing it as a parameter
-- to the LoadGist action
-- https://github.com/input-output-hk/plutus/pull/2498#discussion_r533478042
handleGistAction LoadGist = do
  res <-
    runExceptT
      $ do
          eGistId <- ExceptT $ note "Gist Id not set." <$> use _gistId
          assign _loadGistResult $ Right Loading
          aGist <- lift $ lift $ Server.getApiGistsByGistId eGistId
          assign _loadGistResult $ Right $ fromEither aGist
          gist <-
            ExceptT
              $ pure
              $ toEither (Left "Gist not loaded.")
              $ lmap printAjaxError
              $ fromEither aGist
          lift $ loadGist (gist ^. gistId) Nothing
          pure aGist
  assign _loadGistResult $ map fromEither res
  where
  toEither :: forall e a. Either e a -> RemoteData e a -> Either e a
  toEither _ (Success a) = Right a

  toEither _ (Failure e) = Left e

  toEither x Loading = x

  toEither x NotAsked = x

-- other gist actions are irrelevant here
handleGistAction _ = pure unit

-- assign _workflow (Just $ Project.getWorkflow project)
-- assign _contractMetadata metadata
-- assign _gistId id
-- assign _projectName description
-- updateStore (

loadGist
  :: forall m
   . MonadAff m
  => MonadAjax Api m
  => GistId
  -> Maybe Workflow
  -> HalogenM State Action ChildSlots Void m Unit
loadGist id possibleWorkflow = do
  assign _createGistResult Loading

-- | FIXME: paluh
-- lift $ getApiGistsByGistId id >>= map Project.fromGist >>> case _ of
--   Left error ->
--     modify_
--       ( set _createGistResult (Failure "Ajax error")
--       -- <<< set (_projects <<< Projects._projects)
--       --   (Failure "Failed to load gist")
--       )
--   Right Nothing ->
--     assign _createGistResult (Failure "Gist Decoding problem")
--   Right (Just project) -> do
--     let
--       project' = fromMaybe project do
--         workflow <- possibleWorkflow
--         Project.setWorkflow project workflow
--       workflow = Project.getWorkflow project'

--     modify_
--       ( set _createGistResult (Success project')
--           <<< set _showModal Nothing
--       )

--     setProject project'

--     --toProjects $ Projects.handleAction action
--     let
--       metadataHints = project' ^. Project._metadataHints
--       metadata = project' ^. Project._metadata
--       SourceCode code = project' ^. Project._code

--     case Project.getWorkflow project' of
--       HaskellWorkflow -> toHaskellEditor
--         $ HaskellEditor.handleAction metadata
--         $ HE.InitHaskellProject metadataHints
--         $ code
--       JavascriptWorkflow -> toJavascriptEditor
--         $ JavascriptEditor.handleAction metadata
--         $ JS.InitJavascriptProject metadataHints
--         $ code
--       MarloweWorkflow -> toMarloweEditor
--         $ MarloweEditor.handleAction metadata
--         $ ME.InitMarloweProject code
--       BlocklyWorkflow -> toBlocklyEditor
--         $ BlocklyEditor.handleAction metadata
--         $ BE.InitBlocklyProject code

--     selectView $ workflowView workflow

-- handleAction (ProjectsAction action@(Projects.LoadProject lang gistId)) = do
--   assign _createGistResult Loading
--   res <-
--     runExceptT
--       $ do
--           gist <- ExceptT $ lift $ getApiGistsByGistId gistId
--           lift $ loadGist gist
--           pure gist
--   case res of
--     Right gist ->
--       modify_
--         ( set _createGistResult (Success gist)
--             <<< set _showModal Nothing
--             <<< set _workflow (Just lang)
--         )
--     Left error ->
--       modify_
--         ( set _createGistResult (Failure error)
--             <<< set (_projects <<< Projects._projects)
--               (Failure "Failed to load gist")
--             <<< set _workflow Nothing
--         )
--   toProjects $ Projects.handleAction action
--   selectView $ workflowView lang

------------------------------------------------------------
-- Handles the actions fired by the Confirm Unsaved Navigation modal
handleConfirmUnsavedNavigationAction
  :: forall m
   . MonadAffAjaxStore m
  => Action
  -> ConfirmUnsavedNavigation.Action
  -> HalogenM State Action ChildSlots Void m Unit
handleConfirmUnsavedNavigationAction intendedAction modalAction = do
  fullHandleAction (CloseModal Nothing)
  case modalAction of
    ConfirmUnsavedNavigation.Cancel -> pure unit
    ConfirmUnsavedNavigation.DontSaveProject ->
      handleActionWithoutNavigationGuard intendedAction
    ConfirmUnsavedNavigation.SaveProject -> do
      state <- H.get
      -- TODO: This was taken from the view, from the gistModal helper. I think we should
      -- refactor into a `Save (Maybe Action)` action. The handler for that should do
      -- this check and call the next action as a continuation
      if
        has (_authStatus <<< _Success <<< _Just)
          state then do
        fullHandleAction $ GistAction PublishOrUpdateGist
        fullHandleAction intendedAction
      else
        fullHandleAction $ OpenModal $ GithubLogin $
          ConfirmUnsavedNavigationAction intendedAction modalAction

setUnsavedChangesForLanguage
  :: forall m
   . Workflow
  -> Boolean
  -> HalogenM State Action ChildSlots Void m Unit
setUnsavedChangesForLanguage workflow value = do
  currWorkflow <- peruse _workflow
  when (currWorkflow == Just workflow)
    $ assign _hasUnsavedChanges value

-- This is a HOF intented to be used on top of handleAction. It prevents the user from accidentally doing an Action that
-- would result in losing the progress.
withAccidentalNavigationGuard
  :: forall m
   . MonadAffAjaxStore m
  => (Action -> HalogenM State Action ChildSlots Void m Unit)
  -> Action
  -> HalogenM State Action ChildSlots Void m Unit
withAccidentalNavigationGuard handleAction' action = do
  currentView <- use _view
  hasUnsavedChanges <- use _hasUnsavedChanges
  if viewIsGuarded currentView && actionIsGuarded && hasUnsavedChanges then
    -- If the action would result in the user losing the work, we present a
    -- modal to confirm, cancel or save the work and we preserve the intended action
    -- to be executed after.
    fullHandleAction $ OpenModal $ ConfirmUnsavedNavigation action
  else
    handleAction' action
  where
  -- Which pages needs to be guarded.
  viewIsGuarded = case _ of
    HomePage -> false
    _ -> true

  -- What actions would result in losing the work.
  actionIsGuarded = case action of
    (ChangeView HomePage) -> true
    (NewProjectAction (NewProject.CreateProject _)) -> true
    -- (ProjectsAction (Projects.LoadProject _ _)) -> true
    (DemosAction (Demos.LoadDemo _ _)) -> true
    _ -> false

------------------------------------------------------------
selectView
  :: forall m action message
   . MonadEffect m
  => View
  -> HalogenM State action ChildSlots message m Unit
selectView view = do
  liftEffect $ Routing.setHash
    (RD.print Router.route { subroute: viewToRoute view, gistId: Nothing })
  assign _view view
  liftEffect do
    window <- Web.window
    Window.scroll 0 0 window
  case view of
    HomePage -> modify_ (set _hasUnsavedChanges false)
    _ -> pure unit

------------------------------------------------------------
withSessionStorage
  :: forall m
   . MonadAff m
  => (Action -> HalogenM State Action ChildSlots Void m Unit)
  -> Action
  -> HalogenM State Action ChildSlots Void m Unit
withSessionStorage handleAction' action = do
  -- We use bundles here becase OMap has no Eq instance yet and so
  -- `Project` has no `Eq` instance as well.
  preBundle <- use _project <#> bindFlipped Project.toBundle
  handleAction' action
  postBundle <- use _project <#> bindFlipped Project.toBundle
  when (preBundle /= postBundle) do
    case postBundle of
      Just bundle -> do
        liftEffect $ SessionStorage.setItem StaticData.sessionStorageKey $
          encodeStringifyJson bundle
      Nothing -> pure unit
