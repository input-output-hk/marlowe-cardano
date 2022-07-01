module MainFrame.Types where

import Prologue hiding (div)

import Analytics (class IsEvent, defaultEvent, toEvent)
import Component.Blockly.Types as Blockly
import Component.ConfirmUnsavedNavigation.Types as ConfirmUnsavedNavigation
import Component.CurrencyInput.Types as CurrencyInput
import Component.DateTimeLocalInput.Types as DateTimeLocalInput
import Component.Demos.Types as Demos
import Component.MetadataTab.Types (MetadataAction)
import Component.NewProject.Types as NewProject
import Component.Projects.Types as Projects.Types
import Component.Tooltip.Types (ReferenceId)
import Data.Generic.Rep (class Generic)
import Data.Lens
  ( Fold'
  , Getter'
  , Lens'
  , Traversal'
  , _Just
  , lens
  , non
  , preview
  , set
  , view
  , (^.)
  )
import Data.Lens.Getter as Getter
import Data.Lens.Record (prop)
import Data.Maybe.First (First)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Minutes)
import Gist (Gist)
import Gists.Extra (GistId)
import Gists.Types (GistAction)
import Halogen (ClassName, RefLabel(..))
import Halogen as H
import Halogen.Classes (activeClass)
import Halogen.Monaco (KeyBindings)
import Halogen.Monaco as Monaco
import Halogen.Store.Connect as HS
import Marlowe.Extended.Metadata (MetaData, emptyContractMetadata)
import Network.RemoteData (RemoteData)
import Page.BlocklyEditor.Types as BE
import Page.HaskellEditor.Types as HE
import Page.JavascriptEditor.Types (CompilationState)
import Page.JavascriptEditor.Types as JS
import Page.MarloweEditor.Types as ME
import Page.Simulation.Types as Simulation
import Project (Project, Workflow)
import Project as Project
import Record (delete, get, insert) as Record
import Rename.Types as Rename
import Router (Route)
import SaveAs.Types as SaveAs
import Session (AuthResponse)
import Session as Auth
import Store as Store
import Store.OverlayState (OverlayState)
import Type.Proxy (Proxy(..))
import Types (WebData, WebpackBuildMode)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

data ModalView
  = NewProject
  | OpenProject
  | OpenDemo
  | RenameProject
  | SaveProjectAs
  | GithubLogin Action
  | ConfirmUnsavedNavigation Action

derive instance genericModalView :: Generic ModalView _

instance showModalView :: Show ModalView where
  show NewProject = "NewProject"
  show OpenProject = "OpenProject"
  show OpenDemo = "OpenDemo"
  show RenameProject = "RenameProject"
  show SaveProjectAs = "SaveProjectAs"
  show (ConfirmUnsavedNavigation _) = "ConfirmUnsavedNavigation"
  show (GithubLogin _) = "GithubLogin"

-- Before adding the intended action to GithubLogin, this instance was being
-- handled by the genericShow. Action does not have a show instance so genericShow
-- does not work. For the moment I've made a manual instance, but not sure why
-- ModalView requires show, or if we should make Action an instance of Show
-- show = genericShow
data Query a = ChangeRoute Route a

type StoreContext = { overlayState :: OverlayState }

data Action
  = Init
  | Receive (HS.Connected StoreContext Input)
  | HandleKey H.SubscriptionId KeyboardEvent
  | HaskellAction HE.Action
  | SimulationAction Simulation.Action
  | BlocklyEditorAction BE.Action
  | MarloweEditorAction ME.Action
  | JavascriptAction JS.Action
  | ShowBottomPanel Boolean
  | ChangeView View
  | ConfirmUnsavedNavigationAction Action ConfirmUnsavedNavigation.Action
  | Logout
  -- blockly
  -- | ProjectsAction Projects.Action
  | NewProjectAction NewProject.Action
  | DemosAction Demos.Action
  | RenameAction Rename.Action
  | SaveAsAction SaveAs.Action
  -- Gist support.
  | CheckAuthStatus
  | GistAction GistAction
  | OpenModal ModalView
  | CloseModal (Maybe Action)
  | ModalBackdropClick MouseEvent
  | OpenLoginPopup Action
  | LoadProject Project

-- | Here we decide which top-level queries to track as GA events, and
-- how to classify them.
instance actionIsEvent :: IsEvent Action where
  toEvent Init = Just $ defaultEvent "Init"
  toEvent (Receive _) = Just $ defaultEvent "Receive"
  toEvent (HandleKey _ _) = Just $ defaultEvent "HandleKey"
  toEvent (HaskellAction action) = toEvent action
  toEvent (SimulationAction action) = toEvent action
  toEvent (BlocklyEditorAction action) = toEvent action
  toEvent (JavascriptAction action) = toEvent action
  toEvent (MarloweEditorAction action) = toEvent action
  toEvent (ChangeView view) = Just $ (defaultEvent "View")
    { label = Just (show view) }
  toEvent (ShowBottomPanel _) = Just $ defaultEvent "ShowBottomPanel"
  -- toEvent (ProjectsAction action) = toEvent action
  toEvent (NewProjectAction action) = toEvent action
  toEvent (DemosAction action) = toEvent action
  toEvent (RenameAction action) = toEvent action
  toEvent (SaveAsAction action) = toEvent action
  toEvent (ConfirmUnsavedNavigationAction _ _) = Just $ defaultEvent
    "ConfirmUnsavedNavigation"
  toEvent CheckAuthStatus = Just $ defaultEvent "CheckAuthStatus"
  toEvent (GistAction _) = Just $ defaultEvent "GistAction"
  toEvent (OpenModal view) = Just $ (defaultEvent (show view))
    { category = Just "OpenModal" }
  toEvent (CloseModal _) = Just $ defaultEvent "CloseModal"
  toEvent (ModalBackdropClick _) = Just $ defaultEvent "ModalBackdropClick"
  toEvent (OpenLoginPopup _) = Just $ defaultEvent "OpenLoginPopup"
  toEvent Logout = Just $ defaultEvent "Logout"
  toEvent (LoadProject _) = Just $ defaultEvent "LoadProject"

-- FIXME: We should merge `*Editor` into `CodeEditor` and derive
-- a precise route based on the `Project` state.
data View
  = HomePage
  | MarloweEditor
  | HaskellEditor
  | JSEditor
  | Simulation
  | BlocklyEditor

derive instance eqView :: Eq View

derive instance genericView :: Generic View _

instance showView :: Show View where
  show = genericShow

type ChildSlots =
  ( haskellEditorSlot :: H.Slot Monaco.Query Monaco.Message Unit
  , jsEditorSlot :: H.Slot Monaco.Query Monaco.Message Unit
  , blocklySlot :: H.Slot Blockly.Query Blockly.Message Unit
  , simulationSlot :: H.Slot Simulation.Query Blockly.Message Unit
  , simulatorEditorSlot :: H.Slot Monaco.Query Monaco.Message Unit
  , marloweEditorPageSlot :: H.Slot Monaco.Query Monaco.Message Unit
  , metadata :: forall query. H.Slot query MetadataAction Unit
  , tooltipSlot :: forall query. H.Slot query Void ReferenceId
  , hintSlot :: forall query. H.Slot query Void String
  , saveProject :: forall query. H.Slot query Void Unit
  , openProject :: forall query. H.Slot query (Maybe Project) Unit
  , currencyInput :: CurrencyInput.Slot String
  , dateTimeInput :: DateTimeLocalInput.Slot String
  )

_haskellEditorSlot :: Proxy "haskellEditorSlot"
_haskellEditorSlot = Proxy

_jsEditorSlot :: Proxy "jsEditorSlot"
_jsEditorSlot = Proxy

_blocklySlot :: Proxy "blocklySlot"
_blocklySlot = Proxy

_saveProject :: Proxy "saveProject"
_saveProject = Proxy

_openProject :: Proxy "openProject"
_openProject = Proxy

_simulationSlot :: Proxy "simulationSlot"
_simulationSlot = Proxy

_simulatorEditorSlot :: Proxy "simulatorEditorSlot"
_simulatorEditorSlot = Proxy

_marloweEditorPageSlot :: Proxy "marloweEditorPageSlot"
_marloweEditorPageSlot = Proxy

_walletSlot :: Proxy "walletSlot"
_walletSlot = Proxy

_currencyInputSlot :: Proxy "currencyInput"
_currencyInputSlot = Proxy

_dateTimeInputSlot :: Proxy "dateTimeInput"
_dateTimeInputSlot = Proxy

-----------------------------------------------------------
type Input = { tzOffset :: Minutes, webpackBuildMode :: WebpackBuildMode }

-- We store `Input` data so we are able to reset the state on logout
type State =
  { input :: Input
  , overlayState :: OverlayState
  , view :: View
  , jsCompilationResult :: CompilationState
  , jsEditorKeybindings :: KeyBindings
  , activeJSDemo :: String
  , showBottomPanel :: Boolean
  -- TODO: rename to haskellEditorState
  , haskellState :: HE.State
  -- TODO: rename to javascriptEditorState
  , javascriptState :: JS.State
  , marloweEditorState :: ME.State
  , blocklyEditorState :: BE.State
  , simulationState :: Simulation.StateBase ()

  , project :: Maybe Project

  , newProject :: NewProject.State
  , rename :: Rename.State
  , saveAs :: SaveAs.State
  , authStatus :: AuthResponse
  , gistId :: Maybe GistId
  , createGistResult :: RemoteData String Project
  , loadGistResult :: Either String (WebData Gist)
  , projectName :: String
  , showModal :: Maybe ModalView
  , hasUnsavedChanges :: Boolean

  , featureFlags ::
      { fsProjectStorage :: Boolean
      , logout :: Boolean
      }
  }

_project :: Lens' State (Maybe Project)
_project = prop (Proxy :: _ "project")

_contractMetadata :: Traversal' State MetaData
_contractMetadata = _project <<< _Just <<< Project._metadata

_contractMetadata' :: Getter' State MetaData
_contractMetadata' = Getter.to (preview _contractMetadata) <<< non
  emptyContractMetadata

_input :: Lens' State Input
_input = prop (Proxy :: _ "input")

_view :: Lens' State View
_view = prop (Proxy :: _ "view")

_jsCompilationResult :: Lens' State CompilationState
_jsCompilationResult = prop (Proxy :: _ "jsCompilationResult")

_jsEditorKeybindings :: Lens' State KeyBindings
_jsEditorKeybindings = prop (Proxy :: _ "jsEditorKeybindings")

_activeJSDemo :: Lens' State String
_activeJSDemo = prop (Proxy :: _ "activeJSDemo")

_showBottomPanel :: Lens' State Boolean
_showBottomPanel = prop (Proxy :: _ "showBottomPanel")

_marloweEditorState :: Lens' State ME.State
_marloweEditorState = prop (Proxy :: _ "marloweEditorState")

_blocklyEditorState :: Lens' State BE.State
_blocklyEditorState = prop (Proxy :: _ "blocklyEditorState")

_haskellState :: Lens' State HE.State
_haskellState = prop (Proxy :: _ "haskellState")

_javascriptState :: Lens' State JS.State
_javascriptState = prop (Proxy :: _ "javascriptState")

_simulationState :: Lens' State Simulation.State
_simulationState = do
  let
    _simulationStateBase = prop (Proxy :: Proxy "simulationState")
    _projectNameProxy = Proxy :: Proxy "projectName"
    get_ s = do
      let
        r = view _simulationStateBase s
        n = view _projectName s
      Record.insert _projectNameProxy n r
    set_ s r =
      set _simulationStateBase (Record.delete _projectNameProxy r)
        <<< set _projectName (Record.get _projectNameProxy r)
        $ s
  lens get_ set_

_newProject :: Lens' State NewProject.State
_newProject = prop (Proxy :: _ "newProject")

_rename :: Lens' State Rename.State
_rename = prop (Proxy :: _ "rename")

_saveAs :: Lens' State SaveAs.State
_saveAs = prop (Proxy :: _ "saveAs")

_authStatus :: Lens' State AuthResponse
_authStatus = prop (Proxy :: _ "authStatus")

_gistId :: Lens' State (Maybe GistId)
_gistId = prop (Proxy :: _ "gistId")

_createGistResult :: Lens' State (RemoteData String Project)
_createGistResult = prop (Proxy :: _ "createGistResult")

_loadGistResult :: Lens' State (Either String (WebData Gist))
_loadGistResult = prop (Proxy :: _ "loadGistResult")

_projectName :: forall r. Lens' { projectName :: String | r } String
_projectName = prop (Proxy :: _ "projectName")

_showModal :: Lens' State (Maybe ModalView)
_showModal = prop (Proxy :: _ "showModal")

_hasUnsavedChanges :: Lens' State Boolean
_hasUnsavedChanges = prop (Proxy :: _ "hasUnsavedChanges")

_workflow :: Fold' (First Workflow) State Workflow
_workflow = _project <<< _Just <<< Project._workflow

-- editable
_timestamp
  :: forall s a
   . Lens' { timestamp :: a | s } a
_timestamp = prop (Proxy :: _ "timestamp")

_value :: forall s a. Lens' { value :: a | s } a
_value = prop (Proxy :: _ "value")

isActiveTab :: State -> View -> Array ClassName
isActiveTab state activeView = state ^. _view <<< (activeClass (eq activeView))

-- -----------------------------------------------------------
-- newtype Session = Session
--   { projectName :: String
--   , gistId :: Maybe GistId
--   , workflow :: Maybe Workflow
--   , contractMetadata :: MetaData
--   }
--
-- derive instance newtypeSession :: Newtype Session _
--
-- derive instance eqSession :: Eq Session
--
-- derive instance genericSession :: Generic Session _
--
-- instance encodeJsonSession :: EncodeJson Session where
--   encodeJson (Session { projectName, gistId, workflow, contractMetadata }) =
--     encodeJson
--       { projectName
--       , gistId: maybe jsonNull encodeJson gistId
--       , workflow: maybe jsonNull encodeJson workflow
--       , contractMetadata
--       }
--
-- instance decodeJsonSession :: DecodeJson Session where
--   decodeJson json = do
--     obj <- decodeJObject json
--     projectName <- obj .: "projectName"
--     gistId <- obj .:? "gistId"
--     workflow <- obj .:? "workflow"
--     contractMetadata <- obj .: "contractMetadata"
--     pure $ Session { projectName, gistId, workflow, contractMetadata }
--
-- stateToSession :: State -> Session
-- stateToSession
--   { projectName
--   , gistId
--   , workflow
--   , contractMetadata
--   } =
--   Session
--     { projectName
--     , gistId
--     , workflow
--     , contractMetadata
--     }
--
-- sessionToState :: Session -> State -> State
-- sessionToState (Session sessionData) defaultState =
--   defaultState
--     { projectName = sessionData.projectName
--     , gistId = sessionData.gistId
--     , workflow = sessionData.workflow
--     , contractMetadata = sessionData.contractMetadata
--     }
--
isAuthenticated :: State -> Boolean
isAuthenticated = Auth.possiblyAuthenticated <<< view _authStatus

-- | Used to reference backdrop element
-- | so we are able to filter out closing
-- | mouse event click.
modalBackdropLabel :: RefLabel
modalBackdropLabel = RefLabel "modal-backdrop"
