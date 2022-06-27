module Component.Modal.View
  ( modal
  ) where

import Prologue hiding (div)

import Component.ConfirmUnsavedNavigation.View (render) as ConfirmUnsavedNavigation
import Component.Demos.View (render) as Demos
import Component.NewProject.View (render) as NewProject
import Component.Projects as Projects
import Component.Projects.View (render) as Projects
import Data.Lens ((^.))
import Debug (traceM)
import Effect.Aff.Class (class MonadAff)
import GistButtons (authButton)
import Halogen (ComponentHTML, RefLabel(..))
import Halogen.Extra (renderSubmodule)
import Halogen.HTML (ClassName(ClassName), div, text)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes, ref)
import Halogen.Query.Input (Input(..))
import Halogen.Store.Monad (class MonadStore)
import MainFrame.Types
  ( Action(..)
  , ChildSlots
  , ModalView(..)
  , State
  , _newProject
  , _openProject
  , _projects
  , _rename
  , _saveAs
  , _saveProject
  , _showModal
  , hasGlobalLoading
  )
import MainFrame.Types as Types
import Rename.State (render) as Rename
import SaveAs.State (render) as SaveAs
import Store as Store
import Type.Constraints (class MonadAffAjaxStore)
import Web.Event.Event as Event
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.MouseEvent as MouseEvent

modal
  :: forall m
   . MonadAffAjaxStore m
  => State
  -> ComponentHTML Action ChildSlots m
modal state = case state ^. _showModal of
  Nothing -> text ""
  Just view ->
    div
      [ classes overlayClass
      , onClick ModalBackdropClick
      , ref Types.modalBackdropLabel
      ]
      [ div [ classes [ ClassName "modal" ] ]
          [ modalContent view ]
      ]
  where
  overlayClass =
    if hasGlobalLoading state then
      [ ClassName "overlay" ]
    else
      [ ClassName "overlay", ClassName "overlay-background" ]

  modalContent = case _ of
    NewProject -> renderSubmodule _newProject NewProjectAction NewProject.render
      state
    OpenProject -> HH.slot_ _openProject unit Projects.open unit
    -- else if isAuthenticated state then
    --   OpenModal OpenProject
    -- else
    --   OpenModal $ GithubLogin (OpenModal OpenProject)
    -- renderSubmodule _projects ProjectsAction Projects.render
    -- state
    OpenDemo -> renderSubmodule identity DemosAction (const Demos.render) state
    RenameProject -> renderSubmodule _rename RenameAction Rename.render state
    SaveProjectAs -> do
      if state.featureFlags.fsProjectStorage then
        HH.slot_ _saveProject unit Projects.save state.projectName
      else
        renderSubmodule _saveAs SaveAsAction SaveAs.render state

    (ConfirmUnsavedNavigation intendedAction) -> ConfirmUnsavedNavigation.render
      intendedAction
      state
    (GithubLogin intendedAction) -> authButton intendedAction state
