module MainFrame.View where

import Prologue hiding (div)

import Component.Modal.View (modal)
import Component.Projects as Projects
import Contrib.Data.Array.Builder ((:>))
import Contrib.Data.Array.Builder as AB
import Data.Foldable (foldMap)
import Data.Lens (_Just, has, preview, use, (^.), (^?))
import Data.Maybe (fromMaybe)
import Data.Monoid (guard)
import Data.Semigroup.Foldable (foldMap1)
import Debug (spy)
import Halogen (ComponentHTML)
import Halogen.Classes (marlowePlayLogo)
import Halogen.Css (classNames)
import Halogen.Extra (renderSubmodule)
import Halogen.HTML
  ( HTML
  , a
  , div
  , div_
  , footer
  , h1
  , header
  , img
  , main
  , section
  , span
  , text
  )
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (href, id, src, target)
import Halogen.HTML.Properties.ARIA (label, role)
import Home as Home
import Icons (Icon(..), icon)
import MainFrame.Types
  ( Action(..)
  , ChildSlots
  , ModalView(..)
  , State
  , View(..)
  , _authStatus
  , _blocklyEditorState
  , _contractMetadata
  , _contractMetadata'
  , _createGistResult
  , _hasUnsavedChanges
  , _haskellState
  , _javascriptState
  , _marloweEditorState
  , _project
  , _simulationState
  , _view
  , isAuthenticated
  )
import Marlowe.Extended.Metadata (emptyContractMetadata)
import Network.RemoteData (_Loading, _Success)
import Page.BlocklyEditor.View as BlocklyEditor
import Page.HaskellEditor.View (otherActions, render) as HaskellEditor
import Page.JavascriptEditor.View as JSEditor
import Page.MarloweEditor.View as MarloweEditor
import Page.Simulation.View as Simulation
import Project
  ( Project
  , ProjectName(..)
  , StorageLocation
  , _projectName
  , _storage
  , projectNameToString
  )
import Project as Project
import Type.Constraints (class MonadAffAjaxStore)

render
  :: forall m
   . MonadAffAjaxStore m
  => State
  -> ComponentHTML Action ChildSlots m
render state =
  div [ classNames [ "site-wrap" ] ]
    ( [ header [ classNames [ "no-margins", "flex", "flex-col" ] ]
          [ div
              [ classNames
                  [ "flex"
                  , "items-center"
                  , "justify-between"
                  , "bg-gray-dark"
                  , "px-medium"
                  , "py-small"
                  ]
              ]
              [ img
                  [ classNames [ "h-10", "cursor-pointer" ]
                  , onClick $ const $ ChangeView HomePage
                  , src marlowePlayLogo
                  ]
              , projectTitle
              , div_ $ AB.unsafeBuild $ do
                  let
                    tutorial = a
                      [ href "./doc/marlowe/tutorials/index.html"
                      , target "_blank"
                      , classNames [ "font-semibold" ]
                      ]
                      [ text "Tutorial" ]
                    logout = a
                      [ onClick $ const Logout
                      , classNames [ "font-semibold", "ml-4" ]
                      ]
                      [ text "Logout" ]
                  tutorial
                    :> guard
                      (isAuthenticated state && state.featureFlags.logout)
                      (AB.cons logout)

              -- Link disabled as the Actus labs is not working properly. Future plans might include moving this functionality to Marlowe run
              -- , a [ onClick $ const $ ChangeView ActusBlocklyEditor, classNames [ "ml-medium", "font-semibold" ] ] [ text "Actus Labs" ]
              ]
          , topBar
          ]
      , main []
          [ section [ id "main-panel" ] do
              let
                metadata = state ^. _contractMetadata'

              case state ^. _view of
                HomePage -> [ Home.render state ]
                Simulation ->
                  [ renderSubmodule
                      _simulationState
                      SimulationAction
                      (Simulation.render metadata)
                      state
                  ]
                MarloweEditor ->
                  [ renderSubmodule
                      _marloweEditorState
                      MarloweEditorAction
                      (MarloweEditor.render metadata)
                      state
                  ]
                HaskellEditor ->
                  [ renderSubmodule
                      _haskellState
                      HaskellAction
                      (HaskellEditor.render metadata)
                      state
                  ]
                JSEditor ->
                  [ renderSubmodule
                      _javascriptState
                      JavascriptAction
                      (JSEditor.render metadata)
                      state
                  ]
                BlocklyEditor ->
                  [ renderSubmodule
                      _blocklyEditorState
                      BlocklyEditorAction
                      (BlocklyEditor.render metadata)
                      state
                  ]
          ]
      , modal state
      , globalLoadingOverlay
      , footer
          [ classNames
              [ "flex"
              , "justify-between"
              , "px-medium"
              , "py-small"
              , "bg-gray-dark"
              , "font-semibold"
              ]
          ]
          [ div [ classNames [ "flex" ] ]
              [ a
                  [ href "https://cardano.org/"
                  , target "_blank"
                  , classNames [ "pr-small" ]
                  ]
                  [ text "cardano.org" ]
              , a
                  [ href "https://iohk.io/"
                  , target "_blank"
                  , classNames [ "pl-small" ]
                  ]
                  [ text "iohk.io" ]
              ]
          , div_ [ text (copyright <> " 2021 IOHK Ltd") ]
          , div [ classNames [ "flex" ] ]
              [ a
                  [ href "https://t.me/IOHK_Marlowe"
                  , target "_blank"
                  , classNames [ "pr-small" ]
                  ]
                  [ text "Telegram" ]
              , a
                  [ href "https://twitter.com/hashtag/Marlowe"
                  , target "_blank"
                  , classNames [ "pl-small" ]
                  ]
                  [ text "Twitter" ]
              ]
          ]
      ]
    )
  where
  copyright = "\x00A9"

  projectTitle = case state ^. _view of
    HomePage -> text ""
    _ ->
      let
        maybeProjectName = state ^? _project <<< _Just <<< Project._projectName
          <<< _Just

        unsavedChangesIndicator =
          if state ^. _hasUnsavedChanges then "*" else ""

        isLoading = has (_createGistResult <<< _Loading) state

        spinner =
          if isLoading then icon Spinner else div [ classNames [ "empty" ] ] []
      in
        div
          [ classNames [ "project-title" ]
          , role "heading"
          , label "project-title"
          ]
          [ h1 [ classNames [ "text-lg" ] ]
              {- TODO: Fix style when name is super long -}
              [ text $ fromMaybe "Untitled project" $ projectNameToString <$>
                  maybeProjectName
              , span [ classNames [ "unsave-change-indicator" ] ]
                  [ text unsavedChangesIndicator ]
              ]
          , spinner
          ]

  topBar =
    if showtopBar then
      div
        [ classNames [ "global-actions" ] ]
        ([ menuBar state ] <> otherActions (state ^. _view))
    else
      div_ []

  showtopBar = case state ^. _view of
    HaskellEditor -> true
    JSEditor -> true
    BlocklyEditor -> true
    Simulation -> true
    MarloweEditor -> true
    _ -> false

  otherActions HaskellEditor =
    [ renderSubmodule _haskellState HaskellAction HaskellEditor.otherActions
        state
    ]

  otherActions Simulation =
    [ renderSubmodule _simulationState SimulationAction
        (const Simulation.otherActions)
        state
    ]

  otherActions JSEditor =
    [ renderSubmodule _javascriptState JavascriptAction JSEditor.otherActions
        state
    ]

  otherActions MarloweEditor =
    [ renderSubmodule _marloweEditorState MarloweEditorAction
        MarloweEditor.otherActions
        state
    ]

  otherActions BlocklyEditor =
    [ renderSubmodule _blocklyEditorState BlocklyEditorAction
        BlocklyEditor.otherActions
        state
    ]

  otherActions _ = []

  globalLoadingOverlay = case state.overlayState of
    { overlayCounter } | overlayCounter > 0 ->
      div
        [ classNames
            [ "overlay"
            , "overlay-background"
            , "loading-overlay"
            , "text-3xl"
            , "font-semibold"
            , "text-white"
            ]
        ]
        [ div [ classNames [ "mb-small" ] ] [ text "" ]
        , div_ [ icon Spinner ]
        ]
    { backdropCounter } | backdropCounter > 0 -> do
      div [ classNames [ "overlay" ] ] []
    _ -> div_ []

-- | FIXME: paluh - handle redirect
-- saveActionForProject :: forall t2. Project -> t2 -> Action
-- saveActionForProject project _ = do
--   let
--     saveAsAction = OpenModal (ModalComponent Projects.saveAs)
--   fromMaybe saveAsAction $ do
--     storage <- project ^. _storage
--     projectBundle <- Project.toBundle project
--     pure $ SaveProject projectBundle storage

menuBar :: forall p. State -> HTML p Action
menuBar state = do
  div [ classNames [ "menu-bar" ] ] $
    [ menuButton (OpenModal NewProject) "New Project"
    , menuButton (OpenModal (ModalComponent Projects.open)) "Open"
    , menuButton (OpenModal OpenDemo) "Open Example"
    ]
      <> (state ^. _project) `flip foldMap` \_ -> do
        let
          saveAsAction = OpenModal (ModalComponent Projects.saveAs)
        [ menuButton (OpenModal RenameProject) "Rename"
        , menuButton SaveProject "Save"
        , menuButton saveAsAction "Save As..."
        ]
  where
  menuButton action name =
    a [ onClick $ const action ]
      [ span [] [ text name ]
      ]

