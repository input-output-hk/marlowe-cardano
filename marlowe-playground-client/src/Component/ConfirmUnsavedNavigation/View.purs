module Component.ConfirmUnsavedNavigation.View where

import Prologue hiding (div)

import Component.ConfirmUnsavedNavigation.Types as CN
import Data.Lens (_Just, (^?))
import Effect.Aff.Class (class MonadAff)
import Halogen.Classes (btn, btnSecondary, modalContent, spaceRight, uppercase)
import Halogen.HTML (ClassName(..), ComponentHTML, button, div, div_, p_, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes)
import MainFrame.Types (Action(..), ChildSlots, State, _project)
import Project (projectNameToString)
import Project as Project

render
  :: forall m
   . MonadAff m
  => Action
  -> State
  -> ComponentHTML Action ChildSlots m
render intendedAction state = do
  let
    maybeProjectName = state ^? _project <<< _Just <<< Project._projectName <<<
      _Just
  div [ classes [ modalContent, ClassName "confirm-unsaved-navigation" ] ]
    [ p_
        [ text
            "Clicking on the Marlowe logo will take you out of the editor and return you to the home page."
        ]
    , p_ [ text "Unsaved changes will be lost." ]
    , p_
        [ text $ case maybeProjectName of
            Just pn -> "Do you want to save changes to '"
              <> projectNameToString pn
              <> "'?"
            Nothing -> "Do you want to save changes to your new project?"
        ]
    , div [ classes [ ClassName "actions" ] ]
        [ div_
            [ button
                [ classes [ btn, btnSecondary, uppercase ]
                , onConfirm CN.Cancel
                ]
                [ text "Cancel" ]
            ]
        , div_
            [ button
                [ classes [ btn, btnSecondary, uppercase, spaceRight ]
                , onConfirm CN.DontSaveProject
                ]
                [ text "Don't Save" ]
            , button [ classes [ btn, uppercase ], onConfirm CN.SaveProject ]
                [ text "Save" ]
            ]
        ]
    ]
  where
  onConfirm msg = onClick $ const $ ConfirmUnsavedNavigationAction
    intendedAction
    msg
