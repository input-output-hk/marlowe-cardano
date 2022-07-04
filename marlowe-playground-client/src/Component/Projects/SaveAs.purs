module Component.Projects.SaveAs where

import Prelude

import Data.Lens (_Just, preview, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.NonEmpty as NonEmptyString
import Data.Tuple.Nested ((/\))
import Halogen (Component)
import Halogen.Classes (border, borderBlue300, fullWidth, spaceBottom, textSm)
import Halogen.HTML (button, div_, input, text) as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Events as HH
import Halogen.HTML.Properties (classes, disabled, placeholder, value) as HH
import Halogen.Hooks (component) as H
import Halogen.Hooks.Extra.Hooks (usePutState)
import Halogen.Hooks.Hook (bind, pure) as H
import Halogen.Store.Connect as HS
import Halogen.Store.Monad (updateStore)
import Halogen.Store.Select (selectEq)
import Halogen.Store.UseSelector (useSelector)
import Project
  ( ProjectName(..)
  , StorageLocation(..)
  , _projectName
  , projectNameFromString
  , projectNameToString
  )
import Project as Project
import Store (_State)
import Store.AuthState.Hooks (useIsAuthenticated)
import Store.Handlers (loginRequired)
import Store.ProjectState (ProjectStateAction(..), _project)
import Store.ProjectState (_projectState)
import Store.ProjectState as Store.ProjectState
import Type.Constraints (class MonadAffAjaxStore)

data Step
  = ChooseStorage
  | Save Project.StorageLocation

-- updateStore ( Store.ProjectState.action $ OnProjectNameChanged $ projectName)

selector =
  ( selectEq $ preview
      $ _State
          <<< _projectState
          <<< _Just
          <<< _project
          <<< _projectName
          <<< _Just
  )

component
  :: forall t6 t7 t9 m
   . MonadAffAjaxStore m
  => Component t6 t7 t9 m
component = HS.connect selector $ H.component \_ { context: projectName } ->
  H.do
    step /\ putStep <- usePutState ChooseStorage

    authenticated <- fromMaybe false <$> useIsAuthenticated

    projectNameValue /\ putProjectNameValue <- usePutState $ fromMaybe "" $
      projectNameToString <$> projectName

    let
      newProjectName = projectNameFromString projectNameValue

    let
      renderStorageChoice _ = HH.div_
        [ HH.div_ [ HH.text $ show authenticated ]
        , HH.input
            [ HH.classes
                [ spaceBottom, fullWidth, textSm, border, borderBlue300 ]
            , HH.value $ projectNameValue
            , HH.onValueInput $ putProjectNameValue
            , HH.placeholder "Type a name for your project"
            ]
        , HH.button
            [ HH.disabled $ newProjectName == Nothing
            , HE.onClick $ const $ putStep (Save LocalFileSystem)
            ]
            [ HH.text "FS" ]
        , HH.button
            [ HH.disabled $ newProjectName == Nothing
            , HE.onClick $ const $ loginRequired
                (putStep ChooseStorage)
                (const $ putStep (Save $ GistPlatform Nothing))
            ]
            [ HH.text "GistPlatform" ]
        ]
    H.pure $ case step of
      ChooseStorage -> renderStorageChoice unit
      Save LocalFileSystem -> HH.text $ "Saving to local file system..."
      Save (GistPlatform _) ->
        if authenticated then HH.text $ "Saving to github"
        else renderStorageChoice unit

-- | FIXME: paluh
-- | Migrate to this template
-- render :: forall m. MonadAff m => State -> ComponentHTML Action ChildSlots m
-- render state =
--   div [ classes if isFailure' then [ ClassName "modal-error" ] else [] ]
--     [ div [ classes [ spaceTop, spaceLeft ] ]
--         [ h2 [ classes [ textBase, fontSemibold, noMargins ] ]
--             [ text "Save as" ]
--         ]
--     , div [ classes [ modalContent, ClassName "save-as-modal" ] ]
--         [ input
--             [ classes [ spaceBottom, fullWidth, textSm, border, borderBlue300 ]
--             , value (state ^. _projectName)
--             , onValueInput ChangeInput
--             , placeholder "Type a name for your project"
--             ]
--         , div [ classes [ textRight ] ]
--             [ button
--                 [ classes [ btn, btnSecondary, uppercase, spaceRight ]
--                 , onClick $ const Cancel
--                 ]
--                 [ text "Cancel" ]
--             , button
--                 [ classes [ btn, uppercase ]
--                 , disabled $ isEmpty || isLoading'
--                 , onClick $ const SaveProject
--                 ]
--                 if isLoading' then [ icon Spinner ] else [ text "Save" ]
--             ]
--         , renderError (state ^. _status)
--         ]
--     ]
--   where
--   isLoading' = isLoading $ (state ^. _status)
--
--   isFailure' = isFailure $ (state ^. _status)
--
--   renderError = case _ of
--     (Failure err) -> div [ class_ (ClassName "error") ] [ text err ]
--     _ -> text ""
--
--   isEmpty = state ^. _projectName == ""
