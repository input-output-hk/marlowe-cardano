module Component.Projects.Open where

import Prelude

import Component.Projects.Index as Index
import Component.Projects.Types (OpenResult(..), Storage(..))
import Control.Monad.Trans.Class (lift)
import Data.Lens (_Just, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Newtype (un)
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Effect.Console (log)
import Halogen (Component)
import Halogen.Classes
  ( border
  , borderBlue300
  , btn
  , btnSecondary
  , fontSemibold
  , fullWidth
  , modalContent
  , noMargins
  , spaceBottom
  , spaceLeft
  , spaceRight
  , spaceTop
  , textBase
  , textRight
  , textSm
  , uppercase
  )
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HH
import Halogen.Hooks (component) as H
import Halogen.Hooks (raise)
import Halogen.Hooks.Extra.Hooks (usePutState)
import Halogen.Hooks.Hook (bind, pure) as H
import Halogen.Store.Select (selectEq)
import Halogen.Store.UseSelector (useSelector)
import Project (Project, ProjectName(..), _projectName)
import Safe.Coerce (coerce)
import Store.AuthState.Hooks (useIsAuthenticated)
import Store.Handlers (loginRequired)
import Store.ProjectState (_project)
import Type.Constraints (class MonadAffAjaxStore)
import Type.Prelude (Proxy(..))

_projectsIndex = Proxy :: Proxy "projectsIndex"

data Step
  = ChooseStorage
  | Open Storage

component
  :: forall t6 t7 m
   . MonadAffAjaxStore m
  => Component t6 t7 (Maybe Project) m
component = H.component \{ outputToken } _ -> H.do
  step /\ putStep <- usePutState ChooseStorage

  authenticated <- fromMaybe false <$> useIsAuthenticated

  let
    renderStorageChoice _ = HH.div_
      [ HH.div_ [ HH.text $ show authenticated ]
      , HH.button [ HE.onClick $ const $ putStep (Open FS) ] [ HH.text "FS" ]
      , HH.button
          [ HE.onClick $ const $ loginRequired
              (putStep ChooseStorage)
              (const $ putStep (Open GistPlatform))
          ]
          [ HH.text "GistPlatform" ]
      ]
  H.pure $ case step of
    ChooseStorage -> renderStorageChoice unit
    Open FS -> HH.text $ "Opening from local file system..."
    Open GistPlatform ->
      if authenticated then
        HH.slot _projectsIndex unit Index.component unit (raise outputToken)
      -- case _ of
      --   Canceled -> raise Canceled
      --   LoadProject gist -> raise LoadProject do
      --     -- gist <- ExceptT $ lift $ getApiGistsByGistId gistId
      --     traceM "load project"
      else
        renderStorageChoice unit
