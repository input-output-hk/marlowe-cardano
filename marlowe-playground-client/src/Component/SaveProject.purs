module Component.SaveProject where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Lens (_Just, view)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Halogen (Component)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks (component) as H
import Halogen.Hooks.Extra.Hooks (usePutState)
import Halogen.Hooks.Hook (bind, pure) as H
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Halogen.Store.UseSelector (useSelector)
import Marlowe.Project.Types (_projectName)
import Store (_projectState)
import Store as Store
import Store.ProjectState (_project)

newtype ProjectName = ProjectName String

data Storage = FS | GistPlatform

derive instance Generic Storage _
instance Show Storage where
  show = genericShow

data Step
  = ChooseStorage
  | Save Storage

data Action = SetStorage Storage

component
  :: forall t6 t7 t9 t10 t26
   . MonadStore t26 Store.State t10
  => Component t6 t7 t9 t10
component = H.component \_ _ -> H.do
  step /\ putStep <- usePutState ChooseStorage

  projectName <- useSelector
    (selectEq $ view $ _projectState <<< _Just <<< _project <<< _projectName)

  H.pure $ case step of
    ChooseStorage -> HH.div_
      [ HH.button
          [ HE.onClick $ const $ do
              traceM projectName
              putStep (Save FS)
          ]
          [ HH.text "FS" ]
      , HH.button [ HE.onClick $ const $ putStep (Save GistPlatform) ]
          [ HH.text "GistPlatform" ]
      ]
    Save storage -> HH.text $ "Save to " <> show storage

