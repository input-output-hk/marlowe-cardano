module Component.Projects where

import Prelude

import Component.Projects.Open as Open
import Component.Projects.Save as Save
import Control.Alternative as Alternative
import Data.Maybe (Maybe(..))
import Halogen as H
import Project (Project(..))
import Type.Constraints (class MonadAffAjaxStore)

save :: forall t6 t7 t9 m. MonadAffAjaxStore m => H.Component t6 t7 t9 m
save = Save.component

open
  :: forall t6 t7 t9 m
   . MonadAffAjaxStore m
  => H.Component t6 t7 (Maybe Project) m
open = Open.component
