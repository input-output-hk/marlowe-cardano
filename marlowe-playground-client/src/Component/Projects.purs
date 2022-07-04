module Component.Projects where

import Prelude

import Component.Projects.Open as Open
import Component.Projects.SaveAs as SaveAs
import Control.Alternative as Alternative
import Data.Maybe (Maybe(..))
import Halogen as H
import MainFrame.Types (Action)
import Project (Project(..))
import Type.Constraints (class MonadAffAjaxStore)

saveAs :: forall t6 t7 t9 m. MonadAffAjaxStore m => H.Component t6 t7 t9 m
saveAs = SaveAs.component

open
  :: forall t6 t7 t9 m
   . MonadAffAjaxStore m
  => H.Component t6 t7 (Maybe Action) m
open = Open.component

