module Component.NewProject.Types where

import Prologue

import Analytics (class IsEvent)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Project (Workflow)
import Type.Proxy (Proxy(..))

data Action
  = CreateProject Workflow
  | Cancel

instance isEventAction :: IsEvent Action where
  toEvent (CreateProject lang) = Just
    { category: Just "NewProject"
    , action: "CreateProject"
    , label: Just (show lang)
    , value: Nothing
    }
  toEvent Cancel = Just
    { category: Just "NewProject"
    , action: "Cancel"
    , label: Nothing
    , value: Nothing
    }

type State =
  { error :: Maybe String
  }

emptyState :: State
emptyState = { error: Nothing }

_error :: Lens' State (Maybe String)
_error = prop (Proxy :: _ "error")
