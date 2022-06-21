module Store.ProjectState where

import Data.Lens (Lens', set)
import Data.Lens.Record (prop)
import Marlowe.Project.Types (Project, SourceCode, _code, _projectName)
import Type.Prelude (Proxy(..))

type ProjectState =
  { project :: Project
  , modified :: Boolean
  }

_project :: forall a r. Lens' { project :: a | r } a
_project = prop (Proxy :: _ "project")

mkProjectState :: Project -> ProjectState
mkProjectState = { modified: false, project: _ }

data ProjectStateAction
  = OnProjectNameChanged String
  | OnProjectCodeChanged SourceCode
  | OnProjectSaved

reduce :: ProjectState -> ProjectStateAction -> ProjectState
reduce store = case _ of
  OnProjectNameChanged name -> store
    { project = set _projectName name store.project }
  OnProjectCodeChanged code -> store
    { project = set _code code store.project }
  OnProjectSaved -> store { modified = false }
