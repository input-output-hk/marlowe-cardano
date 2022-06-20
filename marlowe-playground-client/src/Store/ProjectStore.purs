module Store.ProjectStore where

import Data.Lens (set)
import Marlowe.Project.Types (Project, SourceCode, _code, _projectName)

type ProjectStore =
  { project :: Project
  , modified :: Boolean
  }

mkProjectStore :: Project -> ProjectStore
mkProjectStore = { modified: false, project: _ }

data ProjectStoreAction
  = OnProjectNameChanged String
  | OnProjectCodeChanged SourceCode
  | OnProjectSaved

reduce :: ProjectStore -> ProjectStoreAction -> ProjectStore
reduce store = case _ of
  OnProjectNameChanged name -> store
    { project = set _projectName name store.project }
  OnProjectCodeChanged code -> store
    { project = set _code code store.project }
  OnProjectSaved -> store { modified = false }
