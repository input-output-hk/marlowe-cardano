module Store.ProjectState where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Prim.Row as Row
import Project (Project, ProjectName, SourceCode)
import Project as Project
import Record as Record
import Type.Prelude (Proxy(..))

_projectStateP = Proxy :: Proxy "projectState"
_projectP = Proxy :: Proxy "project"
_modifiedP = Proxy :: Proxy "modified"
_versionP = Proxy :: Proxy "version"

_projectState :: forall r. Lens' (State r) (Maybe ProjectState)
_projectState = prop _projectStateP

_project :: Lens' ProjectState Project
_project = prop _projectP

-- | `version` is our internal piece 
-- | to do quick comparison and estimate
-- | efficacy.
type ProjectState =
  { project :: Project
  , modified :: Boolean
  , version :: Int
  }

data ProjectStateAction
  = OnProjectNameChanged ProjectName
  | OnProjectCodeChanged SourceCode
  | OnProjectLoaded Project
  | OnProjectSaved
  | OnProjectModified

type StateRow r = (projectState :: Maybe ProjectState | r)
type State r = { | StateRow r }

type ActionRow r = (projectState :: ProjectStateAction | r)
type Action r = Variant (ActionRow r)

mkProjectState :: Project -> ProjectState
mkProjectState = { modified: false, project: _, version: 1 }

insertInitialProjectState
  :: forall r
   . Row.Lacks "projectState" r
  => Maybe Project
  -> { | r }
  -> { | StateRow r }
insertInitialProjectState project = Record.insert _projectStateP
  (mkProjectState <$> project)

action :: forall acc. ProjectStateAction -> Action acc
action = Variant.inj _projectStateP

reduce
  :: forall acc st
   . (Variant acc -> State st -> State st)
  -> Action acc
  -> State st
  -> State st
reduce = Variant.on _projectStateP case _ of
  OnProjectNameChanged n -> modify $ Record.modify _projectP $ flip
    Project.setProjectName
    (Just n)
  OnProjectCodeChanged code -> modify $ Record.modify _projectP $ flip
    Project.setCode
    code
  OnProjectLoaded project -> Record.set _projectStateP
    (Just $ mkProjectState project)
  OnProjectSaved -> modify $ Record.set _modifiedP false
  OnProjectModified -> modify $ Record.set _modifiedP true
  where
  modify :: (ProjectState -> ProjectState) -> State st -> State st
  modify update = Record.modify _projectStateP $ map
    (Record.modify _versionP (_ + 1) <<< update)

