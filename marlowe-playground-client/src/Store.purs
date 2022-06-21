module Store
  ( module Store
  , module Store.ProjectState
  ) where

import Prelude

import Data.Lens (Lens', _Just, over, set)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Marlowe.Project.Types (Project)
import Store.ProjectState (ProjectState, ProjectStateAction, mkProjectState)
import Store.ProjectState as ProjectState
import Type.Prelude (Proxy(..))

newtype State = State
  { projectState :: Maybe ProjectState
  }

derive instance Newtype State _

mkStore
  :: Maybe Project
  -> State
mkStore project = State { projectState: mkProjectState <$> project }

_projectState :: Lens' State (Maybe ProjectState)
_projectState = _Newtype <<< prop (Proxy :: _ "projectState")

data Action
  = ProjectStateAction ProjectStateAction
  | OnProjectLoaded Project

reduce :: State -> Action -> State
reduce store = case _ of
  ProjectStateAction projectStateAction ->
    over (_projectState <<< _Just) (flip ProjectState.reduce projectStateAction)
      store
  OnProjectLoaded project ->
    set (_projectState <<< _Just) (mkProjectState project) store
