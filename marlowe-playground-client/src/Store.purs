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
import Network.RemoteData (RemoteData(..))
import Session (AuthResponse)
import Store.ProjectState (ProjectState, ProjectStateAction, mkProjectState)
import Store.ProjectState as ProjectState
import Type.Prelude (Proxy(..))

newtype State = State
  { authResponse :: AuthResponse
  , projectState :: Maybe ProjectState
  }

derive instance Newtype State _

mkStore
  :: Maybe Project
  -> State
mkStore project = State
  { projectState: mkProjectState <$> project
  , authResponse: NotAsked
  }

_authResponse :: Lens' State AuthResponse
_authResponse = _Newtype <<< prop (Proxy :: _ "authResponse")

_projectState :: Lens' State (Maybe ProjectState)
_projectState = _Newtype <<< prop (Proxy :: _ "projectState")

data Action
  = ProjectStateAction ProjectStateAction
  | OnProjectLoaded Project
  | SetAuthResponse AuthResponse

reduce :: State -> Action -> State
reduce = flip case _ of
  ProjectStateAction projectStateAction ->
    over (_projectState <<< _Just) (flip ProjectState.reduce projectStateAction)
  OnProjectLoaded project ->
    set (_projectState <<< _Just) (mkProjectState project)
  SetAuthResponse resp -> set _authResponse resp
