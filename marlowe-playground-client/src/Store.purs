module Store where

import Prelude

import Data.Maybe (Maybe(..))
import Marlowe.Project.Types (Project)
import Store.ProjectStore (ProjectStore, ProjectStoreAction, mkProjectStore)
import Store.ProjectStore as ProjectStore

type Store =
  { projectStore :: Maybe ProjectStore
  }

mkStore
  :: Maybe Project
  -> Store
mkStore project = { projectStore: mkProjectStore <$> project }

data Action
  = ProjectStoreAction ProjectStoreAction
  | OnProjectLoaded Project

reduce :: Store -> Action -> Store
reduce store = case _ of
  ProjectStoreAction projectStoreAction ->
    store
      { projectStore = ProjectStore.reduce
          <$> store.projectStore
          <@> projectStoreAction
      }
  OnProjectLoaded project ->
    store { projectStore = Just $ ProjectStore.mkProjectStore project }
