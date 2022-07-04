module Component.Projects.Types where

import Prologue

import Analytics (class IsEvent, Event)
import Control.Alternative as Alternative
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode.Aeson as E
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', Prism, has, prism, prism')
import Data.Lens.Record (prop)
import Data.Show.Generic (genericShow)
import Gist (Gist)
import Gists.Extra (GistId)
import Network.RemoteData (RemoteData(..), _Loading)
import Project (Project(..))
import Type.Proxy (Proxy(..))

-- data Action
--   = LoadProjects
--   | LoadProject CodeEditor GistId
--   | Cancel
--
-- defaultEvent :: String -> Event
-- defaultEvent action =
--   { category: Just "Projects", action, label: Nothing, value: Nothing }
--
-- instance isEventAction :: IsEvent Action where
--   toEvent LoadProjects = Just $ defaultEvent "LoadProjects"
--   toEvent (LoadProject lang _) = Just
--     { category: Just "Projects"
--     , action: "LoadProject"
--     , label: Just (show lang)
--     , value: Nothing
--     }
--   toEvent Cancel = Just $ defaultEvent "Cancel"
--
-- type State =
--   { projects :: RemoteData String (Array Gist)
--   }
--
-- emptyState :: State
-- emptyState = { projects: NotAsked }
--
-- _projects :: Lens' State (RemoteData String (Array Gist))
-- _projects = prop (Proxy :: _ "projects")
--
-- modalIsLoading :: State -> Boolean
-- modalIsLoading = has (_projects <<< _Loading)

