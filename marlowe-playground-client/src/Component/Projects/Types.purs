module Component.Projects.Types where

import Prologue
import Analytics (class IsEvent, Event)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (Lens', has)
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))
import Gist (Gist, GistId)
import Network.RemoteData (RemoteData(..), _Loading)
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode.Aeson as E

-----------------------------------------------------------
data Lang
  = Marlowe
  | Haskell
  | Javascript
  | Blockly

derive instance eqLang :: Eq Lang

derive instance ord :: Ord Lang

derive instance genericLang :: Generic Lang _

instance encodeJsonLang :: EncodeJson Lang where
  encodeJson = E.encode E.enum

instance decodeJsonLang :: DecodeJson Lang where
  decodeJson = D.decode D.enum

instance enumLang :: Enum Lang where
  succ = genericSucc
  pred = genericPred

instance boundedLang :: Bounded Lang where
  bottom = genericBottom
  top = genericTop

-----------------------------------------------------------
instance showLang :: Show Lang where
  show lang = genericShow lang

data Action
  = LoadProjects
  | LoadProject Lang GistId
  | Cancel

defaultEvent :: String -> Event
defaultEvent action =
  { category: Just "Projects", action, label: Nothing, value: Nothing }

instance isEventAction :: IsEvent Action where
  toEvent LoadProjects = Just $ defaultEvent "LoadProjects"
  toEvent (LoadProject lang _) = Just
    { category: Just "Projects"
    , action: "LoadProject"
    , label: Just (show lang)
    , value: Nothing
    }
  toEvent Cancel = Just $ defaultEvent "Cancel"

type State =
  { projects :: RemoteData String (Array Gist)
  }

emptyState :: State
emptyState = { projects: NotAsked }

_projects :: Lens' State (RemoteData String (Array Gist))
_projects = prop (Proxy :: _ "projects")

modalIsLoading :: State -> Boolean
modalIsLoading = has (_projects <<< _Loading)
