module Component.Projects.State where

import Prologue hiding (div)

import Component.Projects.Types (Action(..), State, _projects)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (decodeJson, fromString, printJsonDecodeError, stringify)
import Data.Array (sortBy)
import Data.Bifunctor (lmap, rmap)
import Data.DateTime (DateTime)
import Data.DateTime.ISO (ISO)
import Data.Either (hush)
import Data.Lens (assign)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.Ordering (invert)
import Effect.Aff.Class (class MonadAff)
import Gist (Gist(..))
import Halogen (HalogenM)
import MainFrame.Types (ChildSlots)
import Marlowe (Api, getApiGists)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Servant.PureScript (class MonadAjax, printAjaxError)

handleAction
  :: forall m
   . MonadAff m
  => MonadAjax Api m
  => Action
  -> HalogenM State Action ChildSlots Void m Unit
handleAction LoadProjects = do
  assign _projects Loading
  resp <- lift $ RemoteData.fromEither <$> getApiGists
  assign _projects
    $ rmap sortGists
    $ lmap (printAjaxError stringify printJsonDecodeError)
    $ resp

handleAction (LoadProject _ _) = pure unit

handleAction (Cancel) = pure unit

sortGists :: Array Gist -> Array Gist
sortGists = sortBy f
  where
  dt :: String -> DateTime
  dt s = fromMaybe bottom $ map unwrap $ hush
    (decodeJson $ fromString s :: _ _ ISO)

  f (Gist { _gistUpdatedAt: a }) (Gist { _gistUpdatedAt: b }) = invert $ compare
    (dt a)
    (dt b)
