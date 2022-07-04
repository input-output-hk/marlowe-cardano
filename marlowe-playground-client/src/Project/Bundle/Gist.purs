module Project.Bundle.Gist where

import Prologue

import Data.Array as A
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Lens (_1, view, (^.))
import Data.Lens (over) as L
import Data.List (singleton) as L
import Data.Map as M
import Gist
  ( Gist
  , NewGist(NewGist)
  , NewGistFile(..)
  , gistFileContent
  , gistFiles
  )
import Gists.Extra (GistId)
import Marlowe (Api)
import Marlowe as Server
import Project.Bundle as Bundle
import Project.Types
  ( Bundle
  , FileContent(..)
  , FileName(..)
  , Files(..)
  , projectNameToString
  , unBundle
  )
import Servant.PureScript (class MonadAjax)
import Types (JsonAjaxError)

submit
  :: forall m. MonadAjax Api m => GistPayload -> m (Either JsonAjaxError Gist)
submit (GistPayload { gistId: maybeGistId, payload }) = case maybeGistId of
  Nothing -> Server.postApiGists payload
  Just gistId -> Server.postApiGistsByGistId payload gistId

-- `NewGist` is a really missleading type name.
-- It is just payload for new and already existing gist.
newtype GistPayload = GistPayload
  { payload :: NewGist
  , gistId :: Maybe GistId
  }

toGistPayload :: Bundle -> Maybe GistId -> GistPayload
toGistPayload projectBundle gistId = GistPayload
  { payload
  , gistId
  }
  where
  projectName = projectNameToString <<< _.projectName <<< unBundle $
    projectBundle

  Files projectFiles = Bundle.toFiles projectBundle

  _newGistFiles =
    A.fromFoldable
      <<< foldMapWithIndex (\i -> L.singleton <<< fromFile i)
      $ projectFiles

  fromFile (FileName name) (FileContent content) = NewGistFile
    { _newGistFilename: name, _newGistFileContent: content }

  payload = NewGist
    { _newGistDescription: projectName
    , _newGistPublic: true
    , _newGistFiles
    }

fromGist :: Gist -> Maybe Bundle
fromGist gist = Bundle.fromFiles $ Files $ do
  M.fromFoldable
    <<< map (L.over _1 FileName)
    <<< (M.toUnfoldable :: _ -> Array _)
    <<< M.mapMaybe (map FileContent <<< view gistFileContent) $ files
  where
  files = gist ^. gistFiles
