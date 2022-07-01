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
import Project.Bundle as Bundle
import Project.Types (Bundle, FileContent(..), FileName(..), Files(..))

newtype Description = Description String

toNewGist :: Bundle -> Description -> NewGist
toNewGist projectState (Description description) =
  NewGist
    { _newGistDescription: description
    , _newGistPublic: true
    , _newGistFiles
    }
  where
  Files projectFiles = Bundle.toFiles projectState

  _newGistFiles =
    A.fromFoldable
      <<< foldMapWithIndex (\i -> L.singleton <<< fromFile i)
      $ projectFiles

  fromFile (FileName name) (FileContent content) = NewGistFile
    { _newGistFilename: name, _newGistFileContent: content }

fromGist :: Gist -> Maybe Bundle
fromGist gist = Bundle.fromFiles $ Files $ do
  M.fromFoldable
    <<< map (L.over _1 FileName)
    <<< (M.toUnfoldable :: _ -> Array _)
    <<< M.mapMaybe (map FileContent <<< view gistFileContent) $ files
  where
  files = gist ^. gistFiles
