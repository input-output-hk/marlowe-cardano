module Marlowe.Project.Gist where

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
import Marlowe.Project.Types
  ( FileContent(..)
  , FileName(..)
  , Files(..)
  , Project
  , fromFiles
  , toFiles
  )

newtype Description = Description String

toNewGist :: Project -> Description -> NewGist
toNewGist projectState (Description description) =
  NewGist
    { _newGistDescription: description
    , _newGistPublic: true
    , _newGistFiles
    }
  where
  Files projectFiles = toFiles projectState

  _newGistFiles =
    A.fromFoldable
      <<< foldMapWithIndex (\i -> L.singleton <<< fromFile i)
      $ projectFiles

  fromFile (FileName name) (FileContent content) = NewGistFile
    { _newGistFilename: name, _newGistFileContent: content }

fromGist :: Gist -> (Maybe Project)
fromGist gist = fromFiles $ Files $ do
  let
    files = gist ^. gistFiles

  M.fromFoldable
    <<< map (L.over _1 FileName)
    <<< (M.toUnfoldable :: _ -> Array _)
    <<< M.mapMaybe (map FileContent <<< view gistFileContent) $ files
