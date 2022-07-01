module Marlowe.Project.Archive where

import Prologue

import Contrib.Tarballjs (addTextFile)
import Contrib.Tarballjs as T
import Data.Array as A
import Data.Foldable (for_)
import Data.Map as M
import Data.Newtype (un)
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object as FO
import Project.Bundle as Bundle
import Project.Types
  ( Bundle
  , FileContent(FileContent)
  , FileName(FileName)
  , Files(Files)
  )
import Web.File.File as W

toArchive :: Bundle -> Effect T.TarWriter
toArchive bundle = do
  let
    files :: Array _
    files = M.toUnfoldable $ un Files $ Bundle.toFiles bundle
  tarWriter <- T.tarWriter

  for_ files \(FileName n /\ FileContent c) ->
    addTextFile tarWriter (T.FileName n) (T.FileContent c)

  pure tarWriter

fromArchive :: W.File -> Aff (Maybe Bundle)
fromArchive file = do
  let
    fileNamesArr = FO.values <<< FO.fromHomogeneous $ Bundle.fileNames
  tarReader <- liftEffect T.tarReader
  void $ T.readFile tarReader file
  files <- map (M.fromFoldable <<< A.catMaybes) $ for fileNamesArr
    \n@(FileName name) -> liftEffect $ do
      c <- T.getTextFile tarReader (T.FileName name)
      pure $ ((n /\ _) <<< FileContent) <$> c
  pure $ Bundle.fromFiles (Files files)
