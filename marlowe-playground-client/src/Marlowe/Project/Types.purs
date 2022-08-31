module Marlowe.Project.Types where

import Prelude

import Data.Map (Map)
import Data.Map as M
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Safe.Coerce as Safe.Coerce

type ProjectBase f content =
  { playground :: content
  , marlowe :: f content
  , haskell :: f content
  , javascript :: f content
  , actus :: f content
  , metadata :: f content
  }

type Id :: forall k. k -> k
type Id a = a

newtype FileContent = FileContent String

derive instance Newtype FileContent _
derive newtype instance Eq FileContent
derive newtype instance Ord FileContent

newtype FileName = FileName String

derive instance Newtype FileName _
derive newtype instance Eq FileName
derive newtype instance Ord FileName

newtype Files = Files (Map FileName FileContent)

derive instance Newtype Files _
derive newtype instance Eq Files
derive newtype instance Ord Files

newtype ProjectState = ProjectState (ProjectBase Maybe String)

type FileNames = ProjectBase Id FileName

fileNames :: FileNames
fileNames =
  { playground: FileName "playground.marlowe.json"
  , marlowe: FileName "playground.marlowe"
  , haskell: FileName "Main.hs"
  , javascript: FileName "playground.js"
  , actus: FileName "actus.xml"
  , metadata: FileName "metadata.json"
  }

toFiles :: ProjectState -> Files
toFiles (ProjectState r) = do
  let
    insert' k (Just c) = Map.insert k (FileContent c)
    insert' _ _ = identity

    x =
      insert' fileNames.playground (Just r.playground)
        <<< insert' fileNames.actus r.actus
        <<< insert' fileNames.haskell r.haskell
        <<< insert' fileNames.javascript r.javascript
        <<< insert' fileNames.marlowe r.marlowe
        <<< insert' fileNames.metadata r.metadata
        $ M.empty
  Files x

fromFiles :: Files -> ProjectState
fromFiles (Files m) = do
  ProjectState $ Safe.Coerce.coerce
    { actus: M.lookup fileNames.actus m
    , haskell: M.lookup fileNames.haskell m
    , javascript: M.lookup fileNames.javascript m
    , marlowe: M.lookup fileNames.marlowe m
    , metadata: M.lookup fileNames.metadata m
    , playground: fromMaybe (FileContent "{}") (M.lookup fileNames.playground m)
    }
