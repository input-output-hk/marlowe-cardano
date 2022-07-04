module Project.Bundle where

import Prelude

import CallByName.Alt ((<|>))
import Control.Alternative as Alternative
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode.Aeson as E
import Data.Argonaut.Extra (encodeStringifyJson, parseDecodeJson)
import Data.Array as Array
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (hush)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.FoldableWithIndex (foldWithIndexM, foldrWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', has)
import Data.Lens (Lens', lens)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as M
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, un)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString
import Data.Tuple.Nested (type (/\), (/\))
import Debug (traceM)
import Foreign.Object as Object
import Gist (Gist)
import Gists.Extra (GistId)
import Marlowe.Extended (Contract) as Marlowe.Extended
import Marlowe.Extended (Contract) as Marlowe.Extended
import Marlowe.Extended.Metadata (MetaData) as Marlowe.Extended
import Marlowe.Extended.Metadata (MetaData) as Marlowe.Extended
import Network.RemoteData (RemoteData(..), _Loading)
import Project.Types
  ( Bundle(..)
  , FileContent(..)
  , FileName(..)
  , Files(..)
  , Language(..)
  , ProjectName
  , SourceCode
  )
import Project.Types
  ( Language(..)
  , Project(..)
  , ProjectName(..)
  , SourceCode(..)
  , StorageLocation(..)
  )
import Project.Types as Types
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))

fileNames
  :: { actus :: FileName
     , haskell :: FileName
     , javascript :: FileName
     , marlowe :: FileName
     , metadata :: FileName
     , playground :: FileName
     }
fileNames =
  { playground: FileName "playground.marlowe.json"
  , marlowe: FileName "playground.marlowe"
  , haskell: FileName "Main.hs"
  , javascript: FileName "playground.js"
  , actus: FileName "actus.xml"
  , metadata: FileName "metadata.json"
  }

toFiles :: Bundle -> Files
toFiles (Bundle bundle) = do
  let
    codeFileName = case bundle.language of
      Haskell -> fileNames.haskell
      Javascript -> fileNames.javascript
      Marlowe -> fileNames.marlowe

  Files $ Map.fromFoldable
    [ codeFileName /\ coerce bundle.code
    , fileNames.playground /\ FileContent
        (NonEmptyString.toString $ un ProjectName $ bundle.projectName)
    , fileNames.metadata /\ FileContent
        (encodeStringifyJson $ bundle.metadata)
    ]

fromFiles :: Files -> Maybe Bundle
fromFiles (Files m) = Bundle <$> do
  projectName <- ProjectName <$> NonEmptyString.fromString pn
  metadata <- do
    FileContent json <- M.lookup fileNames.metadata m
    hush $ parseDecodeJson json
  ( -- do
    --   code <- lookupContent fileNames.actus m
    --   pure $ ActusProject { projectName, metadata, code, contract, storage }
    do
      code <- lookupContent fileNames.haskell m
      pure { projectName, metadata, code, language: Haskell }
      <|>
        do
          code <- lookupContent fileNames.javascript m
          pure { projectName, metadata, code, language: Javascript }
      <|> do
        code <- lookupContent fileNames.marlowe m
        pure { projectName, metadata, code, language: Marlowe }
  )
  where
  lookupContent n = coerce <<< M.lookup n
  FileContent pn = fromMaybe (FileContent "Uknown")
    (M.lookup fileNames.playground m)

detectLanguage :: Array FileName -> Maybe Language
detectLanguage fns = do
  Alternative.guard $ fileNames.playground `Array.elem` fns ||
    fileNames.metadata `Array.elem` fns
  Alternative.guard (fileNames.marlowe `Array.elem` fns)
    $> Marlowe
      <|> Alternative.guard (fileNames.haskell `Array.elem` fns)
    $> Haskell
      <|> Alternative.guard (fileNames.javascript `Array.elem` fns)
    $> Javascript

data BundleVersion
  = PreBundle
  | Bundle_0_1_0

detectVersion :: Files -> Maybe BundleVersion
detectVersion (Files c) =
  fileNames.playground `Map.lookup` c
    $> PreBundle
      <|> fileNames.metadata `Map.lookup` c
    $> Bundle_0_1_0
