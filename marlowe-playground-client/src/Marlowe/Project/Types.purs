module Marlowe.Project.Types where

import Prelude

import CallByName.Alt ((<|>))
import Data.Argonaut.Extra (encodeStringifyJson, parseDecodeJson)
import Data.Either (hush)
import Data.Lens (Lens', lens)
import Data.Map (Map)
import Data.Map as M
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, un)
import Data.Tuple.Nested ((/\))
import Marlowe.Extended (Contract) as Marlowe.Extended
import Marlowe.Extended.Metadata (MetaData) as Marlowe.Extended
import Safe.Coerce (coerce)

newtype SourceCode = SourceCode String

derive instance Newtype SourceCode _

newtype ProjectName = ProjectName String

derive instance Newtype ProjectName _
derive newtype instance Eq ProjectName
derive newtype instance Ord ProjectName

type ProjectContent extra =
  { projectName :: ProjectName
  , code :: SourceCode
  , metadata :: Marlowe.Extended.MetaData
  | extra
  }

data Project
  = MarloweProject
      (ProjectContent (contract :: Maybe Marlowe.Extended.Contract))
  | HaskellProject (ProjectContent ())
  | JavascriptProject (ProjectContent ())
  | ActusProject (ProjectContent ())

getProjectName :: Project -> ProjectName
getProjectName = case _ of
  MarloweProject r -> r.projectName
  HaskellProject r -> r.projectName
  JavascriptProject r -> r.projectName
  ActusProject r -> r.projectName

setProjectName :: Project -> ProjectName -> Project
setProjectName project projectName = case project of
  MarloweProject r -> MarloweProject r { projectName = projectName }
  HaskellProject r -> HaskellProject r { projectName = projectName }
  JavascriptProject r -> JavascriptProject r { projectName = projectName }
  ActusProject r -> ActusProject r { projectName = projectName }

_projectName :: Lens' Project ProjectName
_projectName = lens getProjectName setProjectName

getCode :: Project -> SourceCode
getCode = case _ of
  MarloweProject r -> r.code
  HaskellProject r -> r.code
  JavascriptProject r -> r.code
  ActusProject r -> r.code

setCode :: Project -> SourceCode -> Project
setCode project code = case project of
  MarloweProject r -> MarloweProject r { code = code }
  HaskellProject r -> HaskellProject r { code = code }
  JavascriptProject r -> JavascriptProject r { code = code }
  ActusProject r -> ActusProject r { code = code }

_code :: Lens' Project SourceCode
_code = lens getCode setCode

getMetadata :: Project -> Marlowe.Extended.MetaData
getMetadata = case _ of
  MarloweProject r -> r.metadata
  HaskellProject r -> r.metadata
  JavascriptProject r -> r.metadata
  ActusProject r -> r.metadata

setMetadata :: Project -> Marlowe.Extended.MetaData -> Project
setMetadata project metadata = case project of
  MarloweProject r -> MarloweProject r { metadata = metadata }
  HaskellProject r -> HaskellProject r { metadata = metadata }
  JavascriptProject r -> JavascriptProject r { metadata = metadata }
  ActusProject r -> ActusProject r { metadata = metadata }

_metadata :: Lens' Project Marlowe.Extended.MetaData
_metadata = lens getMetadata setMetadata

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

toFiles :: Project -> Files
toFiles project = do
  let
    codeFileName /\ codeFileContent = case project of
      MarloweProject { code: SourceCode code } -> do
        let
          codeFileContent = FileContent $ encodeStringifyJson code
        fileNames.marlowe /\ codeFileContent
      HaskellProject { code } -> fileNames.haskell /\ coerce code
      JavascriptProject { code } -> fileNames.javascript /\ coerce code
      ActusProject { code } -> fileNames.actus /\ coerce code
  Files $ Map.fromFoldable
    [ codeFileName /\ codeFileContent
    , fileNames.playground /\ FileContent
        (un ProjectName $ getProjectName project)
    , fileNames.metadata /\ FileContent
        (encodeStringifyJson $ getMetadata project)
    ]

fromFiles :: Files -> Maybe Project
fromFiles (Files m) = do
  metadata <- do
    FileContent json <- M.lookup fileNames.metadata m
    hush $ parseDecodeJson json
  ( do
      code <- lookupContent fileNames.actus m
      pure $ ActusProject { projectName, metadata, code }
      <|> do
        code <- lookupContent fileNames.haskell m
        pure $ HaskellProject
          { projectName, metadata, code }
      <|>
        do
          code <- lookupContent fileNames.javascript m
          pure $ JavascriptProject
            { projectName, metadata, code }
      <|> do
        code <- lookupContent fileNames.marlowe m
        pure $ MarloweProject { projectName, metadata, code, contract: Nothing }
  )
  where
  lookupContent n = coerce <<< M.lookup n
  FileContent pn = fromMaybe (FileContent "Uknown")
    (M.lookup fileNames.playground m)
  projectName = ProjectName pn
