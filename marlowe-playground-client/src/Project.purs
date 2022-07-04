module Project
  ( module Project
  , module Types
  ) where

import Prelude

import Data.Lens (Getter', Lens', lens, (^.))
import Data.Lens.Getter as Getter
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype as Newtype
import Gist (Gist, gistId)
import Marlowe.Extended.Metadata (MetaData) as Marlowe.Extended
import Marlowe.Extended.Metadata
  ( MetadataHintInfo
  , emptyContractMetadata
  , getHintsFromMetadata
  )
import Project.Bundle.Gist as Bundle.Gist
import Project.Types
  ( Bundle(..)
  , FileContent(..)
  , FileName(..)
  , Files(..)
  , Language(..)
  , Project
  , ProjectBase
  , ProjectName(..)
  , SourceCode(..)
  , StorageLocation(..)
  , Workflow(..)
  , projectNameFromString
  , projectNameToString
  , unBundle
  , unProject
  , workflowLanguage
  ) as Types
import Project.Types
  ( Bundle(..)
  , Language(..)
  , Project
  , ProjectName
  , ProjectRecord
  , Snapshot(..)
  , SourceCode
  , StorageLocation(..)
  , Version(..)
  , Workflow(..)
  , unProject
  , unsafeProject
  , workflowLanguage
  )
import Record as Record
import Type.Proxy (Proxy(..))

getProjectName :: Project -> Maybe ProjectName
getProjectName = unProject >>> _.projectName

-- Modifies project and bumps version
unsafeModifyProject :: (ProjectRecord -> ProjectRecord) -> Project -> Project
unsafeModifyProject f = unProject >>> f >>> unsafeProject

bumpVersion :: Project -> Project
bumpVersion = unsafeModifyProject
  (\r -> r { version = Newtype.over Version (_ + 1) r.version })

_version :: Getter' Project Version
_version = Getter.to (unProject >>> _.version)

setProjectName :: Project -> Maybe ProjectName -> Project
setProjectName project projectName = project # unsafeModifyProject _
  { projectName = projectName }

_projectName :: Lens' Project (Maybe ProjectName)
_projectName = lens getProjectName setProjectName

getCode :: Project -> SourceCode
getCode = unProject >>> _.code

setCode :: Project -> SourceCode -> Project
setCode project code = project # unsafeModifyProject _ { code = code }

_code :: Lens' Project SourceCode
_code = lens getCode setCode

getLanguage :: Project -> Language
getLanguage = unProject >>> _.language

setLanguage :: Project -> Language -> Project
setLanguage project language = project # unsafeModifyProject _
  { language = language }

_language :: Lens' Project Language
_language = lens getLanguage setLanguage

getStorage :: Project -> Maybe StorageLocation
getStorage = unProject >>> _.storage

setStorage :: Project -> Maybe StorageLocation -> Project
setStorage project storage = project # unsafeModifyProject _
  { storage = storage }

_storage :: Lens' Project (Maybe StorageLocation)
_storage = lens getStorage setStorage

getMetadata :: Project -> Marlowe.Extended.MetaData
getMetadata = unProject >>> _.metadata

setMetadata :: Project -> Marlowe.Extended.MetaData -> Project
setMetadata project metadata = project # unsafeModifyProject _
  { metadata = metadata }

_metadata :: Lens' Project Marlowe.Extended.MetaData
_metadata = lens getMetadata setMetadata

getMetadataHints :: Project -> MetadataHintInfo
getMetadataHints = unProject >>> _.metadataHints

setMetadataHints :: Project -> MetadataHintInfo -> Project
setMetadataHints project metadataHints = project # unsafeModifyProject _
  { metadataHints = metadataHints }

_metadataHints :: Lens' Project MetadataHintInfo
_metadataHints = lens getMetadataHints setMetadataHints

_languageP = (Proxy :: Proxy "language")

_metadataHintsP = (Proxy :: Proxy "metadataHints")

_projectNameP = (Proxy :: Proxy "projectName")

fromBundle :: Maybe StorageLocation -> Bundle -> Project
fromBundle storage (Bundle r) =
  unsafeProject
    <<< Record.set _projectNameP (Just r.projectName)
    <<< Record.merge { contract, metadataHints, storage, workflow, version }
    $ r
  where
  contract = Nothing
  metadataHints = getHintsFromMetadata r.metadata
  workflow = defaultWorkflow r.language
  -- Default workflow for a language
  version = Version 0

defaultWorkflow :: Language -> Workflow
defaultWorkflow = case _ of
  Haskell -> HaskellWorkflow
  Javascript -> JavascriptWorkflow
  Marlowe -> MarloweWorkflow

_storageP = (Proxy :: Proxy "storage")
_contractP = (Proxy :: Proxy "contract")
_blocklyWorkflowP = (Proxy :: Proxy "blocklyWorkflow")

toBundle :: Project -> Maybe Bundle
toBundle project = do
  let
    { code, language, metadata, projectName: maybeProjectName } = unProject
      project
  projectName <- maybeProjectName
  pure $ Bundle
    { code, language, metadata, projectName }

toSnapshot :: Project -> Snapshot
toSnapshot project = do
  let
    { code, language, metadata, projectName, version } = unProject project
  Snapshot
    { code, language, metadata, projectName, version }

fromSnapshot :: Snapshot -> Project
fromSnapshot (Snapshot r) =
  unsafeProject
    <<< Record.merge { contract, metadataHints, storage, workflow, version }
    $ r
  where
  contract = Nothing
  metadataHints = getHintsFromMetadata r.metadata
  workflow = defaultWorkflow r.language
  storage = Nothing
  version = r.version

fromGist :: Gist -> Maybe Project
fromGist gist = do
  let
    storage = GistPlatform $ Just $ gist ^. gistId
  bundle <- Bundle.Gist.fromGist gist
  pure $ fromBundle (Just storage) bundle

getWorkflow :: Project -> Workflow
getWorkflow = unProject >>> _.workflow

_workflow :: Getter' Project Workflow
_workflow = Getter.to getWorkflow

-- | Set workflow if possible. Usually noop. Have only sens
-- | in the context of Marlowe and MarloweWorkflow / BlocklyWorkflow.
setWorkflow :: Project -> Workflow -> Maybe Project
setWorkflow project workflow = do
  let
    r@{ language } = unProject project
  case workflow, language of
    BlocklyWorkflow, Marlowe -> Just $ unsafeProject $ r
      { workflow = BlocklyWorkflow }
    w, l | workflowLanguage w == l -> Just project
    _, _ -> Nothing

-- | We can only change workflow in the case of `MarloweProject` currently
-- | so in general this operation does nothing.
trySetWorkflow :: Project -> Workflow -> Project
trySetWorkflow project = fromMaybe project <<< setWorkflow project

fromSourceCode :: SourceCode -> Language -> Project
fromSourceCode code language = unsafeProject
  { code
  , contract: Nothing
  , language
  , metadataHints: mempty
  , metadata: emptyContractMetadata
  , storage: Nothing
  , projectName: Nothing
  , workflow: defaultWorkflow language
  , version: Version 0
  }

