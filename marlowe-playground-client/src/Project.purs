module Project
  ( module Project
  , module Types
  ) where

import Prelude

import Control.Alternative as Alternative
import Data.Lens (AGetter, Getter, Getter', Lens', lens, (^.))
import Data.Lens.Getter as Getter
import Data.Maybe (Maybe(..), fromMaybe)
import Gist (Gist, gistId)
import Marlowe.Extended.Metadata (MetaData) as Marlowe.Extended
import Marlowe.Extended.Metadata
  ( MetadataHintInfo
  , emptyContractMetadata
  , getHintsFromMetadata
  )
import Prim.Row as Row
import Project.Bundle.Gist as Bundle.Gist
import Project.Types
  ( Bundle(..)
  , BundleBase
  , FileContent(..)
  , FileName(..)
  , Files(..)
  , Language(..)
  , Project(..)
  , ProjectBase
  , ProjectName(..)
  , ProjectStorage(..)
  , SourceCode(..)
  , Workflow(..)
  , workflowLanguage
  ) as Types
import Project.Types
  ( Bundle(..)
  , Language(..)
  , Project(..)
  , ProjectName
  , ProjectStorage(..)
  , SourceCode
  , Workflow(..)
  )
import Record as Record
import Type.Proxy (Proxy(..))

getProjectName :: Project -> Maybe ProjectName
getProjectName = case _ of
  MarloweProject r -> r.projectName
  HaskellProject r -> r.projectName
  JavascriptProject r -> r.projectName

-- ActusProject r -> r.projectName

setProjectName :: Project -> Maybe ProjectName -> Project
setProjectName project projectName = case project of
  MarloweProject r -> MarloweProject r { projectName = projectName }
  HaskellProject r -> HaskellProject r { projectName = projectName }
  JavascriptProject r -> JavascriptProject r { projectName = projectName }

-- ActusProject r -> ActusProject r { projectName = projectName }

_projectName :: Lens' Project (Maybe ProjectName)
_projectName = lens getProjectName setProjectName

getCode :: Project -> SourceCode
getCode = case _ of
  MarloweProject r -> r.code
  HaskellProject r -> r.code
  JavascriptProject r -> r.code

-- ActusProject r -> r.code

setCode :: Project -> SourceCode -> Project
setCode project code = case project of
  MarloweProject r -> MarloweProject r { code = code }
  HaskellProject r -> HaskellProject r { code = code }
  JavascriptProject r -> JavascriptProject r { code = code }

-- ActusProject r -> ActusProject r { code = code }

_code :: Lens' Project SourceCode
_code = lens getCode setCode

getMetadata :: Project -> Marlowe.Extended.MetaData
getMetadata = case _ of
  MarloweProject r -> r.metadata
  HaskellProject r -> r.metadata
  JavascriptProject r -> r.metadata

-- ActusProject r -> r.metadata

setMetadata :: Project -> Marlowe.Extended.MetaData -> Project
setMetadata project metadata = case project of
  MarloweProject r -> MarloweProject r { metadata = metadata }
  HaskellProject r -> HaskellProject r { metadata = metadata }
  JavascriptProject r -> JavascriptProject r { metadata = metadata }

-- ActusProject r -> ActusProject r { metadata = metadata }

_metadata :: Lens' Project Marlowe.Extended.MetaData
_metadata = lens getMetadata setMetadata

getMetadataHints :: Project -> MetadataHintInfo
getMetadataHints = case _ of
  MarloweProject r -> r.metadataHints
  HaskellProject r -> r.metadataHints
  JavascriptProject r -> r.metadataHints

-- ActusProject r -> r.metadata

setMetadataHints :: Project -> MetadataHintInfo -> Project
setMetadataHints project metadataHints = case project of
  MarloweProject r -> MarloweProject r { metadataHints = metadataHints }
  HaskellProject r -> HaskellProject r { metadataHints = metadataHints }
  JavascriptProject r -> JavascriptProject r { metadataHints = metadataHints }

_metadataHints :: Lens' Project MetadataHintInfo
_metadataHints = lens getMetadataHints setMetadataHints

_languageP = (Proxy :: Proxy "language")

_metadataHintsP = (Proxy :: Proxy "metadataHints")

_projectNameP = (Proxy :: Proxy "projectName")

fromBundle :: Maybe ProjectStorage -> Bundle -> Project
fromBundle storage (Bundle r) = case r.language of
  Haskell -> HaskellProject $ Record.merge r'
    { contract, metadataHints, storage }
  Javascript -> JavascriptProject $ Record.merge r'
    { contract, metadataHints, storage }
  Marlowe -> MarloweProject $ Record.merge r'
    { contract, metadataHints, storage, blocklyWorkflow: false }
  where
  contract = Nothing
  metadataHints = getHintsFromMetadata r.metadata
  r' =
    Record.set _projectNameP (Just r.projectName)
      <<< Record.delete _languageP
      $ r

_storageP = (Proxy :: Proxy "storage")
_contractP = (Proxy :: Proxy "contract")
_blocklyWorkflowP = (Proxy :: Proxy "blocklyWorkflow")

toBundle :: Project -> Maybe Bundle
toBundle project = do
  projectName <- project ^. _projectName
  let
    manageCommonFields
      :: forall t92 t105 t110
       . Row.Lacks "language" t92
      => Row.Lacks "storage" t92
      => Row.Lacks "contract" t92
      => Row.Lacks "metadataHints" t92
      => Language
      -> { contract :: t110
         , storage :: t105
         , metadataHints :: MetadataHintInfo
         , projectName :: Maybe ProjectName
         | t92
         }
      -> { language :: Language
         , projectName :: ProjectName
         | t92
         }
    manageCommonFields l =
      Record.insert _languageP l
        <<< Record.delete _storageP
        <<< Record.delete _contractP
        <<< Record.delete _metadataHintsP
        <<< Record.set _projectNameP projectName

  pure $ Bundle $ case project of
    HaskellProject r -> manageCommonFields Haskell r
    JavascriptProject r -> manageCommonFields Javascript r
    MarloweProject r ->
      manageCommonFields Marlowe
        <<< Record.delete _blocklyWorkflowP
        $ r

fromGist :: Gist -> Maybe Project
fromGist gist = do
  let
    storage = GistProject $ gist ^. gistId
  bundle <- Bundle.Gist.fromGist gist
  pure $ fromBundle (Just storage) bundle

getWorkflow :: Project -> Workflow
getWorkflow (MarloweProject { blocklyWorkflow: true }) = BlocklyWorkflow
getWorkflow (MarloweProject _) = MarloweWorkflow
getWorkflow (HaskellProject _) = HaskellWorkflow
getWorkflow (JavascriptProject _) = JavascriptWorkflow

_workflow :: Getter' Project Workflow
_workflow = Getter.to getWorkflow

setWorkflow :: Project -> Workflow -> Maybe Project
setWorkflow (MarloweProject r) BlocklyWorkflow = Just $ MarloweProject r
  { blocklyWorkflow = true }
setWorkflow (MarloweProject r) MarloweWorkflow = Just $ MarloweProject r
  { blocklyWorkflow = false }
setWorkflow project workflow =
  Alternative.guard (workflow == getWorkflow project) $> project

-- | We can only change workflow in the case of `MarloweProject` currently
-- | so in general this operation does nothing.
trySetWorkflow :: Project -> Workflow -> Project
trySetWorkflow project = fromMaybe project <<< setWorkflow project

fromSourceCode :: SourceCode -> Language -> Project
fromSourceCode code = case _ of
  Marlowe -> MarloweProject $ Record.insert _blocklyWorkflowP false base
  Haskell -> HaskellProject base
  Javascript -> JavascriptProject base
  where
  base =
    { code
    , contract: Nothing
    , metadataHints: mempty
    , metadata: emptyContractMetadata
    , storage: Nothing
    , projectName: Nothing
    }
