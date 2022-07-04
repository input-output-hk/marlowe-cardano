module Project.Types
  ( Bundle(..)
  , BundleRecord(..)
  , FileContent(..)
  , FileName(..)
  , Files(..)
  , SourceCode(..)
  , ProjectName(..)
  , ProjectBase
  , Language(..)
  , Workflow(..)
  , Project
  , StorageLocation(..)
  , ProjectRecord
  , Version(..)
  , Snapshot(..)
  , projectNameToString
  , projectNameFromString
  , unsafeProject
  , unBundle
  , unProject
  , workflowLanguage
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Aeson as E
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString
import Gists.Extra (GistId)
import Marlowe.Extended (Contract) as Marlowe.Extended
import Marlowe.Extended.Metadata (MetaData) as Marlowe.Extended
import Marlowe.Extended.Metadata (MetadataHintInfo)

newtype SourceCode = SourceCode String

derive instance Eq SourceCode
derive instance Generic SourceCode _
derive instance Newtype SourceCode _
instance DecodeJson SourceCode where
  decodeJson = genericDecodeJson

instance EncodeJson SourceCode where
  encodeJson = genericEncodeJson

newtype ProjectName = ProjectName NonEmptyString

derive instance Newtype ProjectName _
derive instance Generic ProjectName _
derive newtype instance Eq ProjectName
derive newtype instance Ord ProjectName
instance DecodeJson ProjectName where
  decodeJson = genericDecodeJson

instance EncodeJson ProjectName where
  encodeJson = genericEncodeJson

data Language
  = Marlowe
  | Haskell
  | Javascript

derive instance Eq Language
derive instance Ord Language
derive instance Generic Language _
instance Show Language where
  show = genericShow

instance DecodeJson Language where
  decodeJson = genericDecodeJson

instance EncodeJson Language where
  encodeJson = genericEncodeJson

-- | We can have unknown `GistId`
-- | when we attempted to save to github
-- | but this action failed.
data StorageLocation
  = LocalFileSystem
  | GistPlatform (Maybe GistId)

derive instance Eq StorageLocation
derive instance Ord StorageLocation

type ProjectBase r =
  { code :: SourceCode
  , language :: Language
  , metadata :: Marlowe.Extended.MetaData
  | r
  }

type BundleRecord = ProjectBase (projectName :: ProjectName)

-- | `Bundle` is UI agnostic and represents a set of data which we save
-- | to preserve playground project.
newtype Bundle = Bundle BundleRecord

unBundle :: Bundle -> BundleRecord
unBundle (Bundle r) = r

derive instance Eq Bundle
derive instance Generic Bundle _
instance DecodeJson Bundle where
  decodeJson = genericDecodeJson

instance EncodeJson Bundle where
  encodeJson = genericEncodeJson

newtype Version = Version Int

derive instance Generic Version _
derive instance Newtype Version _
derive instance Eq Version
derive instance Ord Version

instance DecodeJson Version where
  decodeJson = genericDecodeJson

instance EncodeJson Version where
  encodeJson = genericEncodeJson

type ProjectRecord = ProjectBase
  ( storage :: Maybe StorageLocation
  , contract :: Maybe Marlowe.Extended.Contract
  , metadataHints :: MetadataHintInfo
  , projectName :: Maybe ProjectName
  , workflow :: Workflow
  , version :: Version
  )

-- | `Project` is used to keep the current project state in the app.
-- | Internal invariants are guarded through smart constructor
-- | but we expose `unsafeProject` constructor so it is easier
-- | to modularize internal representation
newtype Project = Project ProjectRecord

unProject :: Project -> ProjectRecord
unProject (Project r) = r

unsafeProject :: ProjectRecord -> Project
unsafeProject = Project

-- `OSet` is missing Eq instance so we are not able to provide one here.
-- derive instance Eq Project

-- | Used to do auto saving of the environment
-- | Version is copied so we can quickly detect
-- | if anything has changed.
newtype Snapshot = Snapshot
  (ProjectBase (projectName :: Maybe ProjectName, version :: Version))

derive instance Eq Snapshot
derive instance Generic Snapshot _
instance DecodeJson Snapshot where
  decodeJson = genericDecodeJson

instance EncodeJson Snapshot where
  encodeJson = genericEncodeJson

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

data Workflow
  = MarloweWorkflow
  | BlocklyWorkflow
  | HaskellWorkflow
  | JavascriptWorkflow

derive instance Eq Workflow

derive instance Ord Workflow

derive instance Generic Workflow _

instance EncodeJson Workflow where
  encodeJson = E.encode E.enum

instance DecodeJson Workflow where
  decodeJson = D.decode D.enum

instance Enum Workflow where
  succ = genericSucc
  pred = genericPred

instance Bounded Workflow where
  bottom = genericBottom
  top = genericTop

instance Show Workflow where
  show lang = genericShow lang

workflowLanguage :: Workflow -> Language
workflowLanguage HaskellWorkflow = Haskell
workflowLanguage BlocklyWorkflow = Marlowe
workflowLanguage JavascriptWorkflow = Javascript
workflowLanguage MarloweWorkflow = Marlowe

projectNameToString :: ProjectName -> String
projectNameToString (ProjectName n) = NonEmptyString.toString n

projectNameFromString :: String -> Maybe ProjectName
projectNameFromString = map ProjectName <<< NonEmptyString.fromString

