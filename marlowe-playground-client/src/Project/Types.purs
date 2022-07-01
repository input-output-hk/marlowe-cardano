module Project.Types where

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

newtype ProjectName = ProjectName String

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

data ProjectStorage
  = LocalFile
  | GistProject GistId

derive instance Eq ProjectStorage
derive instance Ord ProjectStorage

type BundleBase r =
  { code :: SourceCode
  , metadata :: Marlowe.Extended.MetaData
  | r
  }

-- | `Bundle` is UI agnostic and represents a set of data which we save
-- | to preserve playground project.
newtype Bundle = Bundle
  (BundleBase (language :: Language, projectName :: ProjectName))

derive instance Eq Bundle
derive instance Generic Bundle _
instance DecodeJson Bundle where
  decodeJson = genericDecodeJson

instance EncodeJson Bundle where
  encodeJson = genericEncodeJson

type ProjectBase r = BundleBase
  ( storage :: Maybe ProjectStorage
  , contract :: Maybe Marlowe.Extended.Contract
  , metadataHints :: MetadataHintInfo
  , projectName :: Maybe ProjectName
  | r
  )

-- | `Project` is used to keep the current project state in the
-- | playground.
data Project
  = MarloweProject (ProjectBase (blocklyWorkflow :: Boolean))
  | HaskellProject (ProjectBase ())
  | JavascriptProject (ProjectBase ())

-- `OSet` is missing Eq instance so we are not able to provide one here.
-- derive instance Eq Project

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
