module Language.Marlowe.Extended.V1.Metadata.Lenses where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map.Ordered.OMap (OMap)
import Data.Set (Set)
import Data.Set.Ordered.OSet (OSet)
import Language.Marlowe.Core.V1.Semantics.Types as S
import Language.Marlowe.Extended.V1.Metadata.Types
  ( ChoiceInfo
  , ContractType
  , MetaData
  , MetadataHintInfo
  , NumberFormat
  , ValueParameterInfo
  )
import Type.Proxy (Proxy(..))

_valueParameterFormat :: Lens' ValueParameterInfo NumberFormat
_valueParameterFormat = prop (Proxy :: _ "valueParameterFormat")

_valueParameterDescription :: Lens' ValueParameterInfo String
_valueParameterDescription = prop (Proxy :: _ "valueParameterDescription")

_choiceFormat :: Lens' ChoiceInfo NumberFormat
_choiceFormat = prop (Proxy :: _ "choiceFormat")

_choiceDescription :: Lens' ChoiceInfo String
_choiceDescription = prop (Proxy :: _ "choiceDescription")

_contractName :: Lens' MetaData String
_contractName = prop (Proxy :: _ "contractName")

_contractType :: Lens' MetaData ContractType
_contractType = prop (Proxy :: _ "contractType")

_contractShortDescription :: Lens' MetaData String
_contractShortDescription = prop (Proxy :: _ "contractShortDescription")

_contractLongDescription :: Lens' MetaData String
_contractLongDescription = prop (Proxy :: _ "contractLongDescription")

_roleDescriptions :: Lens' MetaData (Map S.TokenName String)
_roleDescriptions = prop (Proxy :: _ "roleDescriptions")

_timeParameterDescriptions :: Lens' MetaData (OMap String String)
_timeParameterDescriptions = prop (Proxy :: _ "timeParameterDescriptions")

_valueParameterInfo :: Lens' MetaData (OMap String ValueParameterInfo)
_valueParameterInfo = prop (Proxy :: _ "valueParameterInfo")

_choiceInfo :: Lens' MetaData (Map String ChoiceInfo)
_choiceInfo = prop (Proxy :: _ "choiceInfo")

_roles :: Lens' MetadataHintInfo (Set S.TokenName)
_roles = prop (Proxy :: _ "roles")

_timeParameters :: Lens' MetadataHintInfo (OSet String)
_timeParameters = prop (Proxy :: _ "timeParameters")

_valueParameters :: Lens' MetadataHintInfo (OSet String)
_valueParameters = prop (Proxy :: _ "valueParameters")

_choiceNames :: Lens' MetadataHintInfo (Set String)
_choiceNames = prop (Proxy :: _ "choiceNames")

