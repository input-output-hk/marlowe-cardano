module Marlowe.Extended.Metadata where

import Prologue
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Aeson as E
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Ordered.OMap (OMap)
import Data.Map.Ordered.OMap as OMap
import Data.Maybe (fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.Ordered.OSet (OSet)
import Data.Set.Ordered.OSet as OSet
import Data.Show.Generic (genericShow)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Marlowe.Extended (Contract, ContractType(..), getChoiceNames)
import Marlowe.HasParties (getParties)
import Marlowe.Semantics as S
import Marlowe.Template (Placeholders(..), getPlaceholderIds)
import Type.Proxy (Proxy(..))

data NumberFormat
  = DefaultFormat
  | DecimalFormat Int String
  | TimeFormat

derive instance eqNumberFormat :: Eq NumberFormat

derive instance genericNumberFormat :: Generic NumberFormat _

instance encodeJsonNumberFormat :: EncodeJson NumberFormat where
  encodeJson = case _ of
    DefaultFormat -> E.encodeTagged "DefaultFormat" unit E.null
    DecimalFormat a b -> E.encodeTagged "DecimalFormat" (Tuple a b) E.value
    TimeFormat -> E.encodeTagged "TimeFormat" unit E.null

instance decodeJsonNumberFormat :: DecodeJson NumberFormat where
  decodeJson =
    D.decode
      $ D.sumType "NumberFormat"
      $ Map.fromFoldable
          [ "DefaultFormat" /\ D.content (DefaultFormat <$ D.null)
          , "DecimalFormat" /\ D.content (uncurry DecimalFormat <$> D.value)
          , "TimeFormat" /\ D.content (TimeFormat <$ D.null)
          ]

instance showNumberFormat :: Show NumberFormat where
  show = genericShow

integerFormat :: NumberFormat
integerFormat = DecimalFormat 0 ""

lovelaceFormat :: NumberFormat
lovelaceFormat = DecimalFormat 6 "₳"

oracleRatioFormat :: String -> NumberFormat
oracleRatioFormat str = DecimalFormat 8 str

isDefaultFormat :: NumberFormat -> Boolean
isDefaultFormat DefaultFormat = true

isDefaultFormat _ = false

isDecimalFormat :: NumberFormat -> Boolean
isDecimalFormat (DecimalFormat _ _) = true

isDecimalFormat _ = false

data NumberFormatType
  = DefaultFormatType
  | DecimalFormatType
  | TimeFormatType

derive instance eqNumberFormatType :: Eq NumberFormatType

toString :: NumberFormatType -> String
toString DefaultFormatType = "DefaultFormatType"

toString DecimalFormatType = "DecimalFormatType"

toString TimeFormatType = "TimeFormatType"

fromString :: String -> Maybe NumberFormatType
fromString "DefaultFormatType" = Just DefaultFormatType

fromString "DecimalFormatType" = Just DecimalFormatType

fromString _ = Nothing

getFormatType :: NumberFormat -> NumberFormatType
getFormatType DefaultFormat = DefaultFormatType

getFormatType (DecimalFormat _ _) = DecimalFormatType

getFormatType TimeFormat = TimeFormatType

defaultForFormatType :: NumberFormatType -> NumberFormat
defaultForFormatType DefaultFormatType = DefaultFormat

defaultForFormatType DecimalFormatType = DecimalFormat 0 ""

defaultForFormatType TimeFormatType = TimeFormat

type ValueParameterInfo =
  { valueParameterFormat :: NumberFormat
  , valueParameterDescription :: String
  }

_valueParameterFormat :: Lens' ValueParameterInfo NumberFormat
_valueParameterFormat = prop (Proxy :: _ "valueParameterFormat")

_valueParameterDescription :: Lens' ValueParameterInfo String
_valueParameterDescription = prop (Proxy :: _ "valueParameterDescription")

emptyValueParameterInfo :: ValueParameterInfo
emptyValueParameterInfo =
  { valueParameterFormat: DefaultFormat
  , valueParameterDescription: mempty
  }

getValueParameterInfo
  :: String -> OMap String ValueParameterInfo -> ValueParameterInfo
getValueParameterInfo str = fromMaybe emptyValueParameterInfo <<< OMap.lookup
  str

updateValueParameterInfo
  :: (ValueParameterInfo -> ValueParameterInfo)
  -> String
  -> OMap String ValueParameterInfo
  -> OMap String ValueParameterInfo
updateValueParameterInfo f = OMap.alter updateValueParameterInfoEntry
  where
  updateValueParameterInfoEntry
    :: Maybe ValueParameterInfo -> Maybe ValueParameterInfo
  updateValueParameterInfoEntry mValueParameterInfo = Just $ f $ fromMaybe
    emptyValueParameterInfo
    mValueParameterInfo

type ChoiceInfo =
  { choiceFormat :: NumberFormat
  , choiceDescription :: String
  }

_choiceFormat :: Lens' ChoiceInfo NumberFormat
_choiceFormat = prop (Proxy :: _ "choiceFormat")

_choiceDescription :: Lens' ChoiceInfo String
_choiceDescription = prop (Proxy :: _ "choiceDescription")

emptyChoiceInfo :: ChoiceInfo
emptyChoiceInfo =
  { choiceFormat: DefaultFormat
  , choiceDescription: mempty
  }

getChoiceInfo :: String -> Map String ChoiceInfo -> ChoiceInfo
getChoiceInfo str = fromMaybe emptyChoiceInfo <<< Map.lookup str

updateChoiceInfo
  :: (ChoiceInfo -> ChoiceInfo)
  -> String
  -> Map String ChoiceInfo
  -> Map String ChoiceInfo
updateChoiceInfo f = Map.alter updateChoiceInfoEntry
  where
  updateChoiceInfoEntry :: Maybe ChoiceInfo -> Maybe ChoiceInfo
  updateChoiceInfoEntry mChoiceInfo = Just $ f $ fromMaybe emptyChoiceInfo
    mChoiceInfo

type MetaData =
  { contractType :: ContractType
  , contractName :: String
  , contractShortDescription :: String
  , contractLongDescription :: String
  , roleDescriptions :: Map S.TokenName String
  , slotParameterDescriptions :: OMap String String
  , valueParameterInfo :: OMap String ValueParameterInfo
  , choiceInfo :: Map String ChoiceInfo
  }

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

_slotParameterDescriptions :: Lens' MetaData (OMap String String)
_slotParameterDescriptions = prop (Proxy :: _ "slotParameterDescriptions")

_valueParameterInfo :: Lens' MetaData (OMap String ValueParameterInfo)
_valueParameterInfo = prop (Proxy :: _ "valueParameterInfo")

_choiceInfo :: Lens' MetaData (Map String ChoiceInfo)
_choiceInfo = prop (Proxy :: _ "choiceInfo")

emptyContractMetadata :: MetaData
emptyContractMetadata =
  { contractType: Other
  , contractName: ""
  , contractShortDescription: ""
  , contractLongDescription: ""
  , roleDescriptions: Map.empty
  , slotParameterDescriptions: mempty
  , valueParameterInfo: mempty
  , choiceInfo: Map.empty
  }

getChoiceFormat :: MetaData -> String -> NumberFormat
getChoiceFormat { choiceInfo } choiceName =
  maybe DefaultFormat (\choiceInfoVal -> choiceInfoVal.choiceFormat) $
    Map.lookup choiceName choiceInfo

type MetadataHintInfo
  =
  { roles :: Set S.TokenName
  , slotParameters :: OSet String
  , valueParameters :: OSet String
  , choiceNames :: Set String
  }

_roles :: Lens' MetadataHintInfo (Set S.TokenName)
_roles = prop (Proxy :: _ "roles")

_slotParameters :: Lens' MetadataHintInfo (OSet String)
_slotParameters = prop (Proxy :: _ "slotParameters")

_valueParameters :: Lens' MetadataHintInfo (OSet String)
_valueParameters = prop (Proxy :: _ "valueParameters")

_choiceNames :: Lens' MetadataHintInfo (Set String)
_choiceNames = prop (Proxy :: _ "choiceNames")

getMetadataHintInfo :: Contract -> MetadataHintInfo
getMetadataHintInfo contract =
  let
    Placeholders placeholders = getPlaceholderIds contract
  in
    { roles:
        Set.mapMaybe
          ( case _ of
              S.Role name -> Just name
              _ -> Nothing
          )
          $ getParties contract
    , slotParameters: OSet.fromFoldable (placeholders.slotPlaceholderIds)
    , valueParameters: OSet.fromFoldable (placeholders.valuePlaceholderIds)
    , choiceNames: getChoiceNames contract
    }

getHintsFromMetadata :: MetaData -> MetadataHintInfo
getHintsFromMetadata
  { roleDescriptions
  , slotParameterDescriptions
  , valueParameterInfo
  , choiceInfo
  } =
  { roles: Map.keys roleDescriptions
  , slotParameters: OMap.keys slotParameterDescriptions
  , valueParameters: OMap.keys valueParameterInfo
  , choiceNames: Map.keys choiceInfo
  }

type ContractTemplate =
  { metaData :: MetaData
  , extendedContract :: Contract
  }

_metaData :: Lens' ContractTemplate MetaData
_metaData = prop (Proxy :: _ "metaData")

_extendedContract :: Lens' ContractTemplate Contract
_extendedContract = prop (Proxy :: _ "extendedContract")
