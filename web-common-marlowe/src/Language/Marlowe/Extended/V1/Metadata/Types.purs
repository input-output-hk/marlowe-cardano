module Language.Marlowe.Extended.V1.Metadata.Types where

import Prologue

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Aeson as E
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
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
import Language.Marlowe.Core.V1.Semantics.Types as S

data ContractType
  = Escrow
  | EscrowWithCollateral
  | ZeroCouponBond
  | CouponBondGuaranteed
  | Swap
  | ContractForDifferences
  | Other

derive instance genericContractType :: Generic ContractType _

derive instance eqContractType :: Eq ContractType

derive instance ordContractType :: Ord ContractType

instance enumContractType :: Enum ContractType where
  succ = genericSucc
  pred = genericPred

instance boundedContractType :: Bounded ContractType where
  bottom = genericBottom
  top = genericTop

instance showContractType :: Show ContractType where
  show v = genericShow v

instance encodeJsonContractType :: EncodeJson ContractType where
  encodeJson = E.encode E.enum

instance decodeJsonContractType :: DecodeJson ContractType where
  decodeJson = D.decode D.enum

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

data NumberFormatType
  = DefaultFormatType
  | DecimalFormatType
  | TimeFormatType

derive instance eqNumberFormatType :: Eq NumberFormatType

type ValueParameterInfo =
  { valueParameterFormat :: NumberFormat
  , valueParameterDescription :: String
  }

type ChoiceInfo =
  { choiceFormat :: NumberFormat
  , choiceDescription :: String
  }

type MetaData =
  { contractType :: ContractType
  , contractName :: String
  , contractShortDescription :: String
  , contractLongDescription :: String
  , roleDescriptions :: Map S.TokenName String
  -- Order map of time parameters. Key is parameter name, value is parameter description
  , timeParameterDescriptions :: OMap String String
  -- Order map of value parameters. Key is parameter name, value is parameter information
  , valueParameterInfo :: OMap String ValueParameterInfo
  , choiceInfo :: Map S.ChoiceName ChoiceInfo
  }

type MetadataHintInfo =
  { roles :: Set S.TokenName
  , timeParameters :: OSet String
  , valueParameters :: OSet String
  , choiceNames :: Set String
  }
