module Component.MetadataTab.Types where

import Contrib.Data.Unfoldable (Move) as Unfoldable
import Language.Marlowe.Core.V1.Semantics.Types as S
import Language.Marlowe.Extended.V1.Metadata.Types (ContractType, NumberFormat)

class ShowConstructor a where
  showConstructor :: a -> String

data MetadataAction
  = SetContractName String
  | SetContractType ContractType
  | SetContractShortDescription String
  | SetContractLongDescription String
  | SetRoleDescription S.TokenName String
  | DeleteRoleDescription S.TokenName
  | SetTimeParameterDescription String String
  | DeleteTimeParameterDescription String
  | MoveTimeParameterDescription Unfoldable.Move
  | SetValueParameterDescription String String
  | MoveValueParameterDescription Unfoldable.Move
  | SetValueParameterFormat String NumberFormat
  | DeleteValueParameterInfo String
  | SetChoiceDescription String String
  | SetChoiceFormat String NumberFormat
  | DeleteChoiceInfo String

instance metadataActionShowConstructor :: ShowConstructor MetadataAction where
  showConstructor (SetContractName _) = "SetContractName"
  showConstructor (SetContractType _) = "SetContractType"
  showConstructor (SetContractShortDescription _) =
    "SetContractShortDescription"
  showConstructor (SetContractLongDescription _) = "SetContractLongDescription"
  showConstructor (SetRoleDescription _ _) = "SetRoleDescription"
  showConstructor (DeleteRoleDescription _) = "DeleteRoleDescription"
  showConstructor (SetTimeParameterDescription _ _) =
    "SetTimeParameterDescription"
  showConstructor (MoveTimeParameterDescription _) =
    "MoveTimeParameterDescription"
  showConstructor (DeleteTimeParameterDescription _) =
    "DeleteTimeParameterDescription"
  showConstructor (SetValueParameterDescription _ _) =
    "SetValueParameterDescription"
  showConstructor (MoveValueParameterDescription _) =
    "MoveValueParameterDescription"
  showConstructor (SetValueParameterFormat _ _) = "SetValueParameterFormat"
  showConstructor (DeleteValueParameterInfo _) = "DeleteValueParameterInfo"
  showConstructor (SetChoiceDescription _ _) = "SetChoiceDescription"
  showConstructor (SetChoiceFormat _ _) = "SetChoiceFormat"
  showConstructor (DeleteChoiceInfo _) = "DeleteChoiceInfo"
