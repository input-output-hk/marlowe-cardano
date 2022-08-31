module Language.Marlowe.Extended.V1.Metadata where

import Prologue

import Data.Map (Map)
import Data.Map as Map
import Data.Map.Ordered.OMap (OMap)
import Data.Map.Ordered.OMap as OMap
import Data.Maybe (fromMaybe, maybe)
import Data.Set as Set
import Data.Set.Ordered.OSet as OSet
import Language.Marlowe.Core.V1.Semantics.Types as S
import Language.Marlowe.Extended.V1 (Contract, getChoiceNames)
import Language.Marlowe.Extended.V1.Metadata.Types
  ( ChoiceInfo
  , ContractType(..)
  , MetaData
  , MetadataHintInfo
  , NumberFormat(..)
  , NumberFormatType(..)
  , ValueParameterInfo
  )
import Marlowe.HasParties (getParties)
import Marlowe.Template (Placeholders(..), getPlaceholderIds)

contractTypeInitials :: ContractType -> String
contractTypeInitials Escrow = "ES"

contractTypeInitials EscrowWithCollateral = "EC"

contractTypeInitials ZeroCouponBond = "ZC"

contractTypeInitials CouponBondGuaranteed = "CB"

contractTypeInitials Swap = "S"

contractTypeInitials ContractForDifferences = "CD"

contractTypeInitials Other = "O"

contractTypeName :: ContractType -> String
contractTypeName Escrow = "Escrow"

contractTypeName EscrowWithCollateral = "Escrow with Collateral"

contractTypeName ZeroCouponBond = "Zero Coupon Bond"

contractTypeName CouponBondGuaranteed = "Coupon Bond Guaranteed"

contractTypeName Swap = "Swap"

contractTypeName ContractForDifferences = "Contract for Differences"

contractTypeName Other = "Other"

initialsToContractType :: String -> ContractType
initialsToContractType "ES" = Escrow

initialsToContractType "EC" = EscrowWithCollateral

initialsToContractType "ZC" = ZeroCouponBond

initialsToContractType "CB" = CouponBondGuaranteed

initialsToContractType "S" = Swap

initialsToContractType "CD" = ContractForDifferences

initialsToContractType _ = Other

contractTypeArray :: Array ContractType
contractTypeArray =
  [ Escrow
  , EscrowWithCollateral
  , ZeroCouponBond
  , CouponBondGuaranteed
  , Swap
  , ContractForDifferences
  , Other
  ]

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

emptyContractMetadata :: MetaData
emptyContractMetadata =
  { contractType: Other
  , contractName: "Unknown"
  , contractShortDescription: "Unknown"
  , contractLongDescription: "We couldn't find information about this contract"
  , roleDescriptions: Map.empty
  , timeParameterDescriptions: mempty
  , valueParameterInfo: mempty
  , choiceInfo: Map.empty
  }

getChoiceFormat :: MetaData -> String -> NumberFormat
getChoiceFormat { choiceInfo } choiceName =
  maybe DefaultFormat (\choiceInfoVal -> choiceInfoVal.choiceFormat) $
    Map.lookup choiceName choiceInfo

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
    , timeParameters: OSet.fromFoldable (placeholders.timeoutPlaceholderIds)
    , valueParameters: OSet.fromFoldable (placeholders.valuePlaceholderIds)
    , choiceNames: getChoiceNames contract
    }

getHintsFromMetadata :: MetaData -> MetadataHintInfo
getHintsFromMetadata
  { roleDescriptions
  , timeParameterDescriptions
  , valueParameterInfo
  , choiceInfo
  } =
  { roles: Map.keys roleDescriptions
  , timeParameters: OMap.keys timeParameterDescriptions
  , valueParameters: OMap.keys valueParameterInfo
  , choiceNames: Map.keys choiceInfo
  }

