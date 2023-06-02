{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Extended.V1.Metadata.Types
  where

import Data.Aeson (FromJSON, Value, parseJSON, withArray, withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.Bifunctor (Bifunctor(first))
import qualified Data.Foldable as F
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Ordered (OMap)
import qualified Data.Map.Ordered as OMap
import Data.Set (Set)
import Data.Set.Ordered (OSet)
import Data.Text.Encoding as Text
import qualified GHC.Generics
import Plutus.V1.Ledger.Api (TokenName)
import qualified Plutus.V1.Ledger.Value as Val

data ContractType
  = Escrow
  | EscrowWithCollateral
  | ZeroCouponBond
  | CouponBondGuaranteed
  | Swap
  | ContractForDifferences
  | Other
    deriving stock (Show,Eq,GHC.Generics.Generic)
    deriving anyclass (FromJSON)


data NumberFormat
  = DefaultFormat
  | DecimalFormat Int String
  | TimeFormat
   deriving stock (Show,Eq,GHC.Generics.Generic)
   deriving anyclass (FromJSON)


data NumberFormatType
  = DefaultFormatType
  | DecimalFormatType
  | TimeFormatType
  deriving stock (Show, Eq, GHC.Generics.Generic)
  deriving anyclass (FromJSON)

data ValueParameterInfo = ValueParameterInfo
  { valueParameterFormat      :: NumberFormat
  , valueParameterDescription :: String
  }
  deriving stock (Show, Eq, GHC.Generics.Generic)
  deriving anyclass (FromJSON)


data ChoiceInfo = ChoiceInfo
  { choiceFormat      :: NumberFormat
  , choiceDescription :: String
  }
  deriving stock (Show, Eq, GHC.Generics.Generic)
  deriving anyclass (FromJSON)

type ChoiceName = String

data MetaData = MetaData
  { contractType              :: ContractType
  , contractName              :: String
  , contractShortDescription  :: String
  , contractLongDescription   :: String
  , roleDescriptions          :: Map TokenName String
  -- Order map of time parameters. Key is parameter name, value is parameter description
  , timeParameterDescriptions :: OMap String String
  -- Order map of value parameters. Key is parameter name, value is parameter information
  , valueParameterInfo        :: OMap String ValueParameterInfo
  , choiceInfo                :: Map ChoiceName ChoiceInfo
  }
  deriving stock (Show, Eq, GHC.Generics.Generic)


instance FromJSON MetaData where
    parseJSON =
        withObject "Module" (\v -> do
            cType <- v .: "contractType"
            cName <- v .: "contractName"
            cShortDescription <- v .: "contractShortDescription"
            cLongDescription <- v .: "contractLongDescription"
            roleDesc <- (v .: "roleDescriptions")
                >>= withArray "Role descriptions"
                        (\pl -> mapM parseRoleDesc (F.toList pl)
                        )
            timeParametersDesc <-
                (v .: "timeParameterDescriptions")
                    >>= withArray "Time parameters"
                        (\pl -> mapM parseJSON (F.toList pl)
                        )
            valParameterInfo <-
                (v .: "valueParameterInfo")  >>= withArray "Value parameters"
                        (\pl -> mapM parseJSON (F.toList pl)
                        )
            chParameterInfo <-
                (v .: "choiceInfo")  >>= withArray "choice info"
                        (\pl -> mapM parseJSON (F.toList pl)
                        )

            return $ MetaData
                { contractType = cType
                , contractName = cName
                , contractShortDescription = cShortDescription
                , contractLongDescription = cLongDescription
                , roleDescriptions = Map.fromList roleDesc
                , timeParameterDescriptions = OMap.fromList timeParametersDesc
                , valueParameterInfo = OMap.fromList valParameterInfo
                , choiceInfo = Map.fromList chParameterInfo
                }
        )
        where
        parseRoleDesc :: Value -> Parser (TokenName, String)
        parseRoleDesc val = first (Val.tokenName . Text.encodeUtf8 ) <$> parseJSON val

data MetadataHintInfo = MetadataHintInfo
  { roles           :: Set TokenName
  , timeParameters  :: OSet String
  , valueParameters :: OSet String
  , choiceNames     :: Set String
  }
  deriving stock (Show,Eq)
