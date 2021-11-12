{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Spec.Marlowe.ACTUS.ContractTerms (actusJsonToContractTerms) where

import Control.Applicative ((<|>))
import Control.Monad (guard, mzero)
import Data.Aeson (GFromJSON, Zero, withArray, withObject, (.:), (.:?))
import Data.Aeson.Types (Options (..), Parser, Value (..), defaultOptions, explicitParseField, explicitParseFieldMaybe,
                         genericParseJSON)
import Data.Maybe (fromMaybe)
import Data.Text as T hiding (reverse, takeWhile)
import Data.Text.Read as T
import qualified Data.Vector as Vector
import GHC.Generics (Generic (Rep))
import Language.Marlowe.ACTUS.Domain.ContractTerms

jsonStringToEnumWithoutPrefix :: (Generic a, GFromJSON Zero (Rep a )) => Value -> Parser a
jsonStringToEnumWithoutPrefix =
  genericParseJSON defaultOptions
    { constructorTagModifier = reverse . takeWhile (/= '_') . reverse }

actusJsonToCr :: Value -> Parser CR
actusJsonToCr = jsonStringToEnumWithoutPrefix

actusJsonToDcc :: Value -> Parser DCC
actusJsonToDcc (String "AA")         = pure DCC_A_AISDA
actusJsonToDcc (String "A360")       = pure DCC_A_360
actusJsonToDcc (String "A365")       = pure DCC_A_365
actusJsonToDcc (String "30E360ISDA") = pure DCC_E30_360ISDA
actusJsonToDcc (String "30E360")     = pure DCC_E30_360
actusJsonToDcc (String "B252")       = pure DCC_B_252
actusJsonToDcc _                     = mzero

actusJsonToEOMC :: Value -> Parser EOMC
actusJsonToEOMC = jsonStringToEnumWithoutPrefix

actusJsonToBDC :: Value -> Parser BDC
actusJsonToBDC = jsonStringToEnumWithoutPrefix

actusJsonToCalendar :: Value -> Parser Calendar
actusJsonToCalendar = jsonStringToEnumWithoutPrefix

actusJsonToScheduleConfig :: Value -> Parser ScheduleConfig
actusJsonToScheduleConfig = withObject "ScheduleConfig" \v ->
  ScheduleConfig
    <$> explicitParseFieldMaybe actusJsonToCalendar v "calendar"
    <*> explicitParseFieldMaybe actusJsonToEOMC v "endOfMonthConvention"
    <*> explicitParseFieldMaybe actusJsonToBDC v "businessDayConvention"

actusJsonToPRF :: Value -> Parser PRF
actusJsonToPRF = jsonStringToEnumWithoutPrefix

actusJsonToFEB :: Value -> Parser FEB
actusJsonToFEB = jsonStringToEnumWithoutPrefix

actusJsonToIPCB :: Value -> Parser IPCB
actusJsonToIPCB = jsonStringToEnumWithoutPrefix

actusJsonToSCEF :: Value -> Parser SCEF
actusJsonToSCEF = jsonStringToEnumWithoutPrefix

actusJsonToPYTP :: Value -> Parser PYTP
actusJsonToPYTP = jsonStringToEnumWithoutPrefix

actusJsonToOPTP :: Value -> Parser OPTP
actusJsonToOPTP = jsonStringToEnumWithoutPrefix

actusJsonToOPXT :: Value -> Parser OPXT
actusJsonToOPXT = jsonStringToEnumWithoutPrefix

actusJsonToDS :: Value -> Parser DS
actusJsonToDS = jsonStringToEnumWithoutPrefix

actusJsonToPPEF :: Value -> Parser PPEF
actusJsonToPPEF = jsonStringToEnumWithoutPrefix

actusJsonToPeriod :: Value -> Parser Period
actusJsonToPeriod = jsonStringToEnumWithoutPrefix

actusJsonToStub :: Value -> Parser Stub
actusJsonToStub (String "0") = pure ShortStub
actusJsonToStub (String "1") = pure LongStub
actusJsonToStub _            = mzero

actusJsonToCycle :: Value -> Parser Cycle
actusJsonToCycle (String s) = fromMaybe mzero (parseCycle s)
  where
    parseCycle :: Text -> Maybe (Parser Cycle)
    parseCycle c = do
      r0 <- unconsConstant 'P' c
      (n, r1) <- hush $ T.decimal r0
      (p, r2) <- uncons r1
      if T.null r2
        then
          Just $
            pure (Cycle n)
              <*> actusJsonToPeriod (String $ singleton p)
              <*> pure LongStub
              <*> pure False
        else do
          r3 <- unconsConstant 'L' r2
          Just $
            pure (Cycle n)
              <*> actusJsonToPeriod (String $ singleton p)
              <*> actusJsonToStub (String r3)
              <*> pure False

    unconsConstant :: Char -> Text -> Maybe Text
    unconsConstant c t = do (ht, tt) <- uncons t
                            guard (ht == c)
                            pure tt

    hush :: Either a b -> Maybe b
    hush = either (const Nothing) Just
actusJsonToCycle _ = mzero

actusJsonToContractStructure :: Value -> Parser ContractStructure
actusJsonToContractStructure = withObject "ContractStructure" \v ->
  ContractStructure
    <$> (v .: "object" >>= withObject "marketObjectCode" \o -> o .: "marketObjectCode")
    <*> v .: "referenceType"
    <*> v .: "referenceRole"

actusJsonToContractTerms :: Value -> Parser ContractTerms
actusJsonToContractTerms = withObject "ContractTerms" \v ->
  ContractTermsPoly
    <$> (v .: "contractID" <|> v .: "contractId")
    <*> v .: "contractType"
    <*> (explicitParseField (array actusJsonToContractStructure) v "contractStructure" <|> pure [])
    <*> explicitParseField actusJsonToCr v "contractRole"
    <*> v .:? "settlementCurrency"
    <*> v .:? "initialExchangeDate"
    <*> explicitParseFieldMaybe actusJsonToDcc v "dayCountConvention"
    <*> (explicitParseField actusJsonToScheduleConfig v "scheduleConfig"
          <|> actusJsonToScheduleConfig (Object v)
          )
    <*> v .: "statusDate"
    <*> explicitParseFieldMaybe actusJsonToPRF v "contractPerformance"
    <*> explicitParseFieldMaybe actusJsonToCycle v "cycleOfFee"
    <*> v .:? "cycleAnchorDateOfFee"
    <*> v .:? "feeAccrued"
    <*> explicitParseFieldMaybe actusJsonToFEB v "feeBasis"
    <*> v .:? "feeRate"
    <*> v .:? "cycleAnchorDateOfInterestPayment"
    <*> explicitParseFieldMaybe actusJsonToCycle v "cycleOfInterestPayment"
    <*> v .!? "accruedInterest"
    <*> v .:? "capitalizationEndDate"
    <*> v .:? "cycleAnchorDateOfInterestCalculationBase"
    <*> explicitParseFieldMaybe actusJsonToCycle v "cycleOfInterestCalculationBase"
    <*> explicitParseFieldMaybe actusJsonToIPCB v "interestCalculationBase"
    <*> v .!? "interestCalculationBaseAmount"
    <*> v .!? "nominalInterestRate"
    <*> v .!? "interestScalingMultiplier"
    <*> v .:? "maturityDate"
    <*> v .:? "amortizationDate"
    <*> v .:? "exerciseDate"
    <*> v .!? "notionalPrincipal"
    <*> v .!? "premiumDiscountAtIED"
    <*> v .:? "cycleAnchorDateOfPrincipalRedemption"
    <*> explicitParseFieldMaybe actusJsonToCycle v "cycleOfPrincipalRedemption"
    <*> v .!? "nextPrincipalRedemptionPayment"
    <*> v .:? "purchaseDate"
    <*> v .!? "priceAtPurchaseDate"
    <*> v .:? "terminationDate"
    <*> v .!? "priceAtTerminationDate"
    <*> v .:? "scalingIndexAtStatusDate"
    <*> v .:? "cycleAnchorDateOfScalingIndex"
    <*> explicitParseFieldMaybe actusJsonToCycle v "cycleOfScalingIndex"
    <*> explicitParseFieldMaybe actusJsonToSCEF v "scalingEffect"
    <*> v .!? "scalingIndexAtContractDealDate"
    <*> v .:? "marketObjectCodeOfScalingIndex"
    <*> v .!? "notionalScalingMultiplier"
    <*> explicitParseFieldMaybe actusJsonToCycle v "cycleOfOptionality"
    <*> v .:? "cycleAnchorDateOfOptionality"
    <*> explicitParseFieldMaybe actusJsonToOPTP v "optionType"
    <*> v .!? "optionStrike1"
    <*> explicitParseFieldMaybe actusJsonToOPXT v "optionExerciseType"
    <*> explicitParseFieldMaybe actusJsonToCycle v "settlementPeriod"
    <*> explicitParseFieldMaybe actusJsonToDS v "deliverySettlement"
    <*> v .!? "exerciseAmount"
    <*> v .!? "futuresPrice"
    <*> v .:? "penaltyRate"
    <*> explicitParseFieldMaybe actusJsonToPYTP v "penaltyType"
    <*> explicitParseFieldMaybe actusJsonToPPEF v "prepaymentEffect"
    <*> explicitParseFieldMaybe actusJsonToCycle v "cycleOfRateReset"
    <*> v .:? "cycleAnchorDateOfRateReset"
    <*> v .!? "nextResetRate"
    <*> v .!? "rateSpread"
    <*> v .!? "rateMultiplier"
    <*> v .:? "periodFloor"
    <*> v .:? "periodCap"
    <*> v .:? "lifeCap"
    <*> v .:? "lifeFloor"
    <*> v .:? "marketObjectCodeOfRateReset"
    <*> explicitParseFieldMaybe actusJsonToCycle v "cycleOfDividendPayment"
    <*> v .:? "cycleAnchorDateOfDividendPayment"
    <*> v .:? "nextDividendPaymentAmount"
    <*> (fromMaybe False <$> (v .:? "enableSettlement"))
    <*> v .:? "constraints"
    <*> (fromMaybe 0 <$> (v .:? "collateralAmount"))
  where
    (.!?) w s = w .:? s <|> (fmap read <$> w .:? s)
    array parseElement = withArray "List" $ traverse parseElement . Vector.toList
