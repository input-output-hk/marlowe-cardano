{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Spec.Marlowe.ACTUS.TestFramework
  where

import           Control.Applicative                               ((<|>))
import           Control.Monad                                     (mzero)
import           Data.Aeson
import           Data.ByteString.Lazy.UTF8                         as BLU (fromString)
import           Data.Char                                         (toUpper)
import           Data.List                                         as L (find)
import           Data.Map                                          as Map (Map, lookup, toList)
import           Data.Maybe                                        (fromJust, fromMaybe)
import           Data.Time                                         (LocalTime (..), defaultTimeLocale, parseTimeM)
import           GHC.Generics                                      (Generic)
import           GHC.Records                                       (getField)
import           Language.Marlowe.ACTUS.Analysis
import           Language.Marlowe.ACTUS.Definitions.BusinessEvents
import           Language.Marlowe.ACTUS.Definitions.ContractTerms  hiding (Assertion)
import           Language.Marlowe.ACTUS.Definitions.Schedule
import           Language.Marlowe.ACTUS.Model.Utility.DateShift    (getFollowingBusinessDay)
import           Test.Tasty
import           Test.Tasty.HUnit                                  (Assertion, assertBool, assertFailure, testCase)

tests :: String -> [TestCase] -> TestTree
tests n t = testGroup n $ [ testCase (getField @"identifier" tc) (runTest tc) | tc <- t]

runTest :: TestCase -> Assertion
runTest TestCase {..} =
  let (TC testcase) = terms
      contract = setDefaultContractTermValues testcase

      getRiskFactors ev date =
        let riskFactors =
              RiskFactorsPoly
                { o_rf_CURS = 1.0,
                  o_rf_RRMO = 1.0,
                  o_rf_SCMO = 1.0,
                  pp_payoff = 0.0
                }

            observedKey RR = marketObjectCodeOfRateReset contract
            observedKey SC = marketObjectCodeOfScalingIndex contract
            observedKey DV = Just (fmap toUpper identifier ++ "_DV")
            observedKey XD = Just . marketObjectCode . Prelude.head $ contractStructure contract
            observedKey _  = settlementCurrency contract

            value = fromMaybe 1.0 $ do
              k <- observedKey ev
              DataObserved {values = values} <- Map.lookup k dataObserved
              ValueObserved {value = valueObserved} <- L.find (\ValueObserved {timestamp = timestamp} ->
                getFollowingBusinessDay timestamp (fromJust . calendar $ scheduleConfig contract) == date) values
              return valueObserved
         in case ev of
              RR -> riskFactors {o_rf_RRMO = value}
              SC -> riskFactors {o_rf_SCMO = value}
              DV -> riskFactors {pp_payoff = value}
              XD -> riskFactors {pp_payoff = value}
              _  -> riskFactors {o_rf_CURS = value}

      cashFlows = genProjectedCashflows getRiskFactors contract
      cashFlowsTo = maybe cashFlows (\d -> filter (\cf -> cashCalculationDay cf <= d) cashFlows) (parseDate to)
   in assertTestResults cashFlowsTo results identifier

testCasesFromFile :: [String] -> FilePath -> IO [TestCase]
testCasesFromFile excludedTestCases fileName = do
  tcs <- readFile fileName
  case let tc = fromString tcs in eitherDecode tc :: Either String (Map String TestCase) of
    (Right decodedTests) ->
      return $
        filter (\TestCase {..} -> notElem identifier excludedTestCases) $
          fmap snd (Map.toList decodedTests)
    Left e -> assertFailure ("Cannot parse test specification from file: " ++ fileName ++ "\nError: " ++ e) >> return []

assertTestResults :: [CashFlow] -> [TestResult] -> String -> IO ()
assertTestResults [] [] _ = return ()
assertTestResults (cashFlow : restCash) (testResult : restTest) identifier' = do
  assertTestResult cashFlow testResult identifier'
  assertTestResults restCash restTest identifier'
assertTestResults _ _ _ = assertFailure "Sizes differ"

assertTestResult :: CashFlow -> TestResult -> String -> IO ()
assertTestResult
  CashFlow {cashPaymentDay = date, cashEvent = event, amount = payoff'}
  testResult@TestResult {eventDate = testDate, eventType = testEvent, payoff = testPayoff}
  identifier' = do

    assertBool
      ("[" ++ show identifier' ++ "] Generated event and test event types should be the same: actual " ++ show event ++ ", expected for " ++ show testResult)
      $ event == (read testEvent :: EventType)

    assertBool
      ("Generated date and test date should be the same: actual " ++ show date ++ ", expected for " ++ show testResult ++ " in " ++ identifier')
      $ date == (fromJust $ parseDate testDate)

    assertBool
      ("[" ++ show identifier' ++ "]  Generated payoff and test payoff should be the same: actual " ++ show payoff' ++ ", expected for " ++ show testResult)
      $ (realToFrac payoff' :: Float) == (realToFrac testPayoff :: Float)

parseDate :: String -> Maybe LocalTime
parseDate date =
  let format
        | length date == 19 = "%Y-%-m-%-dT%T"
        | otherwise = "%Y-%-m-%-dT%H:%M"
   in parseTimeM True defaultTimeLocale format date :: Maybe LocalTime

data DataObserved = DataObserved
  { identifier :: String
  , values     :: [ValueObserved]
  }
  deriving stock (Show, Generic)

instance FromJSON DataObserved where
  parseJSON (Object v) =
    DataObserved
      <$> v .: "identifier"
      <*> v .: "data"
  parseJSON _ = mzero

data ValueObserved = ValueObserved
  { timestamp :: LocalTime
  , value     :: Double
  }
  deriving stock (Show, Generic)

instance FromJSON ValueObserved where
  parseJSON (Object v) =
    ValueObserved
      <$> v .: "timestamp"
      <*> (v .: "value" <|> (read <$> v.: "value"))
  parseJSON _ = mzero

data TestResult = TestResult
  { eventDate           :: String,
    eventType           :: String,
    payoff              :: Double,
    currency            :: String,
    notionalPrincipal   :: Double,
    exerciseAmount      :: Maybe Double,
    nominalInterestRate :: Maybe Double,
    accruedInterest     :: Maybe Double
  }
  deriving stock (Show, Generic)

-- types are inconsistent in json files for NAM and ANN
-- test cases in https://github.com/actusfrf/actus-tests/tree/master/tests
instance FromJSON TestResult where
  parseJSON (Object v) =
    TestResult
      <$> v .: "eventDate"
      <*> v .: "eventType"
      <*> (v .: "payoff" <|> (read <$> v .: "payoff"))
      <*> v .: "currency"
      <*> (v .: "notionalPrincipal" <|> (read <$> v.: "notionalPrincipal"))
      <*> v .:? "exerciseAmount"
      <*> (v .:? "nominalInterestRate" <|> (fmap read <$> v.:? "nominalInterestRate"))
      <*> (v .:? "accruedInterest" <|> (fmap read <$> v.:? "accruedInterest"))
  parseJSON _ = mzero

data TestCase = TestCase
  { identifier     :: String,
    terms          :: ContractTermsTC Double LocalTime,
    to             :: String,
    dataObserved   :: Map String DataObserved,
    eventsObserved :: Value,
    results        :: [TestResult]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

newtype ContractTermsTC a b = TC (ContractTermsPoly a b) deriving (Show, Generic)

instance (Read a, Read b, FromJSON a, FromJSON b) => FromJSON (ContractTermsTC a b) where
  parseJSON (Object v) =
    let sc = ScheduleConfig <$> v .:? "calendar" <*> v .:? "endOfMonthConvention" <*> v .:? "businessDayConvention"
        ct = ContractTermsPoly
                <$> v .: "contractID"
                <*> v .: "contractType"
                <*> (v .: "contractStructure" <|> return [])
                <*> v .: "contractRole"
                <*> v .:? "settlementCurrency"
                <*> v .:? "initialExchangeDate"
                <*> v .:? "dayCountConvention"
                <*> sc
                <*> v .: "statusDate"
                <*> v .:? "contractPerformance"
                <*> v .:? "cycleOfFee"
                <*> v .:? "cycleAnchorDateOfFee"
                <*> v .:? "feeAccrued"
                <*> v .:? "feeBasis"
                <*> v .:? "feeRate"
                <*> v .:? "cycleAnchorDateOfInterestPayment"
                <*> v .:? "cycleOfInterestPayment"
                <*> (v .:? "accruedInterest" <|> (fmap read <$> v .:? "accruedInterest"))
                <*> v .:? "capitalizationEndDate"
                <*> v .:? "cycleAnchorDateOfInterestCalculationBase"
                <*> v .:? "cycleOfInterestCalculationBase"
                <*> v .:? "interestCalculationBase"
                <*> (v .:? "interestCalculationBaseAmount" <|> (fmap read <$> v .:? "interestCalculationBaseAmount"))
                <*> (v .:? "nominalInterestRate" <|> (fmap read <$> v .:? "nominalInterestRate"))
                <*> (v .:? "interestScalingMultiplier" <|> (fmap read <$> v .:? "interestScalingMultiplier"))
                <*> v .:? "maturityDate"
                <*> v .:? "amortizationDate"
                <*> v .:? "exerciseDate"
                <*> (v .:? "notionalPrincipal" <|> (fmap read <$> v .:? "notionalPrincipal"))
                <*> (v .:? "premiumDiscountAtIED" <|> (fmap read <$> v .:? "premiumDiscountAtIED"))
                <*> v .:? "cycleAnchorDateOfPrincipalRedemption"
                <*> v .:? "cycleOfPrincipalRedemption"
                <*> (v .:? "nextPrincipalRedemptionPayment" <|> (fmap read <$> v .:? "nextPrincipalRedemptionPayment"))
                <*> v .:? "purchaseDate"
                <*> (v .:? "priceAtPurchaseDate" <|> (fmap read <$> v .:? "priceAtPurchaseDate"))
                <*> v .:? "terminationDate"
                <*> (v .:? "priceAtTerminationDate" <|> (fmap read <$> v .:? "priceAtTerminationDate"))
                <*> v .:? "scalingIndexAtStatusDate"
                <*> v .:? "cycleAnchorDateOfScalingIndex"
                <*> v .:? "cycleOfScalingIndex"
                <*> v .:? "scalingEffect"
                <*> (v .:? "scalingIndexAtContractDealDate" <|> (fmap read <$> v .:? "scalingIndexAtContractDealDate"))
                <*> v .:? "marketObjectCodeOfScalingIndex"
                <*> (v .:? "notionalScalingMultiplier" <|> (fmap read <$> v .:? "notionalScalingMultiplier"))
                <*> v .:? "cycleOfOptionality"
                <*> v .:? "cycleAnchorDateOfOptionality"
                <*> v .:? "optionType"
                <*> (v .:? "optionStrike1" <|> (fmap read <$> v .:? "optionStrike1"))
                <*> v .:? "optionExerciseType"
                <*> v .:? "settlementPeriod"
                <*> v .:? "deliverySettlement"
                <*> (v .:? "exerciseAmount" <|> (fmap read <$> v .:? "exerciseAmount"))
                <*> (v .:? "futuresPrice" <|> (fmap read <$> v .:? "futuresPrice"))
                <*> v .:? "penaltyRate"
                <*> v .:? "penaltyType"
                <*> v .:? "prepaymentEffect"
                <*> v .:? "cycleOfRateReset"
                <*> v .:? "cycleAnchorDateOfRateReset"
                <*> (v .:? "nextResetRate" <|> (fmap read <$> v.:? "nextResetRate"))
                <*> (v .:? "rateSpread" <|> (fmap read <$> v.:? "rateSpread"))
                <*> (v .:? "rateMultiplier" <|> (fmap read <$> v.:? "rateMultiplier"))
                <*> v .:? "periodFloor"
                <*> v .:? "periodCap"
                <*> v .:? "lifeCap"
                <*> v .:? "lifeFloor"
                <*> v .:? "marketObjectCodeOfRateReset"
                <*> v .:? "cycleOfDividendPayment"
                <*> v .:? "cycleAnchorDateOfDividendPayment"
                <*> v .:? "nextDividendPaymentAmount"
                <*> (fromMaybe False <$> (v .:? "enableSettlement"))
                <*> v .:? "constraints"
                <*> (fromMaybe 0 <$> (v .:? "collateralAmount"))
    in TC <$> ct
  parseJSON _ = mzero

