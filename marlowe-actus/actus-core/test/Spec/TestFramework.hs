{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Spec.TestFramework
  where

import Actus.Core
import Actus.Domain hiding (Assertion)
import Actus.Utility (applyBDCWithCfg, (<+>))
import Control.Applicative ((<|>))
import Control.Monad (join, mzero)
import Data.Aeson
import Data.ByteString.Lazy as B (readFile)
import Data.Char (toUpper)
import Data.List as L (find)
import Data.Map as Map (Map, elems, lookup)
import Data.Maybe (isJust)
import Data.Time (LocalTime (..))
import GHC.Generics (Generic)
import GHC.Records (getField)
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)
import Text.Printf (printf)

type TestCashFlow = CashFlow Double
type TestContractState = ContractState Double
type TestContractTerms = ContractTerms Double

tests :: String -> [TestCase] -> TestTree
tests n t =
  testGroup
    n
    [testCase (getField @"identifier" tc) (runTest tc {terms = setDefaultContractTermValues (terms tc)}) | tc <- t]

runTest :: TestCase -> Assertion
runTest tc@TestCase {..} =
  let unscheduleEvents = join $ map (unSchedEv terms) eventsObserved
      cashFlows = genProjectedCashflows riskFactors terms unscheduleEvents
      latestCashFlow = cashPaymentDay
                        <$> (find (\cf -> cashEvent cf == STD) cashFlows
                        <|> find (\cf -> contractType terms /= SWAPS && cashEvent cf == MD) cashFlows)
      cashFlowsTo =
        maybe
          cashFlows
          (\d -> filter ((<= d) . cashPaymentDay) cashFlows)
          (min <$> latestCashFlow <*> to <|> latestCashFlow <|> to)
   in assertTestResults cashFlowsTo results
  where
    riskFactors i ev date =
      case getValue i tc ev date of
        Just v -> case ev of
          RR -> defaultRiskFactors {o_rf_RRMO = v}
          SC -> defaultRiskFactors {o_rf_SCMO = v}
          DV -> defaultRiskFactors {dv_payoff = v}
          XD -> defaultRiskFactors {xd_payoff = v}
          _  -> defaultRiskFactors {o_rf_CURS = v}
        Nothing -> defaultRiskFactors

getValue :: String -> TestCase -> EventType -> LocalTime -> Maybe Double
getValue i TestCase {..} ev date =
  do
    key <- observedKey ev
    DataObserved {values} <- Map.lookup key dataObserved
    ValueObserved {value} <-
      L.find
        ( \ValueObserved {timestamp} ->
            let d = applyBDCWithCfg (scheduleConfig terms) timestamp
             in calculationDay d == date
        )
        values
    return value
  where
    observedKey RR | Actus.Domain.contractId terms == i = marketObjectCodeOfRateReset terms
    observedKey RR =
      do a <- mapM termsFromStructure $ contractStructure terms
         head <$> (mapM marketObjectCodeOfRateReset $ filter (\b -> Actus.Domain.contractId b == i) a)
    observedKey SC = marketObjectCodeOfScalingIndex terms
    observedKey DV = Just (fmap toUpper identifier ++ "_DV")
    observedKey XD = let l = map (getMarketObjectCode . reference) (contractStructure terms) in head $ filter isJust l
    observedKey _  = settlementCurrency terms

termsFromStructure :: ContractStructure a -> Maybe (ContractTerms a)
termsFromStructure cs = case reference cs of
  ReferenceTerms rt -> Just rt
  ReferenceId _     -> Nothing

-- |Unscheduled events from test cases
unSchedEv :: TestContractTerms -> EventObserved -> [(String, EventType, ShiftedDay)]
unSchedEv
  ContractTerms
    { contractType,
      contractStructure,
      creditEventTypeCovered = Just CETC_DF,
      maturityDate = Just mat,
      settlementPeriod,
      scheduleConfig
    }
  EventObserved
    { eventType = CE,
      contractId = Just cid,
      time,
      states = Just PRF_DF
    }
    | contractType `elem` [CEG, CEC]
        && (time <= mat)
        && Just cid `elem` map (getContractIdentifier . reference) contractStructure =
      [(cid, XD, applyBDCWithCfg scheduleConfig time), (cid, STD, applyBDCWithCfg scheduleConfig $ applySettlementPeriod settlementPeriod time)]
unSchedEv
  ContractTerms
    { contractType,
      contractStructure,
      creditEventTypeCovered = Just CETC_DF,
      scheduleConfig,
      settlementPeriod,
      maturityDate = Nothing
    }
  EventObserved
    { eventType = CE,
      contractId = Just cid,
      time,
      states = Just PRF_DF
    }
    | contractType `elem` [CEG, CEC]
        && Just cid `elem` map (getContractIdentifier . reference) contractStructure =
      case mapM f (filter (\cs -> referenceRole cs == COVE) contractStructure) of
        Just m ->
          if time < maximum m
            then [(cid, XD, mkShiftedDay time), (cid, STD, applyBDCWithCfg scheduleConfig $ applySettlementPeriod settlementPeriod time)]
            else []
        Nothing -> []
    where
      f cs = case reference cs of
        ReferenceTerms rt -> maturityDate rt
        ReferenceId _     -> undefined
unSchedEv
  ContractTerms
    { contractType,
      contractStructure,
      creditEventTypeCovered = Just CETC_DF
    }
  EventObserved
    { eventType = CE,
      contractId,
      states = Just PRF_DF
    }
    | contractType `elem` [CEG, CEC]
        && contractId `elem` map (getContractIdentifier . reference) contractStructure =
      []
unSchedEv
  ContractTerms
    { contractType = CSH,
      contractId
    }
  EventObserved
    { eventType = AD,
      time
    } = [(contractId, AD, ShiftedDay time time)]
unSchedEv _ _ = []

applySettlementPeriod :: Maybe Cycle -> LocalTime -> LocalTime
applySettlementPeriod (Just c) s = s <+> c
applySettlementPeriod Nothing s  = s

defaultRiskFactors :: RiskFactors Double
defaultRiskFactors =
  RiskFactors
    { o_rf_CURS = 1.0,
      o_rf_RRMO = 1.0,
      o_rf_SCMO = 1.0,
      pp_payoff = 0.0,
      xd_payoff = 0.0,
      dv_payoff = 0.0
    }

assertTestResults :: [TestCashFlow] -> [TestResult] -> IO ()
assertTestResults [] []               = return ()
assertTestResults (cf : cfs) (r : rs) = assertTestResult cf r >> assertTestResults cfs rs
assertTestResults _ _                 = assertFailure "Sizes differ"

assertTestResult :: TestCashFlow -> TestResult -> IO ()
assertTestResult CashFlow {..} TestResult {eventDate, eventType, payoff} = do
  assertEqual cashEvent eventType
  assertEqual cashPaymentDay eventDate
  assertEqual (realToFrac amount :: Float) (realToFrac payoff :: Float)
  where
    assertEqual a b = assertBool (err a b) $ a == b
    err a b = printf "Mismatch: actual %s, expected %s" (show a) (show b)

testCasesFromFile :: [String] -> FilePath -> IO [TestCase]
testCasesFromFile excluded testfile =
  load testfile
    >>= either
      msg
      ( return
          . filter (\TestCase {..} -> notElem identifier excluded)
          . elems
      )
  where
    load :: FilePath -> IO (Either String (Map String TestCase))
    load f = eitherDecode <$> B.readFile f
    msg err = putStr ("Cannot parse test specification from file: " ++ testfile ++ "\nError: " ++ err) >> return []

data DataObserved = DataObserved
  { identifier :: String
  , values     :: [ValueObserved]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

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
  deriving anyclass (ToJSON)

instance FromJSON ValueObserved where
  parseJSON (Object v) =
    ValueObserved
      <$> v .: "timestamp"
      <*> (v .: "value" <|> (read <$> v.: "value"))
  parseJSON _ = mzero

data EventObserved = EventObserved
  { time       :: LocalTime
  , eventType  :: EventType
  , value      :: Double
  , contractId :: Maybe String
  , states     :: Maybe PRF
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON EventObserved where
  parseJSON (Object v) =
    EventObserved
      <$> v .: "time"
      <*> v .: "type"
      <*> v .: "value"
      <*> v .:? "contractId"
      <*> (v .:? "states" >>= obj)
    where
      obj Nothing           = pure Nothing
      obj (Just (Object o)) = o .: "contractPerformance"
      obj _                 = fail "Error parsing states"
  parseJSON _ = mzero

data TestResult = TestResult
  { eventDate           :: LocalTime,
    eventType           :: EventType,
    payoff              :: Double,
    currency            :: String,
    notionalPrincipal   :: Double,
    exerciseAmount      :: Maybe Double,
    nominalInterestRate :: Maybe Double,
    accruedInterest     :: Maybe Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

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
    terms          :: TestContractTerms,
    to             :: Maybe LocalTime,
    dataObserved   :: Map String DataObserved,
    eventsObserved :: [EventObserved],
    results        :: [TestResult]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON TestCase where
  parseJSON (Object v) =
    TestCase
      <$> v .: "identifier"
      <*> v .: "terms"
      <*> (v .:? "to" <|> return Nothing)
      <*> v .: "dataObserved"
      <*> v .: "eventsObserved"
      <*> v .: "results"
  parseJSON _ = mzero
