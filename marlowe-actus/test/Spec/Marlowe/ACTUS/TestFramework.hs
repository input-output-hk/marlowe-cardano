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

module Spec.Marlowe.ACTUS.TestFramework
  ( tests
  , testCasesFromFile
  )
  where

import           Control.Applicative                           ((<|>))
import           Control.Monad                                 (join, mzero)
import           Control.Monad.Reader                          (Reader, ask, runReader, withReader)
import           Data.Aeson
import           Data.ByteString.Lazy                          as B (readFile)
import           Data.Char                                     (toUpper)
import           Data.List                                     as L (find, unzip4)
import           Data.Map                                      as Map (Map, elems, lookup)
import           Data.Maybe                                    (fromJust, fromMaybe)
import           Data.Sort                                     (sortOn)
import           Data.Time                                     (LocalTime (..))
import           GHC.Generics                                  (Generic)
import           GHC.Records                                   (getField)
import           Language.Marlowe.ACTUS.Domain.BusinessEvents
import           Language.Marlowe.ACTUS.Domain.ContractState
import           Language.Marlowe.ACTUS.Domain.ContractTerms   hiding (Assertion)
import           Language.Marlowe.ACTUS.Domain.Schedule
import           Language.Marlowe.ACTUS.Generator.Analysis
import           Language.Marlowe.ACTUS.Model.ContractSchedule as S (maturity, schedule)
import           Language.Marlowe.ACTUS.Model.Payoff           (CtxPOF (CtxPOF))
import           Language.Marlowe.ACTUS.Model.StateTransition  (CtxSTF (..))
import           Language.Marlowe.ACTUS.Utility.DateShift      (getFollowingBusinessDay)
import           Test.Tasty
import           Test.Tasty.HUnit                              (Assertion, assertBool, assertFailure, testCase)
import           Text.Printf                                   (printf)

tests :: String -> [TestCase] -> TestTree
tests n t =
  testGroup
    n
    [testCase (getField @"identifier" tc) (runTest tc {terms = setDefaultContractTermValues (terms tc)}) | tc <- t]
  where
    runTest :: TestCase -> Assertion
    runTest tc@TestCase {..} =
      let riskFactors ev date =
            let rf =
                  RiskFactorsPoly
                    { o_rf_CURS = 1.0,
                      o_rf_RRMO = 1.0,
                      o_rf_SCMO = 1.0,
                      pp_payoff = 0.0,
                      xd_payoff = 0.0,
                      dv_payoff = 0.0
                    }

                observedKey RR = marketObjectCodeOfRateReset terms
                observedKey SC = marketObjectCodeOfScalingIndex terms
                observedKey DV = Just (fmap toUpper identifier ++ "_DV")
                observedKey XD = Prelude.head $ map (getMarketObjectCode . reference) (contractStructure terms)
                observedKey _  = settlementCurrency terms

                v = fromMaybe 1.0 $ do
                  k <- observedKey ev
                  DataObserved {values} <- Map.lookup k dataObserved
                  ValueObserved {value} <-
                    L.find
                      ( \ValueObserved {timestamp} ->
                          getFollowingBusinessDay timestamp (fromJust . calendar $ scheduleConfig terms) == date
                      )
                      values
                  return value
             in case ev of
                  RR -> rf {o_rf_RRMO = v}
                  SC -> rf {o_rf_SCMO = v}
                  DV -> rf {dv_payoff = v}
                  XD -> rf {xd_payoff = v}
                  _  -> rf {o_rf_CURS = v}

          cashFlows =
            runReader
              (run tc)
              $ CtxSTF
                terms
                (calculationDay <$> schedule FP terms)
                (calculationDay <$> schedule PR terms)
                (calculationDay <$> schedule IP terms)
                (S.maturity terms)
                riskFactors

       in assertTestResults cashFlows results

    assertTestResults :: [CashFlow] -> [TestResult] -> IO ()
    assertTestResults [] []               = return ()
    assertTestResults (cf : cfs) (r : rs) = assertTestResult cf r >> assertTestResults cfs rs
    assertTestResults _ _                 = assertFailure "Sizes differ"

    assertTestResult :: CashFlow -> TestResult -> IO ()
    assertTestResult CashFlowPoly {..} TestResult {eventDate, eventType, payoff} = do
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

run :: TestCase -> Reader (CtxSTF Double) [CashFlow]
run TestCase {..} = do
  ctx <- ask
  pof <- genProjectedPayoffs

  -- scheduled events
  let schedCfs = genCashflow (contractTerms ctx) <$> pof
  let schedCfsTruncated = maybe schedCfs (\d -> filter ((<= d) . cashCalculationDay) schedCfs) to

  let (_, _, st, _) = unzip4 pof

  -- unscheduled events
  unschedStates <- join <$> mapM (unscheduledEvents (contractTerms ctx) st) eventsObserved
  unschedPayoffs <- trans . genPayoffs $ unschedStates

  return $ case unschedPayoffs of
    [] -> schedCfsTruncated
    _ ->
      -- merging together
      let unschedCfs =
            genCashflow (contractTerms ctx)
              <$> zipWith (\(x, y, z) -> (x,y,z,)) unschedStates unschedPayoffs
          merged = sortOn cashCalculationDay $ schedCfsTruncated ++ unschedCfs
          mergedTo = maybe merged (\d -> filter ((<= d) . cashCalculationDay) merged) $
            case map cashCalculationDay (filter f merged) of
              [] -> Nothing
              ts -> Just $ minimum ts
            where
              f CashFlowPoly {cashEvent = MD}  = True
              f CashFlowPoly {cashEvent = STD} = True
              f _                              = False
       in mergedTo
  where
    trans :: Reader (CtxPOF a) b -> Reader (CtxSTF a) b
    trans = withReader (\ctx -> CtxPOF (contractTerms ctx) (riskFactors ctx))

unscheduledEvents ::
  ContractTerms ->
  [ContractState] ->
  EventObserved ->
  Reader (CtxSTF Double) [(EventType, ShiftedDay, ContractStatePoly Double)]
unscheduledEvents
  ContractTermsPoly
    { contractType,
      contractStructure,
      creditEventTypeCovered = Just CETC_DF
    }
  sts
  EventObserved
    { eventType = CE,
      contractId,
      time,
      states = PRF_DF
    }
    | contractType `elem` [CEG, CEC]
        && Just contractId `elem` map (getContractIdentifier . reference) contractStructure =
      let stn = last $ filter (\st -> sd st < time) sts
       in genStates
            [ (XD, ShiftedDay time time),
              (STD, ShiftedDay time time)
            ]
            stn
unscheduledEvents _ _ _ = return []

getMarketObjectCode :: Reference Double -> Maybe String
getMarketObjectCode (ReferenceId i)    = marketObjectCode i
getMarketObjectCode (ReferenceTerms _) = Nothing

getContractIdentifier :: Reference Double -> Maybe String
getContractIdentifier (ReferenceId i)                         = contractIdentifier i
getContractIdentifier (ReferenceTerms ContractTermsPoly {..}) = Just contractId

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
  , contractId :: String
  , states     :: PRF
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON EventObserved where
  parseJSON (Object v) =
    EventObserved
      <$> v .: "time"
      <*> v .: "type"
      <*> v .: "value"
      <*> v .: "contractId"
      <*> (v .: "states" >>= obj)
    where
      obj (Object o) = o .: "contractPerformance"
      obj _          = fail "Error parsing states"
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
    terms          :: ContractTerms,
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
